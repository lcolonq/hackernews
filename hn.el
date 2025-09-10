;;; hn --- Hacker News new | past | comments | ask | show | jobs | submit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;; Library imports
(require 'cl-lib)
(require 'shr)
(require 'f)
(require 'ht)
(require 'request)
(require 'elfeed)

(defconst hn/root
  (if load-file-name
    (f-parent (f-canonical load-file-name))
    default-directory))
(defconst hn/data (or (getenv "HACKERNEWS_DATA_DIR") hn/root))
(message "root: %s" hn/root)
(message "data: %s" hn/data)

(defun hn/youtube-feed (url)
  "Given a YouTube channel page URL, return the feed URL."
  (let ((page nil))
    (request url
      :sync t
      :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
      :complete
      (cl-function
        (lambda (&key data &allow-other-keys)
          (setf page data))))
    (when-let*
      ((link
         (car
           (--filter (s-equals? (dom-attr it 'title) "RSS")
             (dom-by-tag page 'link)))))
      (dom-attr link 'href))))

(defun hn/read-sexp (s)
  "Read string S into a Lisp form.
Return nil on error."
  (condition-case nil (read s) (error nil)))
(setq elfeed-feeds (hn/read-sexp (f-read-text (f-join hn/root "feeds.eld"))))
(message "feeds: %s" elfeed-feeds)
(setq elfeed-db-directory (f-join hn/data "elfeed"))
(defun hn/elfeed-log-wrapper (f &rest args)
  "Advice function wrapping F and ARGS."
  (ignore f)
  (message "elfeed: %s" args))
(advice-add 'elfeed-log :around #'hn/elfeed-log-wrapper)

(defun hn/get-recent-entries ()
  "Return a list of recent feed entries."
  (let ( (counter 0)
         (ret nil))
    (with-elfeed-db-visit (entry _feed)
      (when (> counter 29)
        (elfeed-db-return))
      (cl-incf counter)
      (push entry ret))
    (nreverse ret)))
(defun hn/get-recent-entries-no-duplicates ()
  "Return a list of recent feed entries.
Do not return any duplicates from the same feed."
  (let ( (counter 0)
         (visited (ht-create))
         (ret nil))
    (with-elfeed-db-visit (entry feed)
      (when (> counter 29)
        (elfeed-db-return))
      (unless (ht-get visited feed)
        (ht-set! visited feed t)
        (cl-incf counter)
        (push entry ret)))
    (nreverse ret)))
(defun hn/get-recent-entries-no-duplicates-no-youtube ()
  "Return a list of recent feed entries.
Do not return any duplicates from the same feed.
Do not return any YouTube links"
  (let ( (counter 0)
         (visited (ht-create))
         (ret nil))
    (with-elfeed-db-visit (entry feed)
      (when (> counter 29)
        (elfeed-db-return))
      (let ((url (url-generic-parse-url (elfeed-entry-link entry))))
        (unless (or (ht-get visited feed) (s-contains? "youtube" (url-host url)))
          (ht-set! visited feed t)
          (cl-incf counter)
          (push entry ret))))
    (nreverse ret)))
(defun hn/entry-default-author ()
  "Return a random default author."
  (let ((choices
          '( "\"friend\""
             "mrblue"
             "mrred"
             "mryellow"
             "mrgreen"
             "mrorange"
             "mrpurple")))
    (nth (random (length choices)) choices)))
(defun hn/entry-author (e)
  "Return the author of E based on the tags."
  (format "%s"
    (or
      (car (--filter (not (eq it 'unread)) (elfeed-entry-tags e)))
      (hn/entry-default-author))))
(defun hn/render-entry (e)
  "Convert the Elfeed entry E into DOM."
  (let ((url (url-generic-parse-url (elfeed-entry-link e))))
    `(div ()
       (div ((class . "hn-link-above"))
         (a ((href . ,(elfeed-entry-link e))) ,(elfeed-entry-title e))
         ,@(cond
             ((-contains? '("gemini" "gopher") (url-type url))
               (setf (url-type url) "")
               `((a ( (href .
                        ,(s-concat "https://portal.mozz.us/gemini/"
                           (s-chop-prefix "://" (url-recreate-url url))))
                      (class . "hn-link-proxy"))
                   "(gemini proxy)")))
             (t
               `((a ( (href . ,(s-concat "https://" (url-host url)))
                      (class . "hn-link-proxy"))
                   ,(format "(%s)" (url-host url)))))))
       (div ((class . "hn-link-below"))
         ,(format "%s points by %s %s hours ago | hide | %s comments"
            (+ (random 200) 10)
            (hn/entry-author e)
            (round
              (/
                (time-convert
                  (time-subtract
                    (current-time)
                    (elfeed-entry-date e))
                  'integer)
                3600.0))
            (random 50))))))
(defun hn/render-entries (es)
  "Convert the Elfeed entries ES into DOM."
  `(ol () ,@(--map `(li () ,(hn/render-entry it)) es)))

(defconst hn/server-process "hn-server")
(defconst hn/port 3030)

(defun hn/server-buffer (proc)
  "Return the buffer name for the server buffer associated with client PROC."
  (format " *hin-server-buffer-%s*" proc))

(defun hn/200-content-type (ct body)
  "Build a response string around BODY with content type CT."
  (format "HTTP/1.1 200 OK\r
Content-Type: %s; charset=UTF-8\r
Content-Length: %s\r
\r
%s" ct (string-bytes body) body))

(defun hn/200 (body)
  "Build a response string around BODY."
  (hn/200-content-type "text/html" body))

(defun hn/html (dom)
  "Given DOM, return an HTML string."
  (shr-dom-to-xml dom))

(defun hn/xml (dom)
  "Given DOM, return an XML string."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
    (dom-print dom nil t)
    (insert "\n")
    (buffer-string)))

(defun hn/atom-time (time)
  "Convert TIME to a string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time 0))

(defun hn/atom-feed (title url entries)
  "Given TITLE, URL, and ENTRIES, return an Atom feed listing those entries."
  (let*
    ( (newest (list 0 0 0 0))
      (nonces nil)
      (ents
        (-map
          (lambda (ent)
            (when (time-less-p newest (elfeed-entry-date ent))
              (setf newest (elfeed-entry-date ent)))
            (let ((nonce (s-concat "urn:sha256:" (secure-hash 'sha256 (elfeed-entry-link ent)))))
              (push nonce nonces)
              `(entry ()
                 (title ()
                   ,(s-concat
                      (elfeed-feed-title (elfeed-entry-feed ent)) " > " (elfeed-entry-title ent)))
                 (link ((href . ,(elfeed-entry-link ent))))
                 (id () ,nonce)
                 (author ()
                   (name () ,(hn/entry-author ent)))
                 (updated () ,(hn/atom-time (elfeed-entry-date ent)))
                 )))
          entries)))
    (hn/xml
      `(feed ((xmlns . "http://www.w3.org/2005/Atom"))
         (title () ,title)
         (link ((rel . "self") (href . ,url)))
         (link ((href . "https://news.colonq.computer")))
         (updated () ,(hn/atom-time newest))
         (id () "https://news.colonq.computer/")
         ,@ents))))

(defun hn/listing-page (title entries)
  "Given TITLE and ENTRIES, return a page listing those entries."
  (hn/html
    `(html ()
       (head ()
         (title () ,title)
         (style () ,(f-read-text (f-join hn/root "main.css"))))
       (body ()
         (div ((id . "hn-body"))
           (div ((id . "hn-header"))
             (img ((src . "/logo.png")) "")
             (b ((id . "hn-title")) "Hacker News")
             (a ((href . "/new")) "new")
             " | past | comments | ask | show | "
             (a ((href . "/jobs")) "jobs")
             " | submit")
           ,(hn/render-entries entries))))))

(defun hn/respond (path)
  "Given PATH, return a response to send back."
  (cond
    ((s-equals? path "/logo.png")
      (hn/200-content-type "image/png" (f-read-bytes (f-join hn/root "mrgreen.png"))))
    ((s-equals? path "/atom.xml")
      (hn/200
        (hn/atom-feed "GREEN NEWS" "https://news.colonq.computer/atom.xml" (hn/get-recent-entries-no-duplicates))))
    ((s-equals? path "/new")
      (hn/200
        (hn/listing-page "New Links | Hacker News" (hn/get-recent-entries))))
    ((s-equals? path "/jobs")
      (hn/200
        (hn/listing-page "jobs | Hacker News" (hn/get-recent-entries-no-duplicates-no-youtube))))
    (t
      (hn/200
        (hn/listing-page "Hacker News" (hn/get-recent-entries-no-duplicates))))))

(defun hn/handle-request (proc)
  "Read and handle a request from the current buffer by client PROC."
  (when-let* ( (start (point))
               (end (save-excursion (search-forward "\r\n\r\n" nil t)))
               (req (buffer-substring start end)))
    (delete-region start end)
    (let ((path (cadr (s-split " " req))))
      (process-send-string proc (hn/respond path)))))

(defun hn/server-filter (proc data)
  "Process filter for the server on PROC and DATA."
  (with-current-buffer (get-buffer-create (hn/server-buffer proc))
    (when (not (marker-position (process-mark proc)))
      (set-marker (process-mark proc) (point-max)))
    (goto-char (process-mark proc))
    (insert data)
    (set-marker (process-mark proc) (point))
    (goto-char (point-min))
    (hn/handle-request proc)
    ))

(defun hn/server-start ()
  "Start the Hacker News server."
  (hn/server-stop)
  (make-network-process
    :name hn/server-process
    :family 'ipv4
    :service hn/port
    :server 5
    :filter #'hn/server-filter
    ))

(defun hn/server-stop ()
  "Stop the Hacker News server."
  (when (process-live-p (get-process hn/server-process))
    (delete-process hn/server-process)))

(defun hn/update-feeds ()
  "Update all feeds."
  (message "Updating feeds...")
  (elfeed-update))

(defun hn/main ()
  "Run the server and loop forever."
  (hn/server-start)
  (hn/update-feeds)
  (run-with-timer 600 t #'hn/update-feeds))

(provide 'hn)
;;; hn.el ends here
