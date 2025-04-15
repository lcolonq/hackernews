.PHONY: run
hn.eln: hn.el
	emacs --batch --load=compile.el
run: hn.eln
	emacs --fg-daemon=hackernews --quick --load hn.el --eval '(hn/main)'
