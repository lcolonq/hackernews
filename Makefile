.PHONY: run
hn.eln: hn.el
	emacs --batch --load=compile.el
run: hn.eln
	emacs --batch --load=hn.eln --eval='(hn/main)'
