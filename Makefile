all: compile test

autoloads:
	@emacs -batch \
        --eval '(setq generated-autoload-file (expand-file-name "evil-snipe-autoloads.el"))' \
		-f batch-update-autoloads .

compile:
	@emacs -batch -L . -f batch-byte-compile *.el

test:
	@emacs -batch -L . -L themes/ -l test/test-helper.el test/*-test.el

clean:
	@rm -vf *.elc *-autoloads.el *~

.PHONY: test
