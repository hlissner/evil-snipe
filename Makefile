all: autoloads compile test

autoloads:
	@emacs -batch \
        --eval '(setq generated-autoload-file (expand-file-name "evil-snipe-autoloads.el"))' \
		-f batch-update-autoloads .

compile:
	@emacs -batch -L . -l test/setup.el -f batch-byte-compile *.el

test:
	@emacs -batch -L . -l test/setup.el -l test/test-helper.el test/*-test.el

clean:
	@rm -vrf *.elc *-autoloads.el *~ test/.packages

.PHONY: test
