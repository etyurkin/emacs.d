clean:
	@rm -f init.elc config.el config.elc core.el components/*.el
	@find lisp -name '*.elc' -delete 2>/dev/null || true
	@find eln-cache -name 'config-*.eln' -delete 2>/dev/null || true

compile: clean
	@emacs -Q --batch -l init.el --eval '(kwarks/tangle-config)'
