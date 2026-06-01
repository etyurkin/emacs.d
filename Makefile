clean:
	@rm -f init.elc config.el config.elc core.el components/*.el

compile: clean
	@emacs -Q --batch -l init.el --eval '(kwarks/tangle-config)'
