emacs ?= emacs
# EMACS = emacs-24.3

LOAD = -l emacs-conflict.el -l emacs-conflict-test.el

.PHONY: all test clean checkdoc

all: compile test checkdoc

test:
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

compile:
	$(emacs) -batch -l targets/emacs-conflict-init.el

run:
	$(emacs) -Q -l targets/emacs-conflict-init.el

clean:
	rm -f *.elc
