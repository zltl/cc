LISP ?= sbcl

.PHONY: run build test clean

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load cc.asd \
		--eval '(ql:quickload :cc)' \
		--eval '(asdf:make :cc)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp

clean:
	rm -rf cc
