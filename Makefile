LISP ?= sbcl

.PHONY: run build test clean example

all: test

run:
	$(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load cc.asd \
		--eval '(ql:quickload :cc)' \
		--eval '(asdf:make :cc)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp

example:
	$(LISP) --non-interactive \
		--load run-example.lisp

clean:
	rm -rf cc
