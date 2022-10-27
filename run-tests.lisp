
(load "cc.asd")
(load "cc-tests.asd")

(ql:quickload "cc-tests")

(in-package :cc-tests)

(uiop:quit (if (run-all-tests) 0 1))
