(in-package :cc-tests)

;; Define your project tests here...

(ql:quickload "cc")

(def-suite testmain
    :description "test suite 1")

(in-suite testmain)

(test test1
  (is (= (+ 1 2)
         3)))

