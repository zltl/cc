(in-package :cc-tests)

;; Define your project tests here...

(ql:quickload "cc")

(def-suite testmain
    :description "test suite 1")

(in-suite testmain)

(test test1
  (is (= (+ 1 2)
         3)))

(test case-expr
      (is (= 3 (cc:case-expr
		(nil (format t "1") 1)
		(nil (format t "2") 2)
		(t   (format t "3") 3)
		(t   (format t "4") 4)))))

(cc:case-expr
 (nil (format t "1") 1)
 (nil (format t "2") 2)
 (t   (format t "3") 3)
 (t   (format t "4") 4))
