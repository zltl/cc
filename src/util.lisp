(in-package :cc-util)

(defun make-pointer-eql-able (pointer)
  "Abstraction to make a CFFI pointer #'eql to itself. Does its best to be the
   most performant for the current implementation."
  (when pointer
    #+(or ccl)
      pointer
    #-(or ccl)
      (if (cffi:pointerp pointer)
          (cffi:pointer-address pointer)
          pointer)))
