(in-package :cc-errno)

(defcvar "errno" :int)

(defcfun (strerror "strerror") :string (errno :int))

(defun str ()
  (strerror *errno*))


