(in-package :cc-errno)

(defcvar "errno" :int)

(defcfun (strerror "strerror") :string (errno :int))

(defun str ()
  "Return string by C code: strerror(errno)"
  (strerror *errno*))

(defun code ()
  "Return linux C integer variable value: errno"
  *errno*)
