(in-package :cc)

(defmacro case-expr (&rest cond-expr-list)
  "Case switch by condition expresion

(case-expr
 (nil (format t \"1\") 1)
 (nil (format t \"2\") 2)
 (t   (format t \"3\") 3)
 (t   (format t \"4\") 4))

=> print 3, return 3
"
  (let ((con (gensym))
	(expr (gensym)))
    `(loop for (,con . ,expr) in ',cond-expr-list
	   when ,con
	     return (eval (push 'progn ,expr)))))

;; Define your project functionality here...

(defun greet (&optional (name "liaotonglang@gmail.com"))
  (log:info "Hello ~a from ~a!~&" name "cc"))

(defun help ()
  (format t "~&Usage:

  cc [name]~&"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (greet  (or (first argv)
              "dear lisp user")))

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))

