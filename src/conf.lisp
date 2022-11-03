(in-package :cc-conf)

(defclass conf ()
  ((name :documentation "Name of configuration"
	 :initform (error "You didn't supply an initial value for slot name")
	 :initarg :name
	 :type string
	 :accessor name)
   
    (try-files :documentation "Files list that conf will try to load,
   yaml format only"
 	      :initform nil
 	      :initarg :try-files
 	      :accessor try-files
 	      :type list)
    
    (from-env :documentation "Load from environtment variables if
   t. E.g. if you search \"cc.foo.bar\", env registry will look for env
   variable name \"CC_FOO_BAR\""
 	     :initform t
 	     :initarg :from-env
 	     :accessor from-env)
    
   (env-prefix :documentation "Defines a prefix that ENVIRONMENT
  variables will use. E.g. if your prefix is \"CC\", the env registry
  will look for env variable that start with \"CC_\""
	       :initform ""
	       :initarg :env-prefix
	       :accessor env-prefix
	       :type string)
   
   (yaml-object :documentation "Object that store results of
  yaml:parse"
		:initform nil))
  
  (:documentation "Configuration class"))

(defparameter *default-conf*
  (make-instance 'conf
		 :name "cc-conf"
		 :try-files (list "/etc/config.yaml" "./config.yaml")
		 :from-env t
		 :env-prefix "")
  "The default conf")

(defmethod load-yaml ((c conf))
  "Load and return yaml files from try-files, set object to
conf:yaml-object."
  (with-slots (yaml-object try-files) c
    (let ((real-filename nil))
      (loop for filename in try-files
	    until real-filename
	    do (setf real-filename (probe-file filename)))
      (and real-filename (setf yaml-object (yaml:parse real-filename))))))

(defun dot-split-to-env-name (name &optional &key env-prefix)
  "Convert dot-split string like \"cc.foo.bar\" to environment
  variable name like \"CC_FOO_BAR\", with env-prefix concated as
  prefix."
  (let ((suffix (string-upcase (ppcre:regex-replace-all "\\." name "_"))))
    (if (> (length env-prefix) 0)
	(concatenate 'string env-prefix "_" suffix)
	suffix)))

(defun parse-float (str)
  (with-input-from-string (s str)
    (car (loop
	   :for num := (read s nil nil)
	   :while num
	   :collect num))))

(defun string/int/float (str)
  "Check if str can parse to float or int, float -> 'float
int -> 'int
else return 'string"
  (if (not str) nil
      (let ((have-chr nil)
	    (have-dot nil))
	(loop for c across str
	      :until have-chr
	      do
		 (let ((digit (digit-char-p c)))
		   (and (not digit)
			(case c
			  (#\. (setf have-dot t))
			  (otherwise (setf have-chr t))))))
	(if have-chr
	    'string
	    (if have-dot
		'float
		'int)))))

(defun parse-duration (str)
  "Parse string to seconds and microseconds as multiple value. 1d3h12m1s.123
where 123 is microsecond, 1s=1e6 microsecond."
  (let ((second 0)
	(microsecond 0)
	(value 0)) ;; value store temp number
    (loop for idex from 0 to (- (length str) 1)
	  do
	     (let* ((chr (aref str idex)) ;; chr = char of str[idex]
		    (digit (digit-char-p chr))) ;; digit = digit number of chr
	       (if digit
		   ;; if is digit, value = value *10 + digit
		   (setf value (+ (* 10 value) digit))
		   ;; else check d,h,m,s .xxxx
		   (case chr
		     (#\. (setf second (+ second value)
				microsecond
				(* 1000000 (parse-float (subseq str idex)))
				value 0
				;; break when reach .xyz
				idex (length str)))
		     (#\d (setf second (+ second (* 60 60 24 value))
				value 0))
		     (#\h (setf second (+ second (* 60 60 value))
				value 0))
		     (#\m (setf second (+ second (* 60 value))
				value 0))
		     (#\s (setf second (+ second value)
				value 0))
		     (otherwise (error (concatenate
					'string
					"Unreconize char: "
					(string chr))))))))
    (setf second (+ second value))
    (values second microsecond)))

(defun get-string-from-env (name &optional &key env-prefix)
  "Convert name to env-name and get env value string"
  (let ((env-name (dot-split-to-env-name name :env-prefix env-prefix)))
    (uiop:getenv env-name)))

(defun get-from-object (name obj)
  "Get object from yaml object"
  (let ((hierarches (ppcre:split "\\." name))
	(cur-obj obj))
    (dolist (field hierarches cur-obj)
      (setf cur-obj
	    (if (hash-table-p cur-obj)
		(gethash field cur-obj))))))

(defmethod get-value ((c conf) name)
  "Get object specify by name like \"cc.foo.bar\""
  ;; get from env
  (or (let ((v (get-string-from-env name :env-prefix (env-prefix c))))
	(case (string/int/float v)
	  (string v)
	  (int (parse-integer v))
	  (float (parse-float:parse-float v))
	  (otherwise nil)))
      ;; get from conf::yaml-object
      (get-from-object name (slot-value c 'yaml-object))))

(defun +load-yaml ()
  "Load yaml file from try-files into *default-conf*:yaml-object."
  (load-yaml *default-conf*))

(defun +get-value (name)
  "Get object specify by name lik \"cc.foo.bar\" from *default-conf*"
  (get-value *default-conf* name))

