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
    (if env-prefix
	(concatenate 'string env-prefix "_" suffix)
	suffix)))

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
  (or (conf-get-string-from-env name :env-prefix (env-prefix c))
      ;; get from conf::yaml-object
      (get-from-object name (slot-value c 'yaml-object))))

(defun +load-yaml ()
  "Load yaml file from try-files into *default-conf*:yaml-object."
  (load-yaml *default-conf*))

(defun +get-value (name)
  "Get object specify by name lik \"cc.foo.bar\" from *default-conf*"
  (get-value *default-conf* name))
