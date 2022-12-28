(in-package :cc-examples)

(defun handle-root (req)
  (log:info "/ => return 111")
  (http:request-reply-string req http:+ok+ "111"))
(defun handle-error (req)
  (log:info "/error => 500")
  (/ 3 0))
(defun handle-foo (req)
  (log:info "/foo => return 222")
  (http:request-reply-string req http:+ok+ "222"))

;; html generator
(defparameter *shopping-list*
  '("Atmospheric ponds"
    "Electric gumption socks"
    "Mrs. Leland's embyronic television combustion"
    "Savage gymnatic aggressors"
    "Pharmaceutical pianos"
    "Intravenous retribution champions"))
(defparameter *user-name* "John Q. Lisper")
(defparameter *last-login* "12th Never")
(defmacro with-page ((&key title) &body body)
  `(with-html-string
       (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))
(defun shopping-list ()
  (with-page (:title "Home page")
    (:header
     (:h1 "Home page"))
    (:section
     ("~A, here is *your* shopping list: " *user-name*)
     (:ol (dolist (item *shopping-list*)
	    (:li (1+ (random 10)) item))))
    (:footer ("Last login: ~A" *last-login*))))
(defun handle-shop (req)
  (log:info "/html/shop => return generated html")
  (http:request-reply-string req http:+ok+ (shopping-list)))

(defun handle-json (req)
  (log:info "/json/bar => return json")
  (let ((h (http:make-keyvals)))
    (http:keyvals-add h "Content-Type" "application/json")
    (http:request-set-output-headers req h))
  (http:request-reply-string
   req
   http:+ok+
   (json:encode-json-to-string '#(((foo . (1 2 3)) (bar . t) (baz . #\!)) "quux" 4/17 4.25))))

(defun handle-post-json (req)
  (log:info "/json/bar => parse json then echo")
  (let ((h (http:make-keyvals)))
    (http:keyvals-add h "Content-Type" "application/json")
    (http:request-set-output-headers req h))

  (let* ((inbuf (http:request-get-input-buffer req))
	 (instr (net:buffer-to-string inbuf)))
    (log:info "request body = ~a" instr)
    (log:info "parsed: ~a" (json:decode-json-from-string instr))

    (http:request-reply-string
     req
     http:+ok+
     instr)))

(defun handle-varname3 (req)
  (log:info "/text/:xxx/:yyy/:zzz => return 222")
  (let ((params (list
		 (http:request-param req ":xxx")
		 (http:request-param req ":yyy")
		 (http:request-param req ":zzz"))))
    (log:info "param: ~a" params)
    (let ((h (http:make-keyvals)))
      (http:keyvals-add h "Content-Type" "application/json")
      (http:request-set-output-headers req h))    
    (http:request-reply-string req http:+ok+ (json:encode-json-to-string params))))
(defun handle-varname2 (req)
  (log:info "/text/:xxx/:yyy => return 222")
  (let ((params (list
		 (http:request-param req ":xxx")
		 (http:request-param req ":yyy"))))
    (log:info "param: ~a" params)
    (let ((h (http:make-keyvals)))
      (http:keyvals-add h "Content-Type" "application/json")
      (http:request-set-output-headers req h))    
    (http:request-reply-string req http:+ok+ (json:encode-json-to-string params))))
(defun handle-varname1 (req)
  (log:info "/text/:xxx => return 222")
  (let ((params (list
		 (http:request-param req ":xxx"))))
    (log:info "param: ~a" params)
    (let ((h (http:make-keyvals)))
      (http:keyvals-add h "Content-Type" "application/json")
      (http:request-set-output-headers req h))    
    (http:request-reply-string req http:+ok+ (json:encode-json-to-string params))))
(defun handle-varname-jump (req)
  (log:info "/jump/:xxx/foo => return 222")
  (let ((params (list
		 (http:request-param req ":xxx"))))
    (log:info "param: ~a" params)
    (let ((h (http:make-keyvals)))
      (http:keyvals-add h "Content-Type" "application/json")
      (http:request-set-output-headers req h))    
    (http:request-reply-string req http:+ok+ (json:encode-json-to-string params))))
(defun handle-matchall (req)
  (log:info "/mall/*rest => return 222")
  (let ((params (list
		 (http:request-param req "*rest"))))
    (log:info "param: ~a" params)
    (let ((h (http:make-keyvals)))
      (http:keyvals-add h "Content-Type" "application/json")
      (http:request-set-output-headers req h))    
    (http:request-reply-string req http:+ok+ (json:encode-json-to-string params))))

(defun http-simple-server ()
  "Entry point for the example."
  (log:info "start http-simple-server")
  (event:with-base-loop (eb)
    (let ((srv (http:server-new eb))
	  (mux (http:mux-new)))

      ;; set handles to mux
      (http:mux-get mux "/" #'handle-root)
      (http:mux-get mux "/error" #'handle-error)
      (http:mux-get mux "/text/foo" #'handle-foo)
      (http:mux-get mux "/html/shop" #'handle-shop)
      (http:mux-get mux "/json/bar" #'handle-json)
      (http:mux-post mux "/json/bar" #'handle-post-json)
      (http:mux-get mux "/text/:xxx/:yyy/:zzz" #'handle-varname3)
      (http:mux-get mux "/text/:xxx/:yyy" #'handle-varname2)
      (http:mux-get mux "/text/:xxx" #'handle-varname1)
      (http:mux-get mux "/jump/:xxx/foo" #'handle-varname-jump)
      (http:mux-get mux "/mall/*rest" #'handle-matchall)
      
      ;; print
      (http:mux-dfs-print (http:mux-root mux) nil)
      
      ;; bind server
      (http:server-bind srv
			(net:sockaddr-from-string "0.0.0.0:8899"))
      ;; the default content type
      (http:server-set-default-content-type srv "text/html")

      (http:server-set-cb srv :cb (http:mux-serve mux)))))
