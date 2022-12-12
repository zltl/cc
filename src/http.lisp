(in-package :cc-net/http)

(defconstant +continue+ 100) ;; client should proceed to send
(defconstant +switch-protocols+ 101)
(defconstant +processing+ 102)
(defconstant +earlyhints+ 103)
(defconstant +ok+ 200)
(defconstant +created+ 201)
(defconstant +accepted+ 202)
(defconstant +nonauthoritative+ 203)
(defconstant +nocontent+ 204)
(defconstant +moveperm+ 301)
(defconstant +movetemp+ 302)
(defconstant +notmodified+ 304)
(defconstant +badrequest+ 400)
(defconstant +unauthorized+ 401)
(defconstant +paymentrequired+ 402)
(defconstant +forbidden+ 403)
(defconstant +notfound+ 404)
(defconstant +badmethod+ 405)
(defconstant +entitytoolarge+ 413)
(defconstant +expectationfailed+ 417)
(defconstant +internal+ 500)
(defconstant +notimplemented+ 501)
(defconstant +badgateway+ 502)
(defconstant +servunavail+ 503)


;;; requests

(defconstant +get+ (ash 1 0))
(defconstant +post+ (ash 1 1))
(defconstant +head+ (ash 1 2))
(defconstant +put+ (ash 1 3))
(defconstant +delete+ (ash 1 4))
(defconstant +options+ (ash 1 5))
(defconstant +trace+ (ash 1 6))
(defconstant +connect+ (ash 1 7))
(defconstant +patch+ (ash 1 8))
(defconstant +propfind+ (ash 1 9))
(defconstant +proppatch+ (ash 1 10))
(defconstant +mkcol+ (ash 1 11))
(defconstant +lock+ (ash 1 12))
(defconstant +unlock+ (ash 1 13))
(defconstant +copy+ (ash 1 14))
(defconstant +move+ (ash 1 15))

(defconstant +request+ 0)
(defconstant +response+ 1)

(defconstant +timeout+ 0)
(defconstant +eof+ 1)
(defconstant +invalid-header+ 2)
(defconstant +buffer-error+ 3)
(defconstant +request-cancel+ 4)
(defconstant +data-too-long+ 5)

(defstruct http-conn
  c
  base
  url
  scheme
  host
  port
  uri
  )

(defun http-conn-free (h)
  "Free http-conn instance"
  (cc-libevent:evhttp-connection-free (http-conn-c h)))

(defun http-conn-new (eb url)
  "Create and return a connection object for makeing http requests."
  (let ((http-uri-c (null-pointer))
	(scheme nil)
	(host nil)
	(port nil)
	(path nil)
	(query nil)
	(uri nil)
	(bev nil)
	(evhc nil)
	(evh nil))

    ;; parse url
    (setf http-uri-c
	  (with-foreign-string (url-c url)
	    (cc-libevent:evhttp-uri-parse url-c)))
    (if (null-pointer-p http-uri-c)
	(error cc-error:bad-argument "evhttp-uri-parse"))

    (unwind-protect
	 (progn
	   ;; check scheme
	   (setf scheme (cc-libevent:evhttp-uri-get-scheme http-uri-c))
	   (if (or (not scheme)
		   (and (string-not-equal scheme "http")
			(string-not-equal scheme "https")))
	       (error cc-error:bad-argument "scheme is not http/https"))

	   ;; host
	   (setf host (cc-libevent:evhttp-uri-get-host http-uri-c))
	   (if (= 0 (length host))
	       (error cc-error:bad-argument "cannot parse host from url"))

	   ;; port
	   (setf port (cc-libevent:evhttp-uri-get-port http-uri-c))
	   (if (= -1 port)
	       (if (string-equal scheme "http")
		   (setf port 80)
		   (setf port 443)))

	   ;; path
	   (setf path (cc-libevent:evhttp-uri-get-path http-uri-c))
	   (if  (= 0 (length path))
		(setf path "/"))

	   ;; query -> uri
	   (setf query (cc-libevent:evhttp-uri-get-query http-uri-c))
	   (if (= 0 (length query))
	       (setf uri path)
	       (setf uri (concatenate 'string
				       path
				       "?"
				       query)))

	   ;; bufev
	   (setf bev (cc-net:bufev-socket-new eb
					      -1
					      cc-net:+bev-opt-close-on-free+))

	   ;; evhttp connection new
           (setf evhc (cc-libevent:evhttp-connection-base-bufferevent-new
		       (cc-event:base-c eb)
		       (cc-event:base-dns-c eb)
		       (cc-net:bufev-c bev)
		       host
		       port))

           (setf evh (make-http-conn
		      :c evhc
		      :base eb
		      :url url
		      :scheme scheme
		      :host host
		      :port port
		      :uri uri)))

      ;; cleanup
      (and (not (null-pointer-p http-uri-c))
	   (cc-libevent:evhttp-uri-free http-uri-c)))
    
    ;; return created evh
    evh))

(defstruct request
  c
  ;; (cb request ...cb-args)
  cb
  ;;
  cb-args
  ;;
  ptr-for-table
  )

(defcallback http-request-callback :void
    ((reqptr :pointer)
     (arg :pointer))
  (let ((req (cc-event:event-table-get arg)))
    (apply (request-cb req) req (request-cb-args req))))

(defun request-new (cb &rest cb-args)
  "Create a new request object."
  (let ((r (make-request :cb cb
			 :cb-args cb-args
			 :ptr-for-table (foreign-alloc :char :count 1))))
    (setf (request-c r)
	  (cc-libevent:evhttp-request-new (callback http-request-callback)
					  (request-ptr-for-table r)))
    r))

(defun request-free (r)
  "Free request instance"
  ;; Pretty sure the request should be freed automatically after the
  ;; http_request_done has finished executing since you didn't use
  ;; evhttp_request_own to take ownership of the request object so it
  ;; results in a double free.
  ;; (and (request-c r) (cc-libevent:evhttp-request-free (request-c r)))
  (and (request-ptr-for-table r)
       (progn
	 (cc-event:event-table-del (request-ptr-for-table r))
	 (foreign-free (request-ptr-for-table r))         
	 )))


(defun request-do (hcon req type)
  "Make an http request over specified connection.
HCON: instance of http-conn
REQ: the request instance
TYPE: +get+/+post+/..."
  (cc-event:event-table-set (request-ptr-for-table req) req)
  (let ((head (cc-libevent:evhttp-request-get-output-headers (request-c req))))
    (with-foreign-strings ((k-host-c "Host")
			   (v-host-c (http-conn-host hcon))
			   (k-ua "User-Agent")
			   (v-ua "cc-net/http")
			   (uri-c (http-conn-uri hcon)))
      (if (= 0 (length  (cc-libevent:evhttp-find-header head k-host-c)))
	  (cc-libevent:evhttp-add-header head k-host-c v-host-c))
      (if (= 0 (length  (cc-libevent:evhttp-find-header head k-ua)))
	  (cc-libevent:evhttp-add-header head k-ua v-ua))
      (cc-libevent:evhttp-make-request
       (http-conn-c hcon)
       (request-c req)
       type
       uri-c))))

(defun request-get-response-code (req)
  "get response status code"
  (cc-libevent:evhttp-request-get-response-code (request-c req)))
