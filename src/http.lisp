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

(defun http-conn-set-max-header-size (conn size)
  (cc-libevent:evhttp-connection-set-max-headers-size
   (http-conn-c conn)
   size))
(defun http-conn-set-max-body-size (conn size)
  (cc-libevent:evhttp-connection-set-max-body-size
   (http-conn-c conn)
   size))


(defun http-conn-set-connect-timeout (conn timeout)
  "Sets the connect timeout for this connection.
TIMEOUT: '(second, miscrosecond)"
  (cc-timeval:with-c-timeval-values (tv timeout)
    (cc-libevent:evhttp-connection-set-connect-timeout-tv
     (http-conn-c conn)
     tv)))

(defun http-conn-set-read-timeout (conn timeout)
  "Sets the read timeout for this connection.
TIMEOUT: '(second, miscrosecond)"
  (cc-timeval:with-c-timeval-values (tv timeout)
    (cc-libevent:evhttp-connection-set-read-timeout-tv
     (http-conn-c conn)
     tv)))

(defun http-conn-set-write-timeout (conn timeout)
  "Sets the write timeout for this connection.
TIMEOUT: '(second, miscrosecond)"
  (cc-timeval:with-c-timeval-values (tv timeout)
    (cc-libevent:evhttp-connection-set-write-timeout-tv
     (http-conn-c conn)
     tv)))

(defun http-conn-set-initial-retry-timeout (conn timeout)
  "Sets the retry timeout for this connection.
TIMEOUT: '(second, miscrosecond)"
  (cc-timeval:with-c-timeval-values (tv timeout)
    (cc-libevent:evhttp-connection-set-initial-retry-tv
     (http-conn-c conn)
     tv)))

(defun http-conn-set-retries (conn retry-max)
  "Set the retry limit for the connection, -1 repeats indefinitely."
  (cc-libevent:evhttp-connection-set-retries
   (http-conn-c conn)
   retry-max))

(defun http-conn-get-peer (conn)
  "Return sockaddr of remote peer"
  (net:sockaddr-from-c
   (cc-libevent:evhttp-connection-get-addr (conn-http-c conn))))




(defstruct request
  c
  ;; (cb request ...cb-args)
  cb

  ;; (chunk-cb request ...cb-args)
  chunk-cb

  ;; (header-cb request ...cb-args)
  ;; return int
  ;; return < 0 will close the connection
  header-cb

  ;; (error-cb request errcode ...cb-args)
  error-cb

  ;; (complete-cb request ...cb-args)
  complete-cb 
  
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
(defcallback http-request-chunk-callback :void
    ((reqptr :pointer)
     (arg :pointer))
  (let ((req (cc-event:event-table-get arg)))
    (apply (request-chunk-cb req) req (request-cb-args req))))
(defcallback http-request-header-callback :void
    ((reqptr :pointer)
     (arg :pointer))
  (let ((req (cc-event:event-table-get arg)))
    (apply (request-header-cb req) req (request-cb-args req))))
(defcallback http-request-error-callback :void
    ((ercode :int)
     (arg :pointer))
  (let ((req (cc-event:event-table-get arg)))
    (apply (request-error-cb req) req (request-cb-args req))))
(defcallback http-request-complete-callback :void
    ((ercode :int)
     (arg :pointer))
  (let ((req (cc-event:event-table-get arg)))
    (apply (request-complete-cb req) req (request-cb-args req))))

(defun request-new (&key cb cb-args)
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
  (and (request-c r)
       (= 1 (cc-libevent:evhttp-request-is-owned (request-c r)))
       (cc-libevent:evhttp-request-free (request-c r)))
  (and (request-ptr-for-table r)
       (progn
	 (cc-event:event-table-del (request-ptr-for-table r))
	 (foreign-free (request-ptr-for-table r)))))


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

(defun request-set-chunk-cb (req &key cb cb-args)
  "Enable delivery of chunks to requestor.
CB: called after every read of data with the same argument as the
  completion callback. Never be called on empty response. Drain on
  return."

  (cc-libevent:evhttp-request-set-chunked-cb
   (request-c req)
   (callback
    http-request-chunk-callback)))


(defun request-set-header-cb (req &key cb cb-args)
  "Called after receiving and parsing the full header.
cb return < 0 will close the connection."

  (cc-libevent:evhttp-request-set-header-cb
   (request-c req)
   (callback
    http-request-header-callback)))

(defun request-set-error-cb (req &key cb cb-args)
  "Set a callback for errors"
  (cc-libevent:evhttp-request-set-error-cb
   (request-c req)
   (callback
    http-request-error-callback)))

(defun request-set-complete-cb (req *key cb cb-args)
  "Set callback to be called on request completion of evhttp_send_*
function."
  (cc-libevent:evhttp-request-set-on-complete-cb
   (request-c req)
   (callback
    http-request-complete-callback)))

(defun request-get-uri-string (req)
  "return the request URI"
  (cc-libevent:evhttp-request-get-uri (request-c req)))
(defun request-get-uri (req)
  "Return the parsed URI"
  (cc-libevent:evhttp-request-get-evhttp-uri (request-c req)))

(defun request-get-command (req)
  "Return the request command. +get+/+post+..."
  (cc-libevent:evhttp-request-get-command (request-c req)))

(defun request-get-response-code (req)
  "Get status code."
  (cc-libevent:evhttp-request-get-response-code (request-c req)))

(defun request-get-input-headers (req)
  "Returns the input headers"
  (evkeyvalq-to-hash-table
   (cc-libevent:evhttp-request-get-input-headers (request-c req))))
(defun request-get-output-headers (req)
  "Return the output headers"
  (evkeyvalq-to-hash-table
   (cc-libevent:evhttp-request-get-output-headers
    (request-c req))))
(defun request-set-input-headers (req kvs)
  "Set keyvalues to input request."
  (let ((h (cc-libevent:evhttp-request-get-input-headers (request-c
							  req))))
    (cc-libevent:evhttp-clear-headers h)
    (evkeyvalq-from-hash-table h kvs)))
(defun request-set-output-headers (req kvs)
  "Set keyvalues to output request."
  (let ((h (cc-libevent:evhttp-request-get-output-headers (request-c
							   req))))
    (cc-libevent:evhttp-clear-headers h)
    (evkeyvalq-from-hash-table h kvs)))

(defun request-get-input-buffer (req)
  "Return the input buffer."
  (cc-libevent:evhttp-request-get-input-buffer (request-c req)))
(defun request-get-output-buffer (req)
  "Return the output buffer."
  (cc-libevent:evhttp-request-get-output-buffer (request-c req)))
(defun request-get-host (req)
  "Return the host associated with the request."
  (cc-libevent:evhttp-request-get-host (request-c req)))

(defun header-get (h key)
  "Find the value belonging to a header"
  (with-foreign-string (k-c key)
    (cc-libevent:evhttp-find-header h k-c)))
(defun header-del (h key)
  "Removes a header from a list of existing headers."
  (with-foreign-string (k-c key)
    (cc-libevent:evhttp-remove-header h k-c)))
(defun header-add (h key value)
  "Adds a header to a list of existing headers."
  (with-foreign-strings ((key-c key) (value-c value))
    (cc-libevent:evhttp-add-header h key-c value-c)))
(defun header-clear (h)
  "Removes all headers from the header list."
  (cc-libevent:evhttp-clear-headers h))

(defun encode-uri (str)
  "URI encode."
  (with-foreign-string (str-c str)
    (let* ((res-c (cc-libevent:evhttp-encode-uri str))
	   (res (foreign-string-to-lisp res-c)))
      (foreign-free res-c)
      res)))

(defun decode-uri (str)
  "URI decode."
  (with-foreign-string (str-c str)
    (let* ((res-c (cc-libevent:evhttp-decode-uri str))
	   (res (foreign-string-to-lisp res-c)))
      (foreign-free res-c)
      res)))



(defcstruct evkeyval
  (tqe-next :pointer)
  (teq-prev :pointer)
  (key :pointer)
  (value :pointer))



(defun make-keyvals ()
  "Create a new kvs"
  (make-hash-table))

(defun keyvals-get (kvs key)
  "Get value by key from kvs"
  (let ((vs (gethash key kvs)))
    (if (not vs)
	nil
	(car vs))))
(defun keyvals-gets (kvs key)
  "Get all values list by key from kvs"
  (gethash key kvs))
(defun keyvals-set (kvs key value)
  "Replace key=value"
  (setf (gethash key kvs) (list value)))
(defun keyvals-sets (kvs key values)
  "Replace key=[values]"
  (setf (gethash key kvs) values))
(defun keyvals-add (kvs key value)
  "Add value to kvs[key]"
  (let ((old-v (gethash key kvs)))
    (push value old-v)
    (setf (gethash key kvs) old-v)))

(defun evkeyvalq-to-hash-table (kvs-c)
  "Convert struct evkeyvalq to hash table from key to value list."
  (let ((m (make-keyvals))
	(next nil))
    (setf next kvs-c)
    (loop while (not (null-pointer-p inf))
	  do
	     (with-foreign-slots
		 ((tqe-next
		   key
		   value) next (:struct evkeyval))
	       (keyvals-add m key value)
	       (setf next tqe-next))))
  m)
(defun evkeyvalq-from-hash-table (kvs-c m)
  "Convert lisp hash table to C struct evkeyvalq."
  (maphash #'(lambda (key value-list)
	       (dolist (v value-list)
		 (header-add kvs-c key v)))
	     m))

(defun parse-query (str)
  "Parse a URI like
    q=test&s=some+thing
to keyvals hash-table
    q=test, s=some thing"
  (with-foreign-string (str-c str)
    (with-foreign-object (kvs-c (:struct evkeyval))
      (cc-libevent:evhttp-parse-query-str str-c kvs-c)
      (evkeyvalq-to-hash-table kvs-c))))
