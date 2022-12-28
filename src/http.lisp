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

  ;; server only
  s

  param-map
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

(defun request-param-get (req name)
  "Get param from request"
  (let ((param (request-param-map req)))
    (if (not param)
	nil
	(gethash name param))))
(defun request-param-del (req name)
  "Delete param from request"
  (let ((param (request-param-map req)))
    (if (not param)
	nil
	(remhash name param))))
(defun request-param-set (req name value)
  "Set param to request"
  (let ((param (request-param-map req)))
    (if (not param)
	(progn (setf param (make-hash-table :test #'equalp))
	       (setf (request-param-map req) param)))
    (setf (gethash name param) value)))

(setf (fdefinition 'request-param) #'request-param-get)
(defsetf request-param request-param-set)


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
(defun request-get-path (req)
  "Return the path of request."
  (let* ((uri (cc-libevent:evhttp-request-get-evhttp-uri (request-c req)))
	 (path (cc-libevent:evhttp-uri-get-path uri)))
    path))

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

(setf (fdefinition 'request-input-headers) #'request-get-input-headers)
(defsetf request-input-headers request-set-input-headers)
(setf (fdefinition 'requeset-output-headers) #'request-get-output-headers)
(defsetf request-output-headers request-set-output-headers)

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
  (make-hash-table :test #'equalp))

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

(setf (fdefinition 'keyvals-field) #'keyvals-gets)
(defsetf keyvals-field keyvals-sets)

(defun evkeyvalq-to-hash-table (kvs-c)
  "Convert struct evkeyvalq to hash table from key to value list."
  (let ((m (make-keyvals))
	(next kvs-c))
    (loop while (not (null-pointer-p next))
	  do
	     (with-foreign-slots
		 ((tqe-next
		   key
		   value) next (:struct evkeyval))
	       (if (not (null-pointer-p key))
		       (keyvals-add m key value))
	       (setf next tqe-next)))
    m))
(defun evkeyvalq-from-hash-table (kvs-c m)
  "Convert lisp hash table to C struct evkeyvalq."
  (maphash (lambda (key value-list)
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


(defstruct server
  c
  base

  ;; (cb req ...args)
  cb
  cb-args
  
  ;; (error-cb req buffer errcode reason ..err-cb-args)
  error-cb
  error-cb-args  
  )

(defcallback http-server-callback :void
    ((req-c :pointer) (args :pointer))
  (let* ((s (event:event-table-get args))
	 (req (make-request :c req-c :s s)))
    (apply (server-cb s)
	   req
	   (server-cb-args s))))

(defcallback http-server-error-callback :void
    ((req :pointer)
     (buffer :pointer)
     (errorno :int)
     (reason :string)
     (cbarg :pointer))
  (let ((s (event:event-table-get args))
	(req (make-request :c req-c
			   :s s)))
    (and (server-error-cb s)
	 (apply (server-error-cb s)
		req
		buffer
		reason
		(server-error-cb-args s)))))

(defun server-new (eb)
  "Create a new http server."
  (let* ((c (cc-libevent:evhttp-new (cc-event:base-c eb)))
	 (s (make-server :c c :base eb)))
    (event:event-table-set (server-c s) s)
    (cc-libevent:evhttp-set-gencb c
				  (callback http-server-callback)
				  c)
    s))

(defun server-set-cb (s &key cb cb-args)
  "set callback of http server."
  (setf (server-cb s) cb)
  (setf (server-cb-args s) cb-args))

(defun server-set-error-cb (s &key cb cb-args)
  (setf (server-error-cb s) cb)
  (setf (server-error-cb-args s) cb-args))

(defun server-free (s)
  "Free the created HTTP server."
  (cc-libevent:evhttp-free (server-c s))
  (event:event-table-del (server-c s)))

(defun server-bind (s sock)
  "Binds an http server on the specified sockaddr."
  (let ((addr-str (net:ip-to-string (net:sockaddr-ip sock)))
	(port (net:sockaddr-port sock)))
    (with-foreign-string (addr-c addr-str)
      (cc-libevent:evhttp-bind-socket (server-c s)
				      addr-c
				      port))))

(defun server-bind-listener (s ls)
  "Bind an http server on listener"
  (cc-libevent:evhttp-bind-listener (server-c c)
				    (net:listener-c ls)))


(defun server-set-max-headers-size (s size)
  (cc-libevent:evhttp-set-max-headers-size (server-c s) size))
(defun server-set-max-body-size (s size)
  (cc-libevent:evhttp-set-max-body-size (server-c s) size))
(defun server-set-max-connections (s num)
  (cc-libevent:evhttp-set-max-connections (server-c size)))
(defun server-get-connection-count (s)
  "Get the current number of connections."
  (cc-libevent:evhttp-get-connection-count (server-c s)))
(defun server-set-default-content-type (s content-type)
  "Set the value to use for the Content-Type header when one was
  provided."
  (with-foreign-string (ct content-type)
    (cc-libevent:evhttp-set-default-content-type
     (server-c s)
     (foreign-funcall "strdup"
		      (:pointer)
		      ct
		      :pointer))))

(defun server-set-allowed-methods (s methods)
  "defaults get,post,head,put,delete"
  (cc-libevent:evhttp-set-allowed-methods (server-c s)
					  methods))

(defun server-set-timeout (s timeout)
  "Request timeout for server."
  (cc-timeval:with-c-timeval-values (tv timeout)
      (cc-libevent:evhttp-set-timeout-tv (server-c s) tv)))

(defun server-set-read-timeout (s timeout)
  "Request timeout for server."
  (cc-timeval:with-c-timeval-values (tv timeout)
      (cc-libevent:evhttp-set-read-timeout-tv (server-c s) tv)))

(defun server-set-write-timeout (s timeout)
  "Request timeout for server."
  (cc-timeval:with-c-timeval-values (tv timeout)
      (cc-libevent:evhttp-set-write-timeout-tv (server-c s) tv)))

(defun request-reply-error (req code)
  "Send status code to client."
  (cc-libevent:evhttp-send-error (request-c req)
				 code
				 (null-pointer)))

(defun request-reply (req code databuffer)
  "Send and reply to client"
  (cc-libevent:evhttp-send-reply (request-c req)
				 code
				 (null-pointer)
				 databuffer))

(defun request-reply-string (req code str)
  "Send and reply string body to client."
  (let ((buf (net:buffer-new)))
    (net:buffer-add-string buf str)
    (cc-libevent:evhttp-send-reply (request-c req)
				   code
				   (null-pointer)
				   buf)
    (net:buffer-free buf)))

(defun request-reply-start (req code)
  "Initiate a reply that uses Transfer-Encoding chunked"
  (cc-libevent:evhttp-send-reply-start (request-c req)
				       code
				       (null-pointer)))

(defun request-reply-chunk (req databuffer)
  "Send data chunk as part of an ongoing chunked reply."
  (cc-libevent:evhttp-send-reply-chunk (request-c req)
				       databuffer))
(defun request-reply-end (req)
  "Complete a chunked reply."
  (cc-libevent:evhttp-send-reply-end (request-c req)))


;; websocket

(defstruct ws
  c
  req
  
  ;; (cb ws msg-vec ..cb-args)
  cb
  cb-args

  ;; 
  ptr-for-table
  )

(defcallback http-ws-msg-callback :void
  ((wscon :pointer)
   (type :int)
   (msg :pointer)
   (size :size)
   (arg-c :pointer))

  (let ((w (event:event-table-get wscon))
	(vec (make-array size :initial-element 0)))
    (loop for i from 0 below size
	  do (setf (elt vec i) (mem-aref msg :uint8 i)))
    (apply (ws-cb w) vec (ws--cb-args w))))

(defun ws-new (req &key cb cb-args)
  "Opens new websocket session from http request."
  (let* ((w (make-ws :ptr-for-table (foreign-alloc :char :count 1)
		     :req req
		     :cb cb
		     :cb-args cb-args))
	 (c (cc-libevent:evws-new-session (request-c req)
					  (callback http-ws-msg-callback)
					  (ws-ptr-for-table w)
					  0)))
    (setf (ws-c w) c)))

(defun ws-send (w vec)
  "sends data over weboscket connection"
  (with-foreign-object (out :uint8 (length vec))
    (loop for i from 0 below (length vec)
	  do (setf (mem-aref out :uint8 i) (elt vec i)))
    (cc-libevent:evws-send (ws-c w) out (length vec))))

(defun ws-close (w reson)
  "Close a websocket connection with reason code"
  (cc-libevent:evws-close (ws-c w) reason))

(defun ws-free (w)
  "Frees a websocket connection"
  (cc-libevent:evws-connection-free (ws-c w))
  (foreign-free (ws-ptr-for-table w)))




(defstruct mux
  root

  ;; (fallback-handler-cb req ...handler-cb-args)
  fallback-handler-cb
  fallback-handler-cb-args)

(defstruct mux-cb-item
  method
  cb
  cb-args)

(defstruct mux-node
  name
  
  ;; methods mask
  methods

  ;; list of mux-cb-item
  cb-list
  
  ;; childs mux-node hashtable
  childs

  ;; :foo list
  child-vars

  ;; *bar
  child-matchall-var)

(defun %mux-get-cb (node method)
  "Return (valus cb cb-args) of method for this node"
  ;; for each item on cb-list, if match method, return
  (dolist (it (mux-node-cb-list node))
    (if (> (logand method (mux-cb-item-method it)) 0)
	(return-from %mux-get-cb it)))
  ;; return nil if not found any item that match method
  nil)

(defun %mux-set-cb (node method cb cb-args)
  "Set handler cb to node."
  ;; check conflict first
  (log:info "mux set cb")
  (if (%mux-get-cb node method)
      (error (cc-error:conflict "cb conflict with before")))

  (setf (mux-node-methods node) (logior (mux-node-methods node) method))
  
  ;; push new cb to list
  (push (make-mux-cb-item :method method
			  :cb-args cb-args
			  :cb cb)
	(mux-node-cb-list node)))

(defun mux-new ()
  "Create and return new mux."
  (make-mux :fallback-handler-cb #'mux-default-404
	    :root (make-mux-node :name ""
				 :methods 0
				 :childs (make-hash-table :test #'equalp))))

(defun mux-get (m pattern &optional cb-h &key cb cb-args)
  (mux-handler m pattern :methods +get+ :cb (or cb cb-h) :cb-args cb-args))
(defun mux-post (m pattern &optional cb-h &key cb cb-args)
  (mux-handler m pattern :methods +post+ :cb (or cb cb-h) :cb-args cb-args))
(defun mux-put (m pattern &optional cb-h &key cb cb-args)
  (mux-handler m pattern :methods +put+ :cb (or cb cb-h) :cb-args cb-args))
(defun mux-delete (m pattern &optional cb-h &key cb cb-args)
  (mux-handler m pattern :methods +delete+ :cb (or cb cb-h) :cb-args cb-args))
(defun mux-default (m &optional cb-h &key cb cb-args)
  (setf (mux-fallback-handler-cb m) (or cb-h cb)
	(mux-fallback-handler-cb-args m) cb-args))

(defun mux-call (m req)
  "Call the specific callback setted."
  (let* ((path (request-get-path req))
	 (path-list (split-uri path))
	 (called (mux-dfs-call (mux-root m) path-list req)))
    (if called
	t ;; called, just return
 	(apply (mux-fallback-handler-cb m) req (mux-fallback-handler-cb-args m)))))

(defun mux-serve (m)
  "Return handler function of mux"
  (alexandria:curry #'mux-call m))

(defun mux-dfs-print (node path-list)
  "dfs and print all cb nodes."

  ;; null node, return
  (if (not node)
      (return-from mux-dfs-print))

  (let ((new-path-list (append path-list (list (mux-node-name node)))))

    ;; current node have callback
    (if (and (mux-node-methods node) (> (mux-node-methods node) 0))
	(log:info "~{~A/~} +~X+"
		  new-path-list
		  (mux-node-methods node)))

    ;; for each childs
    (maphash (lambda (key value)
	       (mux-dfs-print value new-path-list))
	     (mux-node-childs node))
    
    ;; for each var childs
    (dolist (next-node (mux-node-child-vars node))
      (mux-dfs-print next-node new-path-list))    

    ;; matchall child
    (mux-dfs-print (mux-node-child-matchall-var node) new-path-list)))

(defun mux-dfs-call (node path-list req)
  "dfs find node and call."
  ;; match current node

  (if (not path-list)
      (let ((s (%mux-get-cb node (request-get-command req))))
	(if s
	    (progn
	      (handler-case
		  (apply (mux-cb-item-cb s) req (mux-cb-item-cb-args s))
		(error (c)
		  (let ((ostr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
		    (with-output-to-string (out ostr)
		      (trivial-backtrace:print-backtrace c :output out))
		    (log:error "~a" ostr))
		  (request-reply-error req +internal+)))
              
	      (return-from mux-dfs-call t))
	    (return-from mux-dfs-call nil))))
  
  (let* ((curpath (car path-list))
	 (child-list (mux-node-childs node))
	 (next-node (and child-list (gethash curpath child-list))))

    (if next-node
	;; first we try searching full match	
	(mux-dfs-call next-node (cdr path-list) req)
	;; then else we try vars like :foo
        (progn
	  (dolist (child (mux-node-child-vars node))
	    (let ((child-name (mux-node-name child)))
	      ;; set param
	      (request-param-set req child-name curpath)
	      (if (mux-dfs-call child (cdr path-list) req)
		  (return-from mux-dfs-call t)
		  ;; if not found child, unset param back
		  (request-param-del req child-name))))
	  ;; finally try matchall
	  (let ((child (mux-node-child-matchall-var node)))
	    (if child
		(let ((child-name (mux-node-name child)))
		  (request-param-set req child-name (format nil "/~{~A~^/~}" path-list))
		  (mux-dfs-call child nil req))))))))

(defun mux-default-404 (req)
  "return 404"
  (request-reply-error req +notfound+))

(defun merge-methods (methods)
  "Merge method int/int-list to int mask."
  (cond ((numberp methods) methods)
	(t (reduce #'logior methods))))

(defun mux-handler (m pattern &key methods cb cb-args)
  "Math path with handlers.

# match full path
Pattern: /abc

 /abc                      match
 /abc/                     match

# match variables
Pattern: /user/:user

 /user/gordon              match
 /user/you                 match
 /user/gordon/profile      no match
 /user/                    no match

# match all
Pattern: /src/*filepath

 /src/                     match
 /src/somefile.go          match
 /src/subdir/somefile.go   match
"

  (let ((pattern-list (split-uri pattern))
	(node (mux-root m)))
    (mux-dfs-gen node pattern-list (merge-methods methods) cb cb-args)))

(defun mux-dfs-gen (node pattern-list methods cb cb-args)
  "DFS throw a mux radix tree, generate not exists node.
return nil if match failed."
  ;; match current node  
  (if (uiop:emptyp pattern-list)
      (progn
	(%mux-set-cb node methods cb cb-args)
	(return-from mux-dfs-gen t)))

  ;; is full path match node
  (let* ((cur-pattern (car pattern-list))
	 (firchr (uiop:first-char cur-pattern)))
    (cond ((eql firchr #\*)
	   ;; match all node *foo
	   ;; replace and return
	   (progn
	     (if (cdr pattern-list)
		 (error (error:bad-argument "variables start by a '*' must be the last part of path")))
	     ;; new node if not matchall var exists
	     (if (not (mux-node-child-matchall-var node))
		 (setf (mux-node-child-matchall-var node) (make-mux-node :name cur-pattern
									 :methods methods
									 :childs (make-hash-table :test #'equalp))))
	     (if (string/= cur-pattern (mux-node-name (mux-node-child-matchall-var node)))
		 (error (error:conflict "conflict '*var' mux with previous")))
	     (%mux-set-cb (mux-node-child-matchall-var node) methods cb cb-args)))

	  ((eql firchr #\:)
	   ;; var node :bar
           (progn (dolist (child (mux-node-child-vars node))
		    (if (string= cur-pattern (mux-node-name child))
			(if (mux-dfs-gen child (cdr pattern-list) methods cb cb-args)
			    (return-from mux-dfs-gen t))))
		  ;; not found, create one
		  (let ((new-node (make-mux-node :name cur-pattern
						 :methods 0
						 :childs (make-hash-table :test #'equalp))))
		    (push new-node
			  (mux-node-child-vars node))
		    (mux-dfs-gen new-node (cdr pattern-list) methods cb cb-args))))
	  (t
	   ;; full match '*xyz'
	   (progn
	     (let ((next-node (gethash cur-pattern (mux-node-childs node))))
	       (if (not next-node)
		   (progn
		     (setf next-node (make-mux-node :name cur-pattern
						    :methods 0
						    :childs (make-hash-table :test #'equalp)))
		       (setf (gethash cur-pattern (mux-node-childs node)) next-node)))
	       (mux-dfs-gen next-node (cdr pattern-list) methods cb cb-args)))))))

(defun split-uri (pattern)
  "Split uri by '/', trim empty strings."
  (remove-if #'uiop:emptyp (uiop:split-string pattern :separator "/")))
