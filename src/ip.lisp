(in-package :cc-net)

(defconstant *AF-INET* 2)
(defconstant *AF-INET6* 10)

(defconstant *V4* 'V4)
(defconstant *V6* 'V6)

(defconstant *max-ip-string-len* 64)

(defclass ip ()
  ((family :accessor ip-family :initarg :ip-family :initform nil
	   :documentation "IP family: 'V4 or 'V6")

   (addr :accessor ip-addr :initarg :ip-addr :initform nil
	 :documentation "The addr octet array"))

  (:documentation
   "The ip address"))

(defun ip-equal (a b)
  "Compare if a and b are equal as IP instance."

  (and
   (equal (ip-family a) (ip-family b))
   (equalp (ip-addr a) (ip-addr b))))

(defparameter *ipv4-scanner*
  (cl-ppcre:create-scanner
   "^((25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[0-9]{2}|[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[0-9]{2}|[0-9])$"
   :case-insensitive-mode t)
  "Scanner that detects if a string is an IPV4 address.")

(defparameter *ipv6-scanner*
  (cl-ppcre:create-scanner
   "^\s*((([0-9A-Fa-f]{1,4}:){7}([0-9A-Fa-f]{1,4}|:))|(([0-9A-Fa-f]{1,4}:){6}(:[0-9A-Fa-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){5}(((:[0-9A-Fa-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9A-Fa-f]{1,4}:){4}(((:[0-9A-Fa-f]{1,4}){1,3})|((:[0-9A-Fa-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){3}(((:[0-9A-Fa-f]{1,4}){1,4})|((:[0-9A-Fa-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){2}(((:[0-9A-Fa-f]{1,4}){1,5})|((:[0-9A-Fa-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9A-Fa-f]{1,4}:){1}(((:[0-9A-Fa-f]{1,4}){1,6})|((:[0-9A-Fa-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9A-Fa-f]{1,4}){1,7})|((:[0-9A-Fa-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(%.+)?\s*$"
   :case-insensitive-mode t)
  "Scanner that detects if a string is an IPV6 address.")

(defun ipv4-address-p (addr)
  "Determine if the given host is an IPV4 addr or a hostname."
  (cl-ppcre:scan *ipv4-scanner* addr))

(defun ipv6-address-p (addr)
  "Determine if the given host is an IPV6 addr or a hostname."
  (cl-ppcre:scan *ipv6-scanner* addr))

(defun ip-address-p (addr)
  "Determine if the given host is an IP or a hostname."
  (or (ipv4-address-p addr)
      (ipv6-address-p addr)))

(defmethod ip-len ((ip ip))
  "Get the length of ip address. 4 -> ipv4, 16 -> ipv6"
  (if (eql (ip-family ip) 'V4)
      4
      16))

(defmethod ip-to-string ((ip ip))
  "Convert ipv4/ipv6 address to string"
  (and (ip-addr ip)
       (let ((arr-len 4)
	     (af *AF-INET*)
	     (res nil))
	 (if (eql 'V6 (ip-family ip))
	     (progn (setf arr-len 16)
		    (setf af *AF-INET6*)))

	 (with-foreign-pointer-as-string (str *max-ip-string-len*)
	   (with-foreign-object (c-ip :uint8 arr-len)
	     ;; set array value as ip
	     (dotimes (i arr-len)
	       (setf (mem-aref c-ip :uint8 i) (elt (ip-addr ip) i)))

	     ;; call inet_ntop
	     (setf res (cc-libevent:evutil-inet-ntop af c-ip str *max-ip-string-len*)))))))

(defun ip-from-c-addr (ptr af-family)
  "Convert C inet_addr or inet6_addr to IP instance."
  (let ((arr-len 4)
	(addr nil)
	(family 'V4))
    (if (eql af-family *AF-INET6*)
	(progn (setf arr-len 16)
	       (setf family 'V6)))
    (setf addr (make-array arr-len))
    
    (dotimes (i arr-len)      
      (setf (elt addr i) (mem-aref ptr :uint8 i)))
    (make-instance 'ip :ip-family family :ip-addr addr)))


;; ipv4 for linux
(defcstruct in-addr-t
  (s_addr :uint32))

(defcstruct sockaddr-in
  (sin_family :ushort)
  (sin_port :ushort)
  (sin_addr (:struct in-addr-t)))

;; ipv6 for linux

(defcstruct in6-addr
  (addr :int8 :count 16))

(defcstruct sockaddr-in6
  (sin6_family :ushort)
  (sin6_port :ushort)
  (sin6_flowinfo :uint32)
  (sin6_addr (:struct in6-addr))
  (sin6_scope_id :uint32))



(defun ip-from-c-sockaddr (ptr)
  (let ((so (sockaddr-from-c ptr)))
    (sockaddr-ip so)))

(defun ip-from-string (str)
  "Convert ipv4/ipv6 string to IP instance."
  (let ((af *AF-INET*)
	(family 'V4)
	(arr-len 4)
	(addr nil)
	(res 0))
    (if (not (ipv4-address-p str))
	(progn (setf af *AF-INET6*)
	       (setf arr-len 16)
	       (setf family 'V6)))

    (with-foreign-object (c-ip :uint8 arr-len)
      (with-foreign-string (c-str str)
	(setf res (cc-libevent:evutil-inet-pton af c-str c-ip)))
      (if (eql res 1)
	  (ip-from-c-addr c-ip af)))))

(defmacro with-c-ip ((var ip) &body body)
  "Convert ip to C then run body"
  `(with-foreign-object (,var :uint8 (ip-len ip))
     ,@body))




(defclass sockaddr ()
    ((ip :accessor sockaddr-ip :initarg :ip :initform nil
	 :documentation "The ip address of socket")
     
     (port :accessor sockaddr-port :initarg :port :initform nil
	   :documentation "Port of TCP/UDP")))


(defun sockaddr-from-c (ptr)
  "Create sockaddr object from c sockaddr_in"
  (let ((osin_family nil)
	(inptr nil)
	(sptr nil)
	(port 0))

    (with-foreign-slots ((sin_family sin_port sin_addr) ptr (:struct sockaddr-in))
      (setf osin_family sin_family)
      (setf port sin_port))

    (let
	((mip
	   (if (eql osin_family *AF-INET*)
	       ;; ipv4	
	       (let* ((sin-addr (foreign-slot-pointer ptr '(:struct sockaddr-in) 'sin_addr))
		      (s-addr (foreign-slot-pointer sin-addr '(:struct in-addr-t) 's_addr)))
		 (ip-from-c-addr s-addr osin_family))
	       ;; ipv6
	       (let* ((sin6-addr (foreign-slot-pointer ptr '(:struct sockaddr-in6) 'sin6_addr))
		      (addr (foreign-slot-pointer sin6-addr '(:struct in6-addr) 'addr)))
		 (ip-from-c-addr addr osin_family)))))
      (make-instance 'sockaddr :ip mip :port port))))


