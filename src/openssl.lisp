(in-package :cc-openssl)


(define-foreign-library openssl_crypto
  (:unix (:or "libcrypto.so"))
  (t (:default "libcrypto")))
(define-foreign-library openssl_ssl
  (:unix (:or "libssl.so"))
  (t (:default "libssl")))

(use-foreign-library openssl_crypto)
(use-foreign-library openssl_ssl)

; int OPENSSL_init_ssl(uint64_t opts, const OPENSSL_INIT_SETTINGS *settings);
(defcfun (openssl-init-ssl "OPENSSL_init_ssl") :int
  (opts :uint64)
  (settings :pointer))

(OPENSSL-init-ssl 0 (null-pointer))

;; const SSL_METHOD *TLSv1_2_method(void))
(defcfun (tlsv1.2-method "TLSv1_2_method") :pointer)

;; SSL_CTX *SSL_CTX_new(const SSL_METHOD *meth);
(defcfun (ssl-ctx-new "SSL_CTX_new") :pointer
  (meth :pointer))

;; SSL *SSL_new(SSL_CTX *ctx);
(defcfun (ssl-new "SSL_new") :pointer
  (ctx :pointer))

;; void SSL_free(SSL *ssl);
(defcfun (ssl-free "SSL_free") :void
  (ssl :pointer))
;; void SSL_CTX_free(SSL_CTX *);
(defcfun (ssl-ctx-free "SSL_CTX_free") :void
  (ctx :pointer))

(defcvar "stderr" :pointer)
;; void ERR_print_errors_fp(FILE *fp);
(defcfun (err-print-errors-fp "ERR_print_errors_fp") :void
  (fp :pointer))
(defun perror ()
  (err-print-errors-fp stderr))
