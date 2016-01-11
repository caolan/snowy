(defstruct response
  port
  (code 200)
  reason
  (headers `((content-type . "text/html; charset=utf-8")
             (date . ,(rfc1123-time->string (seconds->utc-time)))
             ;; TODO: set this based on the keep-alive value of the request obj
             (connection . "keep-alive")
             (x-content-type-options . "nosniff")))
  body)

(define-record-printer (response x out)
  (fprintf out "#<response ~S ~S ~S ~S>"
           (response-code x)
           (response-reason x)
           (response-headers x)
           (response-body x)))

(define (response-keep-alive res)
  (and-let* ((value (response-headers-ref res 'connection)))
    (string=? "keep-alive" value)))

(define status-codes
  (alist->hash-table
    '((100 . "Continue")
      (101 . "Switching Protocols")
      (200 . "OK")
      (201 . "Created")
      (202 . "Accepted")
      (203 . "Non-Authoritative Information")
      (204 . "No Content")
      (205 . "Reset Content")
      (206 . "Partial Content")
      (300 . "Multiple Choices")
      (301 . "Moved Permanently")
      (302 . "Moved Temporarily")
      (303 . "See Other")
      (304 . "Not Modified")
      (305 . "Use Proxy")
      (400 . "Bad Request")
      (401 . "Unauthorized")
      (402 . "Payment Required")
      (403 . "Forbidden")
      (404 . "Not Found")
      (405 . "Method Not Allowed")
      (406 . "Not Acceptable")
      (407 . "Proxy Authentication Required")
      (408 . "Request Time-out")
      (409 . "Conflict")
      (410 . "Gone")
      (411 . "Length Required")
      (412 . "Precondition Failed")
      (413 . "Request Entity Too Large")
      (414 . "Request-URI Too Large")
      (415 . "Unsupported Media Type")
      (500 . "Internal Server Error")
      (501 . "Not Implemented")
      (502 . "Bad Gateway")
      (503 . "Service Unavailable")
      (504 . "Gateway Time-out")
      (505 . "HTTP Version not supported"))
    size: 37
    hash: number-hash
    test: =))

(define (header-name->string name)
  (let ((upper-next #t))
    (string-map!
      (lambda (ch)
        (if upper-next
          (begin
            (set! upper-next #f)
            (char-upcase ch))
          (begin
            (when (char=? ch #\-)
              (set! upper-next #t))
            ch)))
      (symbol->string name))))

(define (code->reason code)
  (hash-table-ref/default status-codes code "unknown"))

(define (prepare-response-head res)
  (let ((reason (or (response-reason res)
                    (code->reason (response-code res))))
        (headers (response-headers res)))
    (if (not (any (lambda (h)
                    (or (eq? 'content-length h)
                        (eq? 'transfer-encoding h)))
                  headers))
      (update-response res
        reason: reason
        headers: (cons '(transfer-encoding . "chunked") headers))
      (update-response res reason: reason))))

(define (response-write-head res)
  (let ((res (prepare-response-head res)))
    (with-output-to-port (response-port res)
      (lambda ()
        (let* ((code (response-code res))
               (reason (response-reason res)))
          (write-string
            (string-append/shared
              "HTTP/1.1 " (number->string code) " " reason "\r\n"))
          (for-each (lambda (h)
                      (write-string
                        (string-append/shared
                          (header-name->string (car h)) ": " (cdr h) "\r\n")))
                    (response-headers res))
          (write-string "\r\n")
          res)))))

(define (write-chunk str port)
  (write-string
    (string-append/shared
      (number->string (string-length str) 16) "\r\n"
      str "\r\n")
    #f
    port))

(define (make-chunked-body-port port)
  (make-output-port
    (lambda (str) (write-chunk str port))
    (lambda () (write-string "0\r\n\r\n" #f port))
    (lambda () (flush-output port))))

(define (make-body-port port len)
  (if (not len)
    (make-chunked-body-port port)
    (make-output-port
      (lambda (str) (write-string str #f port))
      (lambda () #f)
      (lambda () (flush-output port)))))

(define (response-write-body res)
  (let ((body (response-body res))
        (len (response-headers-ref res 'content-length))
        (port (response-port res)))
    (cond
      ((procedure? body)
       (let ((out (make-body-port port len)))
         (body out)
         (close-output-port out)))
      ((not len)
       (when body (write-chunk body port))
       (write-string "0\r\n\r\n" #f port))
      (else
        (when body (write-string body #f port))))
    res))

(define (response-write res)
  (response-write-body (response-write-head res)))

(define (merge-headers new old #!optional (pending new))
  (if (null? old)
    new
    (let* ((x (car old))
           (k (car x)))
      (if (assq k pending)
        (merge-headers new (cdr old) pending)
        (merge-headers (cons x new) (cdr old) pending)))))

(define (response-headers-merge res new)
  (let ((old (response-headers res)))
    (update-response res headers: (merge-headers new old))))

(define (response-headers-ref res name)
  (alist-ref name (response-headers res)))
