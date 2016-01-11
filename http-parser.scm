(foreign-declare "#include \"parser.h\"")

(define http-chunk-size (make-parameter 1024))

(define-foreign-enum-type (method (enum "http_method"))
  (method->int int->method)
  ((DELETE method/DELETE) HTTP_DELETE)
  ((GET method/GET) HTTP_GET)
  ((DELETE method/DELETE) HTTP_DELETE)
  ((GET method/GET) HTTP_GET)
  ((HEAD method/HEAD) HTTP_HEAD)
  ((POST method/POST) HTTP_POST)
  ((PUT method/PUT) HTTP_PUT)
  ((CONNECT method/CONNECT) HTTP_CONNECT)
  ((OPTIONS method/OPTIONS) HTTP_OPTIONS)
  ((TRACE method/TRACE) HTTP_TRACE)
  ((COPY method/COPY) HTTP_COPY)
  ((LOCK method/LOCK) HTTP_LOCK)
  ((MKCOL method/MKCOL) HTTP_MKCOL)
  ((MOVE method/MOVE) HTTP_MOVE)
  ((PROPFIND method/PROPFIND) HTTP_PROPFIND)
  ((PROPPATCH method/PROPPATCH) HTTP_PROPPATCH)
  ((SEARCH method/SEARCH) HTTP_SEARCH)
  ((UNLOCK method/UNLOCK) HTTP_UNLOCK)
  ((BIND method/BIND) HTTP_BIND)
  ((REBIND method/REBIND) HTTP_REBIND)
  ((UNBIND method/UNBIND) HTTP_UNBIND)
  ((ACL method/ACL) HTTP_ACL)
  ((REPORT method/REPORT) HTTP_REPORT)
  ((MKACTIVITY method/MKACTIVITY) HTTP_MKACTIVITY)
  ((CHECKOUT method/CHECKOUT) HTTP_CHECKOUT)
  ((MERGE method/MERGE) HTTP_MERGE)
  ((MSEARCH method/MSEARCH) HTTP_MSEARCH)
  ((NOTIFY method/NOTIFY) HTTP_NOTIFY)
  ((SUBSCRIBE method/SUBSCRIBE) HTTP_SUBSCRIBE)
  ((UNSUBSCRIBE method/UNSUBSCRIBE) HTTP_UNSUBSCRIBE)
  ((PATCH method/PATCH) HTTP_PATCH)
  ((PURGE method/PURGE) HTTP_PURGE)
  ((MKCALENDAR method/MKCALENDAR) HTTP_MKCALENDAR))

(define PARSER_DATA_VECTOR_SIZE
  (foreign-value "PARSER_DATA_VECTOR_SIZE" int))

(define PAUSED
  (foreign-value "HPE_PAUSED" int))

(define make-settings
  (foreign-lambda c-pointer "make_settings"))

;; create settings object now
(define SETTINGS (make-settings))

(define make-parser
  (foreign-lambda c-pointer "make_parser" u16vector))

(define-record connection
  request
  parser
  parser-data
  parsed
  chunk
  chunk-length
  port)

(define-record-printer (connection x out)
  (fprintf out "#<connection ~A>" (connection-port x)))

(define (free-connection conn)
  (free-parser (connection-parser conn))
  (release-number-vector (connection-chunk conn))
  (release-number-vector (connection-parser-data conn)))

(define free-parser
  (foreign-lambda void "free_parser" (nonnull-c-pointer (struct http_parser))))

(define (http-connection port)
  ;; TODO: how big should this vector be?
  (let ((parser-data (make-u16vector PARSER_DATA_VECTOR_SIZE #f #t #f))
        (chunk (make-u8vector (http-chunk-size) #f #t #f)))
    (make-connection
     #f                        ;; request
     (make-parser parser-data) ;; parser
     parser-data               ;; parser-data
     #f                        ;; amount of data parsed
     chunk                     ;; chunk
     0                         ;; chunk-length
     port)))                   ;; port

;; reads a u8vector up to limit in size, only so long
;; as there is a char-ready? (after the initial read-char to pull data through)
(define (read-http-chunk p buffer limit)
  (##sys#check-input-port p #t 'read-http-chunk)
  (##sys#check-exact 0 'read-http-chunk)
  (##sys#check-structure buffer 'u8vector 'u8vector-substring)
  (when limit (##sys#check-exact limit 'read-http-chunk))
  (let ((dest (##sys#slot buffer 1)))
    (let loop ((i 0))
      (if (and limit (fx>= i limit))
          (values buffer i)
          (if (or ((##sys#slot (##sys#slot p 2) 6) p) ; char-ready?
                  (fx= i 0))
              (let ((c (##sys#read-char-0 p)))
                (if (eof-object? c)
                    (values buffer i)
                    (begin
                      (##core#inline "C_setsubchar" dest i c)
                      (loop (fx+ i 1)))))
              (values buffer i))))))

;; copies unparsed data from end of chunk to beginning of chunk
;; and updates the chunk length
(define (shift-parsed conn)
  (let ((start (connection-parsed conn))
        (end (connection-chunk-length conn))
        (chunk (connection-chunk conn)))
    (let loop ((i1 0) (i2 start))
      (if (fx< i2 end)
          (begin
            (u8vector-set! chunk i1 (u8vector-ref chunk i2))
            (loop (fx+ i1 1) (fx+ i2 1)))
          (connection-chunk-length-set! conn i1)))
    (connection-parsed-set! conn #f)))

(define (connection-read-chunk conn)
  (let ((parsed (connection-parsed conn)))
    (if (and parsed (< parsed (connection-chunk-length conn)))
        (begin
          (shift-parsed conn)
          (values (connection-chunk conn)
                  (connection-chunk-length conn)))
        (receive (buf len)
            (read-http-chunk (connection-port conn)
                             (connection-chunk conn)
                             (http-chunk-size))
          (connection-chunk-length-set! conn len)
          (values buf len)))))

(define parser-error
  (foreign-lambda* unsigned-int
    (((const (nonnull-c-pointer (struct http_parser))) parser))
    "C_return(parser->http_errno);"))

(define error-description
  (foreign-lambda* c-string ((int errno))
    "C_return(http_errno_description(errno));"))

(define parser-pause
  (foreign-lambda void "http_parser_pause"
    (nonnull-c-pointer (struct http_parser))
    int))

(define MESSAGE_BEGIN (foreign-value "MESSAGE_BEGIN" int))
(define URL (foreign-value "URL" int))
(define HEADER_FIELD (foreign-value "HEADER_FIELD" int))
(define HEADER_VALUE (foreign-value "HEADER_VALUE" int))
(define HEADERS_COMPLETE (foreign-value "HEADERS_COMPLETE" int))
(define BODY (foreign-value "BODY" int))
(define MESSAGE_COMPLETE (foreign-value "MESSAGE_COMPLETE" int))

(define (u8vector-substring v start end)
  (##sys#check-structure v 'u8vector 'u8vector-substring)
  (##sys#check-exact start 'u8vector-substring)
  (##sys#check-exact end 'u8vector-substring)
  (let* ((src (##sys#slot v 1))
         (len (##sys#size src)))
    (if (and (fx<= start end)
             (fx>= start 0)
             (fx<= end len))
        (##sys#substring src start end)
        (##sys#error-hook
         (foreign-value "C_OUT_OF_RANGE_ERROR" int)
         'u8vector-substring start end))))

(define (read-part-string buf parser-data i)
  (let* ((start (u16vector-ref parser-data (fx+ i 1)))
         (end (u16vector-ref parser-data (fx+ i 2))))
    (u8vector-substring buf start end)))

(define (on-message-begin conn)
  (connection-request-set! conn (make-request)))

(define (on-url conn value)
  (let* ((req (connection-request conn))
         (prev-url (request-url req)))
    (if prev-url
        (request-url-set! req (string-append/shared prev-url value))
        (request-url-set! req value))))

(define (on-header-field conn value)
  (let* ((req (connection-request conn))
         (hs (request-headers req)))
    (if (and (not (null? hs)) (string? (car hs)))
        (set-car! hs (string-append (car hs) value))
        (request-headers-set! req (cons value hs)))))

(define (on-header-value conn value)
  (let* ((req (connection-request conn))
         (hs (request-headers req))
         (field (car hs)))
    (if (string? field)
        (request-headers-set! req (cons (cons field value) (cdr hs)))
        (set-cdr! field (string-append (cdr field) value)))))

(define (on-headers-complete conn method keep-alive)
  (let ((req (connection-request conn)))
    (request-headers-set!
     req
     (map (lambda (pair)
            (cons (string->symbol (string-downcase (car pair)))
                  (cdr pair)))
          (request-headers req)))
    (request-method-set! req method)
    (request-keep-alive-set! req keep-alive)
    (request-port-set! req (make-body-port conn req))
    (request-headers-complete-set! req #t)))

(define (on-body conn start end)
  (let ((req (connection-request conn)))
    (request-body-index-set! req start)
    (request-body-end-set! req end)))

(define (on-message-complete conn)
  (let ((req (connection-request conn)))
    (request-message-complete-set! req #t)))

(define (read-part conn buf parser-data i)
  (let ((x (u16vector-ref parser-data i)))
    (cond
     ((fx= x MESSAGE_BEGIN)
      (on-message-begin conn)
      (fx+ i 1))
     ((fx= x URL)
      (on-url conn (read-part-string buf parser-data i))
      (fx+ i 3))
     ((fx= x HEADER_FIELD)
      (on-header-field conn (read-part-string buf parser-data i))
      (fx+ i 3))
     ((fx= x HEADER_VALUE)
      (on-header-value conn (read-part-string buf parser-data i))
      (fx+ i 3))
     ((fx= x HEADERS_COMPLETE)
      (on-headers-complete
       conn
       (int->method (u16vector-ref parser-data (fx+ i 1)))
       (not (= 0 (u16vector-ref parser-data (fx+ i 2)))))
      (fx+ i 3))
     ((fx= x BODY)
      (on-body
       conn
       (u16vector-ref parser-data (fx+ i 1))
       (u16vector-ref parser-data (fx+ i 2)))
      (fx+ i 3))
     ((fx= x MESSAGE_COMPLETE)
      (on-message-complete conn)
      #f)
     (else #f))))

(define (split-input conn buf parser-data)
  (let loop ((i 0))
    (when (and i (fx< i PARSER_DATA_VECTOR_SIZE))
      (loop (read-part conn buf parser-data i)))))

(define parse
  (foreign-lambda size_t "custom_parser_execute"
    (c-pointer "http_parser")
    (c-pointer "http_parser_settings")
    u8vector
    size_t))

(define (parser-execute conn buf len)
  (let* ((parser (connection-parser conn))
         (parser-data (connection-parser-data conn))
         (parsed (parse parser SETTINGS buf len)))
    (when (< parsed len)
      (let ((errno (parser-error parser)))
        (if (fx= PAUSED errno)
            (begin
              (connection-parsed-set! conn parsed)
              (parser-pause parser 0))
            (abort (error-description errno)))))
    (split-input conn buf parser-data)
    parsed))

(define (read-headers conn)
  (call/cc
    (lambda (k)
      (until (and-let* ((req (connection-request conn)))
               (request-headers-complete req))
             (receive (buf len) (connection-read-chunk conn)
               (if (fx= 0 len)
                 (k #f)
                 (parser-execute conn buf len)))))))

(define (skip-body conn)
  (call/cc
    (lambda (k)
      (let ((req (connection-request conn)))
        (until (request-message-complete req)
               (receive (buf len) (connection-read-chunk conn)
                 (if (fx= 0 len)
                   (k #f)
                   (parser-execute conn buf len))))))))

(define (make-body-port conn req)
  (let ((chunk (connection-chunk conn)))
    (make-input-port
     (lambda () ;; read-char
       (let start ()
         (if (>= (request-body-index req) (request-body-end req))
             (if (request-message-complete req)
                 #!eof
                 (receive (buf len) (connection-read-chunk conn)
                   (if (fx= 0 len)
                       #!eof
                       (let ((parsed (parser-execute conn buf len)))
                         (set! chunk buf)
                         (start)))))
             (let ((index (request-body-index req)))
               (begin0
                (integer->char (u8vector-ref chunk index))
                (request-body-index-set! req (fx+ index 1)))))))
     (lambda () ;; char-ready?
       (or (< (request-body-index req) (fx- (request-body-end req) 1))
           (char-ready? (connection-port conn))))
     (lambda () ;; close
       (close-input-port (connection-port conn)))
     #f ;; peek-char
     #f ;; read-string! TODO: add implementation for better performance
     #f ;; read-line
     )))

(define (read-request conn)
  (let ((req (connection-request conn)))
    (when req
      (when (not (request-message-complete req))
        ;; TODO: should we actually close the connection here?
        (skip-body conn))
      (connection-request-set! conn #f))
    (and (read-headers conn)
         (connection-request conn))))
