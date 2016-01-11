(defstruct request
  method
  url
  (headers '())
  keep-alive
  headers-complete
  message-complete
  (body-index 0) ;; index of current connection chunk where body should read from
  (body-end -1)  ;; index of current connection chunk where body chunk ends
  port)

(define-record-printer (request x out)
  (fprintf out "#<request ~S ~S ~S>"
           (request-method x)
           (request-url x)
           (request-headers x)))
