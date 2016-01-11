(module snowy

;; exports:

(;; server
 http-listen
 max-connections
 server-port
 server-bind-address

 ;; response
 make-response
 response-port
 response-code
 response-code-set!
 response-reason
 response-reason-set!
 response-headers
 response-headers-set!
 ;response-headers-sent
 response-headers-ref
 response-body
 response-body-set!
 response-keep-alive
 response-headers-merge
 response-write-head
 response-write-body
 response-write
 update-response
 status-codes
 merge-headers

 ;; request
 make-request
 request-method
 request-url
 request-headers
 request-headers-complete
 request-message-complete
 request-keep-alive
 request-port
 )

(import chicken scheme foreign)

(use data-structures
     extras
     posix
     tcp
     ports
     srfi-1
     srfi-4
     srfi-13
     srfi-18
     srfi-69
     lolevel
     miscmacros
     foreigners
     defstruct)

(include "http-parser.scm")
(include "request.scm")
(include "response.scm")
(include "server.scm")
(include "utils.scm")

)
