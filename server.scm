;;; Configuration
(define max-connections     (make-parameter 1024))
(define snowy-buffer-size   (make-parameter 1024))
(define server-port         (make-parameter 8080))
(define server-bind-address (make-parameter #f))


(define (handle-another-request? req res in out)
  (and (request-keep-alive req)
       (response-keep-alive res)
       (not (port-closed? in))
       (not (port-closed? out))))

(define (close-connection in out)
  (close-input-port in)
  (close-output-port out))

(define (read-request* conn in out)
  (condition-case
    (parameterize
      ;; Apache 2.2+ waits 5 secs for keep-alive
      ((tcp-read-timeout 5000))
      (read-request conn))
    ;; don't report error on read timeout, just close connection
    ((exn i/o net timeout)
     #f)))

(define (connection-loop conn in out remote local f)
  (let loop ((req (read-request* conn in out)))
    (when req ;; connection wasn't closed (otherwise req is #f)
      ;; handle request
      (let ((res (response-write (f req (make-response port: out)))))
        (unless (port-closed? out)
          (flush-output out))
        (when (handle-another-request? req res in out)
          (loop (read-request* conn in out))))))
  (close-connection in out)
  (free-connection conn))

(define (print-thread-error e #!optional (header "Error"))
  (let* ((chain (call-with-output-string print-call-chain))
         (port (current-error-port))
         (tname (thread-name (current-thread)))
         (headstr (sprintf (conc "~A: " header) tname)))
    (print-error-message e port headstr)
    (display chain port)))

(define (accept-loop listener request-handler)
  (let* ((thread-count (make-mutex/value 'thread-count 0))
         (thread-stopped! (make-condition-variable 'thread-stopped!))
         (cleanup-thread (lambda ()
                           (mutex-update! thread-count sub1)
                           ;; Wake up the accepting thread if it's asleep
                           (condition-variable-signal! thread-stopped!))))
    (let loop ()
      ;; Wait until we have a free connection slot
      (mutex-wait! thread-count
                   (lambda (count) (< count (max-connections)))
                   thread-stopped!)
      ;; TODO: leave this exception handling to embedding applications?
      (condition-case
        (let*-values (((in out) (tcp-accept listener))
                      ((local remote) (tcp-addresses in)))
          (mutex-update! thread-count add1)
          (thread-start!
            (make-thread
              (lambda ()
                (let ((conn (http-connection in)))
                  (handle-exceptions
                    e (begin
                        (print-thread-error e)
                        ;; make sure we close the connection
                        (close-connection in out)
                        (free-connection conn)
                        #f)
                    (connection-loop conn in out local remote request-handler))
                  (cleanup-thread))))))
        (e (exn i/o net)
           (print-thread-error e "Connection handshake error")))
      (loop))))

(define (http-listen request-handler #!key (port (server-port))
                             (bind-address (server-bind-address)))
  (let ((listener (tcp-listen port 100 bind-address)))
    (parameterize ((server-port port)
                   (server-bind-address bind-address)
                   (tcp-buffer-size (snowy-buffer-size)))
      (accept-loop listener request-handler))))
