;; Procedures in this file taken directly from Intarweb or Spiffy
;;
;; Intarweb
;;   Copyright (c) 2008-2015, Peter Bex
;;   License: BSD
;;
;; Spiffy
;;   Copyright (c) 2007-2014, Peter Bex
;;   Copyright (c) 2000-2005, Felix L. Winkelmann
;;   License: BSD


;;;;;; <from intarweb> ;;;;;;

(define-constant short-weekdays `#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define-constant short-months `#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (rfc1123-time->string time)
  (let ((padded (lambda (n)
                  (if (fx< n 10)
                      (string-append "0" (number->string n))
                      (number->string n))))
        (secs (vector-ref time 0))
        (mins (vector-ref time 1))
        (hours (vector-ref time 2))
        (mday (vector-ref time 3))
        (month (vector-ref time 4))
        (year (vector-ref time 5))
        (wday (vector-ref time 6)))
    (string-append (vector-ref short-weekdays wday) ", "
                   (padded mday) " " (vector-ref short-months month) " "
                   (number->string (+ 1900 year)) " " (padded hours) ":"
                   (padded mins) ":" (padded secs) " GMT")))

;;;;;; </from intarweb> ;;;;;;


;;;;;; <from spiffy>;;;;;;

(define (mutex-update! m op)
  (dynamic-wind
      (lambda () (mutex-lock! m))
      (lambda () (mutex-specific-set! m (op (mutex-specific m))))
      (lambda () (mutex-unlock! m))))

(define (make-mutex/value name value)
  (let ((m (make-mutex name)))
    (mutex-specific-set! m value)
    m))

;; Check whether the mutex has the correct state. If not, wait for a condition
;; and try again
(define (mutex-wait! m ok? condition)
  (let retry ()
    (mutex-lock! m)
    (if (ok? (mutex-specific m))
        (mutex-unlock! m)
        (begin (mutex-unlock! m condition) (retry)))))

;;;;;; </from spiffy>;;;;;;
