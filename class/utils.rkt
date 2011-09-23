#lang racket

(provide (all-defined-out))

(define (populate-list value length)
  (make-list length value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  TESTS  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-test-list eval-proc lst)
  (let loop ([lst lst])
    (unless (null? lst)
      (let* ([tuple        (car lst)]
             [to-test      (car tuple)]
             [expected-res (cadr tuple)]
             [res          (eval-proc to-test)])
        (unless (equal? res expected-res)
          (error 'test-failure
                 "when testing ~s\n  expected: ~s\n   but got: ~s"
                 to-test expected-res res)))
      (loop (cdr lst)))))
