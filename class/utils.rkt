#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  ERRORS  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This defines a new `error', which throws a new kind of exception.
;; This way, it is possible to distinguish errors raised by student code
;; from errors raised by Racket.  See the `test' below: it is
;; intentionally catching only these kind of errors.

;; The new exception type, a subtype of racket errors
(define-struct (exn:fail:ppl exn:fail) ())

;; Define `error' now -- as a macro.  This is not really needed, but if
;; it's defined as a function, then errors will have this function as
;; the topmost call in the backtrace.  Defining it as a macro avoids
;; this, and make the error look like it comes straight from user code.
;; (Note that `syntax/loc' makes the macro result appear as if it's
;; coming from user code.)
(define-syntax (error stx)
  (syntax-case stx (set!)
    [(set! x . _)
     ;; (set! error ...) is forbidden
     (raise-syntax-error 'set! "cannot mutate module-required identifier"
                         stx #'x)]
    ;; plain (error ...) uses
    [(_ sym fmt arg ...)
     (syntax/loc stx
       (raise (make-exn:fail:ppl
               (format "~a: ~a" sym (format fmt arg ...))
               (current-continuation-marks))))]
    ;; broken (error ...) uses: use the plain function value, so it will
    ;; throw the expected kind of errors (this is a problem of misusing
    ;; `error' itself, so those would be the usual kind of errors.)
    [(_ sym . xs) (syntax/loc stx (error-as-value sym . xs))]
    ;; finally, uses of `error' as a value
    [_ (syntax/loc stx error-as-value)]))
;; This is a definition of an error function that can be used as a
;; value, when `error' is used as an expression, rather than as a
;; function call.  (Such uses could be made into a syntax error, but
;; it's better to stick to a uniform language, where almost everything
;; has a value.)  Note that it actually uses the above macro in its
;; definition, but this use is in a call, so there is no infinite loop
;; problem.  Also, the `let' is not really needed, but it makes the
;; resulting function look like `#<procedure:error>' instead of
;; `#<procedure:error-as-value>'...
(define error-as-value
  (let ([error (lambda (sym fmt . args)
                 (error sym "~a" (apply format fmt args)))])
    error))

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
