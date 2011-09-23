#!/bin/sh
#|
exec racket -um "$0" "$@"; exit

  >>> To use this script, just run it with a file to test as a
  >>> command-line argument.

|#

#lang racket

;; Some sample customizations (could be added as command-line flags)
(define show-stdout? #f)
(define show-stderr? #t)
(define additional-tests
  ;; note: these are Racket expressions to execute, not evaluator expressions
  '((test (not (even? (+ 1 2))))
    (test (+ 1 2) => 3)
    (test (+ 1 2) => 5)))

(require racket/sandbox racket/runtime-path)

(define-runtime-path code-dir "../class")

(define (run filename [show-filename? #t])
  (if (not (file-exists? filename))
    (printf "Missing file: \"~a\", skipping.\n" filename)
    (parameterize ([sandbox-path-permissions `([read ,code-dir])]
                   [sandbox-output
                    (and show-stdout? (current-output-port))]
                   [sandbox-error-output
                    (and show-stderr? (current-error-port))])
      (when show-filename? (printf "Running \"~a\"...\n" filename))
      (define e (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
                  (call-with-input-file filename
                    make-module-evaluator)))
      (if e
        (when (pair? additional-tests)
          (let ([bad (for/fold ([bad 0]) ([expr additional-tests])
                       (+ bad (with-handlers ([(lambda (_) #t) (lambda (_) 1)])
                                (e expr)
                                0)))])
            (if (zero? bad)
              (printf "-> All additional tests passed.\n")
              (printf "-> ~a/~a additional test failed.\n"
                      bad (length additional-tests)))
            (kill-evaluator e)))
        (printf "Errors in the source code!\n")))))

(provide main)
(define (main . args)
  (case (length args)
    [(0) (error 'sandbox-run "expecting file name(s) to run")]
    [(1) (run (car args) #f)]
    [else (for-each run args)]))
