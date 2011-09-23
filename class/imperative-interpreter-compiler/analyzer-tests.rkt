#lang racket

(require "analyzer-core.rkt" "../asp.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  TESTS  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive procedures tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (primitive-tests)
  (test (derive-analyze-eval '(* 3 4)) => 12)
  (test (derive-analyze-eval '(+ 3 4)) => 7)
  (test (derive-analyze-eval '(- 3 4)) => -1)
  (test (derive-analyze-eval '(/ 4 2)) => 2)
  (test (derive-analyze-eval '(null? ())) => #t)
  (test (derive-analyze-eval '(> 3 4)) => #f)
  (test (derive-analyze-eval '(< 3 4)) => #t)
  (test (derive-analyze-eval '(= 3 4)) => #f)
  (test (derive-analyze-eval '(car (list 3 4))) => 3)
  (test (derive-analyze-eval '(cdr (list 3 4))) => '(4))
  (test (derive-analyze-eval '(cons 3 (cons 4 ()))) => '(3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application and `lambda' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (app-lambda-tests)
  (test (derive-analyze-eval '((lambda (x) x) 12)) => 12)
  (test (derive-analyze-eval '((lambda (x y z) (+ x y z)) 12 13 14)) => 39)
  (test (derive-analyze-eval '((lambda (x) ((lambda (x) (+ x 1)) 2)) 12)) => 3)
  (test (derive-analyze-eval '((lambda (f x y) (f x y)) + 12 4)) => 16)
  (test (derive-analyze-eval '((lambda (f x y) (f x y)) ((lambda () +)) 12 4)) => 16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `begin' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin-tests)
  (test (derive-analyze-eval '(begin 1 2 3)) => 3)
  (test (derive-analyze-eval '(begin 1 2 ((lambda () 5)))) => 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `let' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (let-tests)
  (test (derive-analyze-eval '(let ((x 1) (y 2)) (+ x y))) => 3)
  (test (derive-analyze-eval '(let ((x 1) (y 2)) (+ x y ((lambda (x) (* 2 x)) 10)))) => 23)
  (test (derive-analyze-eval '(let ((x ((lambda () 5)))) x)) => 5)
  (test (derive-analyze-eval '(let ((f (lambda (x) (+ x x)))) (f 4))) => 8)
  (test (derive-analyze-eval '(let ((f (lambda (x) (+ x x))) (g (lambda (x y) (* x y))) (h (lambda () 2))) (f (g 3 (h))))) => 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition and function-definition tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (definition-tests)
  (test (derive-analyze-eval '(define x 2)) => 'ok)
  (test (derive-analyze-eval '(define (f x) (+ x x))) => 'ok)
  (test (derive-analyze-eval 'x) => 2)
  (test (derive-analyze-eval '(f x)) => 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `if' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (if-tests)
  (test (derive-analyze-eval '(if (> 3 2) 5 1)) => 5)
  (test (derive-analyze-eval '(if (< 3 2) 5 1)) => 1)
  (test (derive-analyze-eval '(if (> (+ 1 2) 2) (+ 1 2 3) 1)) => 6)
  (test (derive-analyze-eval '(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))) => 'ok)
  (test (derive-analyze-eval '(f 5)) => 120))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `cond' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cond-tests)
  (test (derive-analyze-eval '(cond ((> 3 2) 5) (else 1))) => 5)
  (test (derive-analyze-eval '(cond ((< 3 2) 5) (else 1))) => 1)
  (test (derive-analyze-eval '(let ((x 3)) (cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100)))) => 30)
  (test (derive-analyze-eval '(let ((x 10)) (cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100)))) => 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `set!' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set!-tests)
  (test (derive-analyze-eval '(let ((x 1)) (set! x 5) x)) => 5)
  (test (derive-analyze-eval '(let ((x 1) (y 3)) (set! x (+ y x)) x)) => 4)
  (test (derive-analyze-eval '(let ((x 1)) ((lambda (x) (set! x 5)) x) x)) => 1)
  (test (derive-analyze-eval '(let ((x 1)) ((lambda (x) (set! x 5) x) x))) => 5)
  (test (derive-analyze-eval '(let ((x 1)) ((lambda (y) (set! x (* y y))) 4) x)) => 16)
  (test (derive-analyze-eval '(define f (let ((x 0)) (lambda () (set! x (+ x 1)) x)))) => 'ok)
  (test (derive-analyze-eval '(f)) => 1)
  (test (derive-analyze-eval '(f)) => 2)
  (test (derive-analyze-eval '(begin (f) (f) (f) (f))) => 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Y-combinator tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (y-comb-tests)
  (test (derive-analyze-eval '(( (lambda (f) (f f))
                                 (lambda (fact)
                                   (lambda (n)
                                     (if (= n 0)
                                         1
                                         (* n ((fact fact) (- n 1)))))))
                               6))
        => 720))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests
 (primitive-tests)
 (app-lambda-tests)
 (begin-tests)
 (let-tests)
 (definition-tests)
 (if-tests)
 (cond-tests)
 (set!-tests)
 (y-comb-tests))
