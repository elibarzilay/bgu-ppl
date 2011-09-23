#lang racket

(require "analyzer-core.rkt" "../asp.rkt")

(define test
  (lambda (test-lst)
    (apply-test-list derive-analyze-eval test-lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Primitive procedures tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (primitive-tests)
  (let ((test-out (list '((* 3 4) 12)
                        '((+ 3 4) 7)
                        '((- 3 4) -1)
                        '((/ 4 2) 2)
                        '((null? ()) #t)
                        '((> 3 4) #f)
                        '((< 3 4) #t)
                        '((= 3 4) #f)
                        '((car (list 3 4)) 3)
                        '((cdr (list 3 4)) (4))
                        '((cons 3 (cons 4 ())) (3 4))
                        )))
    (test test-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Application and lambda tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (app-lambda-tests)
  (let ((test-out (list '(((lambda (x) x) 12) 12)
                        '(((lambda (x y z) (+ x y z)) 12 13 14) 39)
                        '(((lambda (x) ((lambda (x) (+ x 1)) 2)) 12) 3)
                        '(((lambda (f x y) (f x y)) + 12 4) 16)
                        '(((lambda (f x y) (f x y)) ((lambda () +)) 12 4) 16)
                        )))
    (test test-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; begin tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin-tests)
  (let ((test-out (list '((begin 1 2 3) 3)
                        '((begin 1 2 ((lambda () 5))) 5)
                        )))
    (test test-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (let-tests)
  (let ((test-out (list '((let ((x 1) (y 2)) (+ x y)) 3)
                        '((let ((x 1) (y 2)) (+ x y ((lambda (x) (* 2 x)) 10))) 23)
                        '((let ((x ((lambda () 5)))) x) 5)
                        '((let ((f (lambda (x) (+ x x)))) (f 4)) 8)
                        '((let ((f (lambda (x) (+ x x))) (g (lambda (x y) (* x y))) (h (lambda () 2))) (f (g 3 (h)))) 12)
                        )))
    (test test-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; definition and function-definition tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (definition-tests)
  (let ((test-out (list '((define x 2) ok)
                        '((define (f x) (+ x x)) ok)
                        '(x 2)
                        '((f x) 4)
                        )))
    (test test-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (if-tests)
  (let ((test-out (list '((if (> 3 2) 5 1) 5)
                        '((if (< 3 2) 5 1) 1)
                        '((if (> (+ 1 2) 2) (+ 1 2 3) 1) 6)
                        '((define (f n) (if (= n 0) 1 (* n (f (- n 1))))) ok)
                        '((f 5) 120)
                        )))
    (test test-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Cond tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cond-tests)
  (let ((test-out (list '((cond ((> 3 2) 5) (else 1)) 5)
                        '((cond ((< 3 2) 5) (else 1)) 1)
                        '((let ((x 3)) (cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100))) 30)
                        '((let ((x 10)) (cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100))) 100)
                        )))
    (test test-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; y-combinator tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (y-comb-tests)
  (let ((test-out (list '((( (lambda (f) (f f))
                             (lambda (fact)
                               (lambda (n)
                                 (if (= n 0)
                                     1
                                     (* n ((fact fact) (- n 1)))))))
                           6) 720)
                        )))
    (test test-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(primitive-tests)
(app-lambda-tests)
(begin-tests)
(let-tests)
(definition-tests)
(if-tests)
(cond-tests)
(y-comb-tests)
