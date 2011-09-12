(load "ASP.scm")
(load "env DS.scm")
(load "utils.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  FUNC-ENV-EVALUATOR  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define derive-eval
  (lambda (exp)
    (env-eval (derive exp) the-global-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main functional-environment-evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Type: <EXP> * ENV --> VAL (union of Number, Symbol, Boolean, Procedure, Pair, List)
;;; Pre-conditions: The given expression is legal according to the concrete syntax, Inner 'define' expressions are not legal.
(define env-eval
  (lambda (exp env)                                  
    (cond ((atomic? exp) (eval-atomic exp env))
          ((special-form? exp) (eval-special-form exp env))
          ((application? exp)
           (apply-procedure (env-eval (operator exp) env)
                            (list-of-values (operands exp) env)))
          (else
           (error "Unknown expression type -- EVAL" exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-atomic 
  (lambda (exp env)
    (if (or (number? exp) (boolean? exp) (null? exp))
        exp
        (lookup-variable-value exp env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Special form handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define special-form? 
  (lambda (exp)
    (or (quoted? exp) (lambda? exp) (definition? exp) 
        (if? exp) (begin? exp) )))

(define eval-special-form 
  (lambda (exp env)
    (cond ((quoted? exp) (text-of-quotation exp))
          ((lambda? exp) (eval-lambda exp env))
          ((definition? exp) 
           (if (not (eq? env the-global-environment))
               (error "Non global definition" exp)
               (eval-definition exp)))
          ((if? exp) (eval-if exp env))
          ((begin? exp) (eval-begin exp env))
          )))

(define eval-lambda 
  (lambda (exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env)))

(define eval-definition 
  (lambda (exp) 
    (add-binding! (make-binding (definition-variable exp)
                                (env-eval (definition-value exp) the-global-environment)))
    'ok))

(define eval-if
  (lambda (exp env)
    (if (true? (env-eval (if-predicate exp) env))
        (env-eval (if-consequent exp) env)
        (env-eval (if-alternative exp) env))))

(define eval-begin 
  (lambda (exp env)
    (eval-sequence (begin-actions exp) env)))

(define eval-sequence 
  (lambda (exps env)
    (cond ((sequence-last-exp? exps) (env-eval (sequence-first-exp exps) env))
          (else (env-eval (sequence-first-exp exps) env)
                (eval-sequence (sequence-rest-exps exps) env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Application handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apply-procedure 
  (lambda (procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (let ((parameters (procedure-parameters procedure)))
             (if (make-frame-precondition parameters arguments)
                 (eval-sequence
                  (procedure-body procedure)
                  (extend-env
                   (make-frame parameters arguments)
                   (procedure-environment procedure)))
                 (error 
                  "make-frame-precondition violation: # of variables does not match # of values while attempting to create a frame"))))
          (else
           (error
            "Unknown procedure type -- APPLY" procedure)))))

(define list-of-values 
  (lambda (exps env)
    (if (no-operands? exps)
        '()
        (cons (env-eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env)))))

;;; The primitive procedures will be captured as data structures of
;;; the evaluator. Therefore, their implementation should be
;;; retrieved from these objects.
(define apply-primitive-procedure 
  (lambda (proc args)
    (apply (primitive-implementation proc) args)))

(define true? 
  (lambda (x)
    (not (eq? x #f))))

(define false? 
  (lambda (x)
    (eq? x #f)))

