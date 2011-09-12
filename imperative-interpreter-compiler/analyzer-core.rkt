(load "env-ds.rkt")
(load "asp.rkt")
(load "utils.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  IMP-ENV-ANALYZER  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (derive-analyze-eval exp)
  ((analyze (derive exp)) the-global-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main imperative-environment-analyzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Type: <EXP>  --> (ENV --> VAL) (union of Number, Symbol, Boolean, Procedure, Pair, List)
;;; Pre-conditions: The given expression is legal according to the concrete syntax, Inner 'define' expressions are not legal.
(define (analyze exp)
    (cond ((atomic? exp) (analyze-atomic exp))
          ((special-form? exp) (analyze-special-form exp))
          ((application? exp) (analyze-application exp))
          (else
           (error "Unknown expression type -- EVAL" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-atomic exp)
  (if (or (number? exp) (boolean? exp) (null? exp))
      (lambda (env) exp)
      (lambda (env) (lookup-variable-value exp env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Special form handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (special-form? exp)
  (or (quoted? exp) (lambda? exp) (assignment? exp)
      (definition? exp) (if? exp) (begin? exp) ))

(define (analyze-special-form exp)
  (cond ((quoted? exp) (analyze-quoted exp))
        ((lambda? exp) (analyze-lambda exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((begin? exp) (analyze-begin exp))
        ))

(define (analyze-quoted exp)
  (let ((text (text-of-quotation exp)))
    (lambda (env)
      text)))

(define (analyze-lambda exp)
  (let ((parameters (lambda-parameters exp))
        (body (analyze-sequence (lambda-body exp))))
    (lambda (env)
      (make-procedure parameters body env))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (val (analyze (assignment-value exp))))
    (lambda (env)
      (set-binding-in-env! var (val env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (val (analyze (definition-value exp))))
    (lambda (env)
      (if (not (eq? env the-global-environment))
          (error "Non global definition" exp)
          (begin (add-binding! (make-binding var (val the-global-environment)))
                 'ok)))))

(define (analyze-if exp)
  (let ((pred (analyze (if-predicate exp)))
        (consequent (analyze (if-consequent exp)))
        (alternative (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pred env))
          (consequent env)
          (alternative env)))))

(define (analyze-begin exp)
  (let ((actions (analyze-sequence (begin-actions exp))))
    (lambda (env)
      (actions env))))

(define (analyze-sequence exps)
  (let ((procs (map analyze exps))
        (last-in-list (lambda (lst) (car (reverse lst)))))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env)
      (let ((vals (map (lambda (proc)(proc env)) procs)))
        (last-in-list vals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Application handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-application exp) 
  (let ((application-operator (analyze (operator exp)))
        (application-operands (map analyze (operands exp))))
    (lambda (env)
      (apply-procedure (application-operator env)
                       (map (lambda (operand) (operand env)) application-operands)))))

(define (apply-procedure procedure arguments) 
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (let ((parameters (procedure-parameters procedure)))
           (if (make-frame-precondition parameters arguments)
               ((procedure-body procedure)
                (extend-env (make-frame parameters arguments)
                            (procedure-environment procedure)))
               (error 
                "make-frame-precondition violation: # of variables does not match # of values while attempting to create a frame"))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;;; The primitive procedures will be captured as data structures of
;;; the evaluator. Therefore, their implementation should be
;;; retrieved from these objects.
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

