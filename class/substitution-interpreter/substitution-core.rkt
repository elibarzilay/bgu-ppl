#lang racket

(require "../asp.rkt" "ge-adt.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  SUBSTITUTION-EVALUATOR  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define derive-eval
  (lambda (exp)
    (applicative-eval (derive exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main substitution-evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Type: <EXP> * ENV -> VAL (union of Number, Symbol, Boolean, Procedure, Pair, List)
;;; Pre-conditions: The given expression is legal according to the concrete syntax, Inner 'define' expressions are not legal.
(define applicative-eval
  (lambda (exp)
    (cond ((atomic? exp) (eval-atomic exp))
          ((special-form? exp) (eval-special-form exp))
          ((list-form? exp) (eval-list exp))
          ((value? exp) exp)
          ((application? exp)
           (let ((renamed-exp (rename exp)))
             (apply-procedure (applicative-eval (operator renamed-exp))
                              (list-of-values (operands renamed-exp)))))
          (else
           (error "Unknown expression type -- EVAL" exp)))))

(define list-of-values
  (lambda (exps)
    (if (no-operands? exps)
        (list)
        (cons (applicative-eval (first-operand exps))
              (list-of-values (rest-operands exps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-atomic
  (lambda (exp)
    (if (not (variable? exp))
        exp
        (lookup-variable-value exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Special form handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define special-form?
  (lambda (exp)
    (or (quoted? exp) (lambda? exp) (definition? exp)
        (if? exp) (begin? exp))))

(define eval-special-form
  (lambda (exp)
    (cond ((quoted? exp) (make-symbol exp))
          ((lambda? exp) (eval-lambda exp))
          ((definition? exp) (eval-definition exp))
          ((if? exp) (eval-if exp))
          ((begin? exp) (eval-begin exp))
          )))

(define eval-lambda
  (lambda (exp)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp))))


(define eval-definition
  (lambda (exp)
    (add-binding! (make-binding (definition-variable exp)
                                (applicative-eval (definition-value exp))))
    'ok))

(define eval-if
  (lambda (exp)
    (if (true? (applicative-eval (if-predicate exp)))
        (applicative-eval (if-consequent exp))
        (applicative-eval (if-alternative exp)))))

(define eval-begin
  (lambda (exp)
    (eval-sequence (begin-actions exp))))

(define eval-sequence
  (lambda (exps)
    (cond ((sequence-last-exp? exps) (applicative-eval (sequence-first-exp exps)))
          (else (applicative-eval (sequence-first-exp exps))
                (eval-sequence (sequence-rest-exps exps))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Value identification and List handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value?
  (lambda (val) (or (evaluator-symbol? val) (evaluator-list? val)
                    (primitive-procedure? val) (compound-procedure? val))))

;(define list-form?
;  (lambda (exp)
 ;   (if (list? exp)
 ;       (let ((tag (get-tag exp)))
 ;         (or (eq? tag 'cons) (eq? tag 'list) (eq? tag 'append)))
 ;       #f)))

(define list-form?
  (lambda (exp)
    (or (tagged-list? exp 'cons) (tagged-list? exp 'list)
        (tagged-list? exp 'append))))

(define eval-list
  (lambda (lst)
    (make-list (apply-primitive-procedure
                (applicative-eval (operator lst))
                (list-of-values (operands lst))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Application handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apply-procedure
  (lambda (procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (let ((body (rename (procedure-body procedure)))
                 (parameters (procedure-parameters procedure)))
             (eval-sequence
              (substitute body parameters arguments))))
          (else
           (error
            "Unknown procedure type -- APPLY" procedure)))))


;;; Retrieved the primitive implementation, and apply to args.
;;; For value args: Their content should be retrieved.
(define apply-primitive-procedure
  (lambda (proc args)
    (apply (primitive-implementation proc)
           (map (lambda (arg)
                  (cond ((evaluator-symbol? arg) (symbol-content arg))
                        ((evaluator-list? arg) (list-content arg))
                        ((primitive-procedure? arg) (primitive-implementation arg))
                        (else arg)))
                args))))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;       RENAMING PROCEDURE       ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Signature: rename(exp)
;;; Purpose: Consistently rename bound variables in 'exp'.
;;; Type: [T -> T]
(define rename
  (letrec ((make-new-names
            (lambda (old-names)
              (if (null? old-names)
                  (list)
                  (cons (gensym) (make-new-names (cdr old-names))))))
           (replace
            (lambda (val-exp)
              (cond ((or (evaluator-symbol? val-exp) (primitive-procedure? val-exp)) val-exp)
                    ((evaluator-list? val-exp)
                     (make-list (map rename (list-content val-exp))))
                    ((compound-procedure? val-exp)
                     (let* ((params (procedure-parameters val-exp))
                            (new-params (make-new-names params))
                            (renamed-subs-body (map rename (procedure-body val-exp)))
                            (renamed-body (substitute renamed-subs-body params new-params)))
                       (make-procedure new-params renamed-body)))))))
    (lambda (exp)
      (cond ((atomic? exp) exp)
            ((lambda? exp)
             (let* ((params (lambda-parameters exp))
                    (new-params (make-new-names params))
                    (renamed-subs (map rename exp)))
               (substitute renamed-subs params new-params)))  ; replaces free occurrences
            ((value? exp) (replace exp))
            (else (map rename exp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;       SUBSTITUTE PROCEDURES       ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Signature: substitute(exp vars vals)
;;; Purpose: Consistent replacement of all FREE occurrences of 'vars' in 'exp' by 'vals', respectively.
;;; Type: [T*LIST(Symbol)*LIST -> T]
;;; Pre-conditions: (1) substitute is not performed on 'define' or 'let' expressions
;;;                     or on expression containing such sub-expressions.
;;;                 (2) 'exp' is already renamed. Therefore, var has no bound occurrences in exp.
;;;                 (3) length(vars)=length(vals)
;;;
(define substitute
  (letrec ((substitute-var-val  ; Substitute one variable
            (lambda (exp var val)
              (cond ((variable? exp)
                     (if (eq? exp var)
                         val ; substitute free occurrence of var with val
                         exp))
                    ((or (number? exp) (boolean? exp) (quoted? exp)) exp)
                    ((value? exp) (substitute-var-val-in-value exp var val))
                    (else ; expression is a list of expressions, application, cond.
                     (map (lambda (e) (substitute-var-val e var val)) exp)))))
           (substitute-var-val-in-value
            (lambda (val-exp var val)
              (cond ((or (evaluator-symbol? val-exp) (primitive-procedure? val-exp)) val-exp)
                    ((evaluator-list? val-exp)
                     (make-list (map (lambda (e) (substitute-var-val e var val))
                                     (list-content val-exp))))
                    ((compound-procedure? val-exp)
                     (make-procedure (procedure-parameters val-exp)
                                     (map (lambda (e) (substitute-var-val e var val))
                                          (procedure-body val-exp))))))))
    (lambda (exp vars vals)
      (if (and (null? vars) (null? vals))
          exp
          (substitute (substitute-var-val exp (car vars) (car vals))
                      (cdr vars)
                      (cdr vals))))))
