(load "asp.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;  Data structures  ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procedure representation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Type: [T --> PAIR(Symbol,T)]
(define make-primitive-procedure
  (lambda (proc)
    (attach-tag (list proc) 'primitive)))

;Type: [T -> Boolean]
(define primitive-procedure?
  (lambda (proc)
    (tagged-list? proc 'primitive)))

;Type: [PAIR(Symbol,T) -> T]
(define primitive-implementation
  (lambda (proc)
    (car (get-content proc))))

;Type: [LIST(Symbol)*LIST*Env -> LIST]
(define make-procedure
  (lambda (parameters body env)
    (attach-tag (list parameters body env) 'procedure)))

;Type: [T -> Boolean]
(define compound-procedure?
  (lambda (p)
    (tagged-list? p 'procedure)))

;Type: [LIST -> LIST(Symbol)]
(define procedure-parameters
  (lambda (p)
    (car (get-content p))))

;Type: [LIST -> LIST]
(define procedure-body
  (lambda (p)
    (cadr (get-content p))))

;Type: [LIST -> Env]
(define procedure-environment
  (lambda (p)
    (caddr (get-content p))))

;Type: [LIST -> LIST]
;Purpose:  An identification predicate for procedures -- closures and primitive:
(define procedure?
  (lambda (p)
    (or (primitive-procedure? p) (compound-procedure? p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Frames 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Post-conditions: produces a pair of <variables,values> or error if (#vars != #vals)
(define (make-frame variables values)
  (if (null? variables)
      (list)
      (cons variables values)))

(define (make-frame-precondition vars vals)
  (= (length vars) (length vals)))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (first-var-in-frame frame) (car (frame-variables frame)))
(define (first-val-in-frame frame) (car (frame-values frame)))
(define (rest-vars-in-frame frame) (cdr (frame-variables frame)))
(define (rest-vals-in-frame frame) (cdr (frame-values frame)))
(define empty-frame? null?)

(define (add-binding-to-frame! binding frame)
  (let ((var (binding-variable binding))
        (val (binding-value binding)))
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame)))))

(define (lookup-variable-value-in-frame var frame) 
  (cond ((or (empty-frame? frame)
             (eq? var (first-var-in-frame frame))) frame) 
        (else (lookup-variable-value-in-frame var (make-frame (rest-vars-in-frame frame)
                                                              (rest-vals-in-frame frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bindings 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-binding var val)
  (cons var val))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enclosing-env env) (cdr env)) 
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (empty-env? env)
  (eq? env the-empty-environment))

(define (extend-env frame base-env)
  (cons frame base-env))

; TYPE: var*env->frame
(define (defined-in-env var env)
  (if (empty-env? env)
      env
      (let ((f (lookup-variable-value-in-frame var (first-frame env))))
        (if (empty-frame? f)  
            (defined-in-env var (enclosing-env env))
            f))))

(define (lookup-variable-value var env) 
  (let ((f (defined-in-env var env)))
    (if (empty-frame? f)
        (begin
          (display var)(newline)
          (display env)(newline)
          (error "Variable not found -- LOOKUP")
          )
        (first-val-in-frame f))))

(define (add-binding! binding)
  (add-binding-to-frame! binding (first-frame the-global-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  Global environment construction  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The global environment ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-global-environment 
  (let ((primitive-procedures
         (list (list 'car car)
               (list 'cdr cdr)
               (list 'cons cons)
               (list 'null? null?)
               (list '+ +)
               (list '* *)
               (list '/ /)
               (list '> >)
               (list '< <)
               (list '- -)
               (list '= =)
               (list 'list list)
               )))
    (extend-env (make-frame (map car primitive-procedures)
                            (map (lambda (x) (make-primitive-procedure (cadr x))) 
                                 primitive-procedures))
                the-empty-environment)))
