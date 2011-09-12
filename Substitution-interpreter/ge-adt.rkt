(load "ASP.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data structures:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol:
(define make-symbol 
  (lambda (x) (attach-tag (list x) 'symbol )))
(define evaluator-symbol?
  (lambda (s) (tagged-list? s 'symbol)))
(define symbol-content
  (lambda (s) (car (get-content s))))


;;; List:
(define make-list
  (lambda (x) (attach-tag (list x) 'evaluator-list)))
(define evaluator-list?
  (lambda (s) (tagged-list? s 'evaluator-list)))
(define list-content
  (lambda (s) (car (get-content s)))) 

;;; Primitive procedure:
(define make-primitive-procedure 
  (lambda (proc)
    (attach-tag (list proc) 'primitive)))

(define primitive-implementation 
  (lambda (proc) 
    (car (get-content proc))))

(define primitive-procedure? 
  (lambda (proc)
    (tagged-list? proc 'primitive)))

;;;; Closure:
(define make-procedure 
  (lambda (parameters body)
    (attach-tag (cons parameters body) 'procedure)))

(define compound-procedure? 
  (lambda (p)
    (tagged-list? p 'procedure)))

(define procedure-parameters 
  (lambda (p) 
    (car (get-content p))))

(define procedure-body 
  (lambda (p) 
    (cdr (get-content p))))

; An identification predicate for procedures -- closures and primitive:
(define evaluator-procedure? 
  (lambda (p)
    (or (primitive-procedure? p) (compound-procedure? p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  Global environment ADT implementation  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; Construction;;;;;;;;;;;;;
(define make-the-global-environment 
  (lambda ()
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
                 (list 'append append)
                 ;;      more primitives
                 )))
      (list (cons (map car primitive-procedures)
                  (map (lambda (x) (make-primitive-procedure (cadr x)))
                       primitive-procedures))))))

(define the-global-environment (make-the-global-environment))


;;;;;;;;;;; selection: 

(define lookup-variable-value 
  (lambda (var)
    (letrec ((frame-variables (lambda (frame) (car frame)))
             (frame-values (lambda (frame) (cdr frame)))
             (first-var-in-frame (lambda (frame) (car (frame-variables frame))))
             (first-val-in-frame (lambda (frame) (car (frame-values frame))))
             (rest-vars-in-frame (lambda (frame) (cdr (frame-variables frame))))
             (rest-vals-in-frame (lambda (frame) (cdr (frame-values frame))))
             (make-frame (lambda (vars vals) (cons vars vals)))
             (lookup (lambda (var frame)
                       (cond ((or (null? (frame-variables frame))
                                  (null? (frame-values frame)))
                              (error "unbound variable" var))
                             ((eq? var (first-var-in-frame frame))
                              (first-val-in-frame frame))
                             (else (lookup var (make-frame (rest-vars-in-frame frame)
                                                           (rest-vals-in-frame frame))))))))
      (lookup var (car the-global-environment)))))


;;;;;;;;;;;;; Mutation:

(define add-binding! 
  (lambda (binding)
    (let ((var (binding-variable binding))
          (val (binding-value binding))
          (frame (car the-global-environment)))
      (set-car! frame (cons var (car frame)))
      (set-cdr! frame (cons val (cdr frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-binding
  (lambda (var val)
    (cons var val)))

(define binding-variable 
  (lambda (binding)
    (car binding)))

(define binding-value 
  (lambda (binding)
    (cdr binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sym (make-symbol 'a))
(define prim-car (make-primitive-procedure car))
(define proc (make-procedure '(x) '(+ x y)))
(define elist (make-list '(1 2 3)))

