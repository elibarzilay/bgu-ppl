(define gensym
  ((lambda (counter prefix)
     (lambda ()
       (set! counter (+ counter 1))
       (string->symbol (string-append prefix (number->string counter)))))
   0 "sym"))

(define (error reason . args)
  (display "Error: ")
  (display reason)
  (for-each (lambda (arg)
              (display " ")
              (write arg))
            args)
  (newline)
  (scheme-report-environment -1))


;(define error #f)
;(call-with-current-continuation 
; (lambda(c)
;   (set! error (lambda(message)
;                 (c `("error: " ,message))))))


(define andmap
  (lambda (f . s)
    (letrec ((loop (lambda (s)
                     (or (null? (car s))
                         (and (apply f (map car s))
                              (loop (map cdr s)))))))
      (loop s))))

(define populate-list
  (lambda (value length)
    (if (= length 0) '() 
        (cons value (populate-list value (- length 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  TESTS  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define apply-test-list
  (lambda (eval-proc lst)
    (letrec ((apply-test-list$ 
              (lambda (lst fail)
                (if (null? lst) 'ok
                    (let ((tuple (car lst)))
                      (let ((res (eval-proc (car tuple)))
                            (expected-res (cadr tuple)))
                        (if (not (equal? res expected-res))
                            (fail (append tuple (list res)))
                            (apply-test-list$ (cdr lst) fail))))))))
      (apply-test-list$ lst error))))
