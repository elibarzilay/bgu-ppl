#lang racket

;; Runs all tests in all subdirectories

;; To get very accurate stacktraces (see all expressions when there are
;; errors, not just functions), either run this in DrRacket, or run it
;; on the command line with:
;;   racket -l errortrace -t all-tests.rkt
;; (but you need to make sure that the files are not compiled.)

(require "../class/substitution-interpreter/substitution-tests.rkt"
         "../class/env-functional-interpreter-compiler/interpreter-tests.rkt"
         "../class/env-functional-interpreter-compiler/analyzer-tests.rkt"
         "../class/imperative-interpreter-compiler/interpreter-tests.rkt"
         "../class/imperative-interpreter-compiler/analyzer-tests.rkt")
