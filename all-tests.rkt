#lang racket

;; Runs all tests in all subdirectories

;; To get very accurate stacktraces (see all expressions when there are
;; errors, not just functions), either run this in DrRacket, or run it
;; on the command line with:
;;   racket -l errortrace -t all-tests.rkt

(require "substitution-interpreter/substitution-tests.rkt"
         "env-functional-interpreter-compiler/interpreter-tests.rkt"
         "env-functional-interpreter-compiler/analyzer-tests.rkt"
         "imperative-interpreter-compiler/interpreter-tests.rkt"
         "imperative-interpreter-compiler/analyzer-tests.rkt")
