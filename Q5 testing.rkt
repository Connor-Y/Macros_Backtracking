#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide fold-<)



; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define <next> "false.")
(define <steps> (void));
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< <combie> <init>)
     <init>]
    [(fold-< <combine> <init> <expr>)
     (fold-< <combine> (next))]
    
    ))



(fold-< + 0 (-< 1 2 3))