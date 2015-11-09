#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#

(define acc '())

(define (subsets lst)
  (begin
    (set! acc '())
    (sort-results lst)))




(define (build-set lst1 lst2)
  (if (or (empty? lst1) (empty? lst2))
      '()
      (-< lst1
          (list (first lst1))
          (cons (first lst1)
                (list (first lst2)))
          (append lst1 lst2)
          (build-set lst1 (rest lst2))
          (build-set (rest lst1) lst2)
          (build-set (remove (last lst1) lst1) lst2)
          )))



(define (sort-results lst)
  (let ([res (sort (build-set lst lst) <)])
    (if (or (in-list? acc res) (duplicate-id res))
        (next)
        (begin
          (set! acc (append acc (list res)))
          (if (equal? res "false.")
              (void)
              res)))))



;(sort-results lst3)
; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#

(define (sudoku-4 board)
  (?- constraints (board-builder board '())))

(define (board-builder lst acc)
  ;If "" feed it (-< 1 2 3 4) otherwise return val
  ;Traverse rows
  (if (empty? lst)
      acc
      (board-builder (rest lst) (append acc (list (board-helper (first lst) '()))))))

(define (board-helper lst acc)
  (if (empty? lst)
      acc
      (if (equal? (first lst) "")
          (board-helper (rest lst) (append acc (list (-< 1 2 3 4))))
          (board-helper (rest lst) (append acc (list (first lst)))))))
      

(define (constraints board)
  (let ([c1 (list (first (first board)) (first (second board))
                  (first (third board)) (first (fourth board)))]
        [c2 (list (second (first board)) (second (second board))
                  (second (third board)) (second (fourth board)))]
        [c3 (list (third (first board)) (third (second board))
                  (third (third board)) (third (fourth board)))]
        [c4 (list (fourth (first board)) (fourth (second board))
                  (fourth (third board)) (fourth (fourth board)))]
        [blk1 (list (first (first board)) (second (first board))
                    (first (second board)) (second (second board)))]
        [blk2 (list (third (first board)) (fourth (first board))
                    (third (second board)) (fourth (second board)))]
        [blk3 (list (first (third board)) (second (third board))
                    (first (fourth board)) (second (fourth board)))]
        [blk4 (list (third (third board)) (fourth (third board))
                    (third (fourth board)) (fourth (fourth board)))])
    (not (or (duplicate-id c1) (duplicate-id c2) (duplicate-id c3) (duplicate-id c4)
             (duplicate-id blk1) (duplicate-id blk2) (duplicate-id blk3) (duplicate-id blk4)
             (duplicate-id (first board)) (duplicate-id (second board))
             (duplicate-id (third board)) (duplicate-id (fourth board))))
    ; Returns #f if there is a problem otherwise #t
    ))



(define (duplicate-id lst)
  (if (empty? lst)
      #f
      (if (in-list? (rest lst) (first lst))
          #t
          (duplicate-id (rest lst)))))


(define (in-list? lst val)
  (if (empty? lst)
      #f
      (if (equal? (first lst) val)
          #t
          (in-list? (rest lst) val))))



(define b1
  '(("" "" 3 "")
    ("" 4 "" 2)
    ("" 2 "" "")
    (1 "" "" "")))

(define b2
  '((3 4 1 "")
    ("" 2 "" "")
    ("" "" 2 "")
    ("" 1 4 3)))

;(sudoku-4 b2)

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
(define-syntax fold-<
  (syntax-rules ()
    ))