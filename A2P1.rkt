#lang racket
#| Assignment 2 - Classes

This file contains your work for Questions 1 and 2, extending the basic
class macro to include support for traits and some basic introspection.
|#
(provide class-meta class-trait)

; QUESTION 1 (metaprogramming)
(define-syntax class-meta
  (syntax-rules ()
    [(class-meta <Class> (<attr> ...)
                 [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg "_attributes")
                (sort (list (list (id->string <attr>) <attr>) ...) #:key car string<?)]
               [(equal? msg "_methods")
                (sort (list (list (id->string <method>) (lambda (<param> ...) <body>)) ...) #:key car string<?)]
               [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))



; QUESTION 2 (traits).

; Do we need a case without (with)???? <-----------------
(define-syntax class-trait
  (syntax-rules (with)
    [(class-trait <Class> (<attr> ...) (with)
                  [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"])))]
    [(class-trait <Class> (<attr> ...) (with <trait> ...)
                  [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)           
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else (void) ;Do stuff here 
                ]))
       )]))

(define-syntax get-obj
  (syntax-rules ()
    [(get-obj <Class> (<attr> ...))
     ((<Class> <attr> ...))]))

(define (distance-trait obj)
  (lambda (msg)
    (cond [(equal? msg "distance-to-self")
           (lambda () ((obj "distance") obj))]
          [(equal? msg "closer")
           (lambda (obj1 obj2)
             (if (<= ((obj "distance") obj1)
                     ((obj "distance") obj2))
                 obj1
                 obj2))]
          [else (obj msg)])))


(class-trait Point (x y) (with distance-trait)
             [(distance other-point)
              (let ([dx (- x (other-point "x"))]
                    [dy (- y (other-point "y"))])
                (sqrt (+ (* dx dx) (* dy dy))))])


(define p1 (Point 1 1))
(define p2 (Point 1 2))

; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))

