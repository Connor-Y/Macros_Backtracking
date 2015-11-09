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
; Problem: Does not overwrite default methods
; Do we need a case without (with)???? <-----------------
(define-syntax class-trait
  (syntax-rules (with)
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"])))]
    [(class <Class> (<attr> ...) (with <trait> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (let ([obj (temp-obj <Class> (<attr> ...)
                              [(<method> <param> ...) <body>] ...
                              )])
           (let ([ret (eager-eval obj msg <trait> ...)])
             (if (equal? ret "Unrecognized message!")
                 (eager-eval obj msg <trait> ...)
                 ret))
           
           )
         
         ))]
    ))

(define-syntax eager-eval
  (syntax-rules ()
    [(eager-eval stop <val>)
     <val>]
    [(eager-eval <obj> <msg> <trait>)
     ((<trait> <obj>) <msg>)]
    [(eager-eval <obj> <msg> <first> <trait> ...)
     (if (equal? ((<first> <obj>) <msg>) "Unrecognized message!")
         (eager-eval <obj> <msg> <trait> ...)
         (eager-eval stop ((<first> <obj>) <msg>)))]))

(define-syntax temp-obj
  (syntax-rules ()
    [(temp-obj <Class> (<attr> ...)
               [(<method> <param> ...) <body>] ...)
     (lambda(msg)
       (cond
         [(equal? msg (id->string <attr>)) <attr>]
         ...
         [(equal? msg (id->string <method>))
          (lambda (<param> ...) <body>)]
         ...
         [else "Unrecognized message!"]))
     ]))

(class-trait Point (x y) (with distance-trait simple-trait overlap-trait later-trait overwrite-trait)
             [(distance other-point)
              (let ([dx (- x (other-point "x"))]
                    [dy (- y (other-point "y"))])
                (sqrt (+ (* dx dx) (* dy dy))))])

(define p1 (Point 1 1))
(define p2 (Point 1 2))

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

(define (simple-trait obj)
  (lambda (msg)
    (if (equal? msg "1")
        "one"
        (obj msg))))

(define (overlap-trait obj)
  (lambda (msg)
    (if (equal? msg "1")
        "overlap"
        (obj msg))))

(define (overwrite-trait obj)
  (lambda (msg)
    (if (equal? msg "x")
        "overwrites"
        (obj msg))))

(define (later-trait obj)
  (lambda (msg)
    (if (equal? msg "a")
        "A"
        (obj msg))))



(p1 "a")
(p1 "1")
(p1 "x")
((p1 "distance-to-self"))
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

