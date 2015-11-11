#lang racket
;---- Point Class
(class Point (x y) 
  [(distance other-point)
   (let ([dx (- x (other-point "x"))]
         [dy (- y (other-point "y"))])
     (sqrt (+ (* dx dx) (* dy dy))))])


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

;-----------------
(define-syntax constructor
  (syntax-rules ()
    ; Simply creates a class <Class> with values (args ...)
    [(constructor <Class> (<param> ...))
     (class <Class> (<param> ...))]
    ; Want to be allow user to set new values
    [(constructor <Class> [(<param> (<func> <arg> ...))
                           ...])
     (class <Class> [(<param> (<func> <arg> ...))
                     ...])
     ]
    ))


(define-syntax class
  (syntax-rules ()
    [(class <Class> [(<param> (<func> <arg> ...))
                     ...])
     (define (<Class> <param> ...)
       (list (cons <param> (list (<func> <arg> ...))) ...)
        )

     ]))


;---------------

(define-syntax class
  (syntax-rules ()
    [(class <Class> (<param> (<func> <arg> ...))
       ...)
     (define (<Class> <param> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <id>)) (<func> <arg> ...)]
               ...
               
               [else "Unrecognized message!"]))
       )])) 