#lang racket




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


; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> [(<param> (<func> <arg> ...))
                     ...])
     (define (<Class> <param> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <param>)) (<func> <arg> ...)]
               ...
               [else "Unrecognized message!"])
         )
       )
     ]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))




;(constructor A (w (+ 2 1)))

;(define a1 (A 1))

;(constructor B [(q (+ 2 1)) (w (+ 5 5))])
;(define b1 (B 3 4))
(define class Point (x y))

(define (test x y)
  (lambda (msg)
    "msg rec"))
