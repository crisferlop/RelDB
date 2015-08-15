#lang racket

(provide AND OR NOT)

(define (myelse )
  (> 3 1)
  )

(define (OR arg1 arg2)
  ;(or arg1 arg2)
  (cond
    ((equal? arg1 #t)
     #t
     )
    ((equal? arg2 #t)
     #t
     )
    ((myelse) #f)
    )
  )

(define (NOT arg)
  ;(not arg)
  (cond
    ((equal? arg #t)
     '#f
     )
    ((myelse)
     '#t
     )
    )
  )

(define (AND arg1 arg2)
  ;(and arg1 arg2)
  (cond 
    ((equal? arg1 #t)
     (cond 
       ((equal? arg2 #t)
        '#t
        )
       ((myelse) '#f)
       )
     )
    ((myelse) '#f)
    )
  )