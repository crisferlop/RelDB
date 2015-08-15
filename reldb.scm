#lang racket 
(require "iopatternverificator.scm")
(require "logicsentences.scm")
(require "humaninterpreter.scm")


;;La finalidad de este archivo es interactuar con el usuario nada mas. En otras palabra verifica si los datos de entrada son correctos

(define (println toprint)
  (display toprint)
  (newline)
)

(define (myelse )
  (> 3 1)
  )

(define (reldb-aux enviroment input-text)
  (cond 
   ((equal? input-text "exit") 
    '())
   ((create-table-exp? input-text)
    (create-table enviroment input-text)
    )
   ((add-ref-exp? input-text)
    (add-reference-table enviroment input-text)
    )
   (else enviroment)
   )
  )

(define (reldb db)
  (println db)
  (println "que desea realizar?")
  (display "> ")
  (cond 
   ((null? db)
    '()
    )
   ((> 3 1) ; condicion siempre cierta para que se de la recursion
    (reldb (reldb-aux db (read-line)))
    )
   )
  )



(println "Bienvenido a la plataforma RelDB en donde puede crear una base de datos relacional de una manera minimalista.")
(println "=============================================================================================================")
(reldb '(() ()))


;(exit)
