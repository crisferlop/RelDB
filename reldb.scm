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
   ((insert-record-exp? input-text)
    (insert-record enviroment input-text)
    )
   ((remove-record-exp? input-text)
    (delete-record enviroment input-text)
    )
   ((update-record-exp? input-text)
    (update-record enviroment input-text)
    )
   ((querry-exp? input-text)
    (simple-query enviroment input-text)
    )
   ((querry-specific-exp? input-text)
    (especific-query enviroment input-text)
    )
   ((querry-specific-filtred-exp? input-text)
    (especific-query-with-filter enviroment input-text)
    )
   ((specific-insert-record-exp? input-text)
    (insert-record-specific enviroment input-text)
    )
   ((cprog-exp? input-text)
    (println "Es un comando valido")
    (store-procedure enviroment input-text)
    )
   ((eval-expr? input-text)
    (cond 
      ((exist-procedure? enviroment input-text)
       (reldb-aux enviroment (get-explicit-procedure enviroment input-text))
       )
      ((myelse)
       (println "El procedimiento citado no existe. Primero debe crearlo para poder usarlo, consulte el manual de usuario.")
       enviroment
       )
      )
    )
   ((myelse) (println "Comando incorrecto, consulte el manual de usuario.") enviroment)
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

;addt estudiantes id nombre appel
;addt estudiantecs id nombre appel
;addr estudiantes id estudiantecs


;addt estudiantes id nombre appel
;insert estudiantes 604220930 cristian rivera
;insert estudiantes (id nombre appel) 604220930 cristian rivera
;rr estudiantes 604220930
;ud estudiantes 604220930 nombre Cris appel Rivers
;query estudiantes
;query estudiantes (all) fil tro
;addt estudiantes id nombre primerapellido segundoapellido edad genero
;ins estudiantes 604220930 cristian Rivera Lopez 20 hombre
;ins estudiantes 101110111 Eddie Jimenez Lopez 14 hombre
;ins estudiantes 202220222 Daniela Rivera Lopez 18 mujer
;query estudiantes (all) genero hombre
;addt estudiantecs id nombre appel
;ins estudiantecs 604220930 cristian Rivera
;rr estudiantes 101110111
;ud estudiantes 101110111 edad 16
;query estudiantes
;query estudiantecs
;addr estudiantes id estudiantecs
;ud estudiantes 101110111 edad 14
;ud estudiantes 101110111 nombre Eddy genero hombre
;cprog estqry (column) query estudiantes (column)
;cprog addestudent (nombre appelido id) ins estudiantes id nombre appelido
;eval addestudent (cristian rivera 604220930)
;eval addestudent (Eddy Jimenez 101110111)
;cprog estqry (column) query estudiantes (column)
;eval estqry (all)