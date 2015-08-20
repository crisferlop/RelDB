#lang racket
(require "logicsentences.scm")
(require "humaninterpreter.scm")



;exporta las funciones importantes
(provide create-table-exp? add-ref-exp? remove-ref-exp? isDefinedExpresion insert-record-exp? 
         specific-insert-record-exp? update-record-exp? remove-record-exp? remove-table-exp?
         querry-exp? cprog-exp? eval-expr? querry-specific-exp? querry-specific-filtred-exp?)


(define (myelse )
  (> 3 1)
  )
(define (isDefinedExpresion regexpresion instruction)
  (cond 
    ((AND (NOT (null? instruction)) (string? instruction)) 
     (regexp-match-exact? regexpresion instruction)
     )
    ((eval '#t)
     #f
     )
    )
  )


;funcion create-table-exp?
;Verifica si la instruccion insertada es valida para insertar una tabla retornando #t si lo es #f si no
(define (create-table-exp? instruction)
  (isDefinedExpresion "(addt|addtable) [A-Za-z]+( [A-Za-z]+)+" instruction)
  )

;funcion add-ref-exp?
;Verifica si la instruccion insertada es valida para crear una relacion entre tablas retornando #t si lo es #f si no
(define (add-ref-exp? instruction)
  (isDefinedExpresion "(addr|addReference) [A-Za-z]+ [A-Za-z]+ [A-Za-z]+" instruction)
  )

;funcion remove-ref-exp?
;Verifica si la instruccion insertada es valida para remover una referencia de una tabla retornando #t si lo es #f si no
(define (remove-ref-exp? instruction)
  (isDefinedExpresion "(remr|remReference) [A-Za-z]+ [A-Za-z]+ [A-Za-z]+" instruction)
  )

;funcion insert-record-exp?
;Verifica si la instruccion insertada es valida para insertar un record en una tabla retornando #t si lo es #f si no
(define (insert-record-exp? instruction)
  (isDefinedExpresion "(ins|insert) [A-Za-z0-9]+ [A-Za-z0-9]+( [A-Za-z0-9]+)+" instruction)
  )

;funcion specific-insert-record-exp?
;Verifica si la instruccion insertada es valida para insertar un record con un formato especifico en una tabla retornando #t si lo es #f si no
(define (specific-insert-record-exp? instruction)
  (isDefinedExpresion "(ins|insert) [A-Za-z]+ \\([A-Za-z]+( [A-Za-z]+)+\\) [A-Za-z0-9]+( [A-Za-z0-9]+)+" instruction)
  )

;funcion update-record-exp?
;Verifica si la instruccion insertada es valida para actualizar un record de una tabla retornando #t si lo es #f si no
(define (update-record-exp? instruction)
  (isDefinedExpresion "(ud|update) [A-Za-z0-9]+( [A-Za-z0-9]+)+" instruction)
  )

;funcion remove-record-exp?
;Verifica si la instruccion insertada es valida para remover un record de una tabla retornando #t si lo es #f si no
(define (remove-record-exp? instruction)
  (isDefinedExpresion "(rr|remover) [A-Za-z0-9]+ [A-Za-z0-9]+" instruction)
  )

;funcion remove-table-exp?
;Verifica si la instruccion insertada es valida para borrar una tabla retornando #t si lo es #f si no
(define (remove-table-exp? instruction)
  (isDefinedExpresion "(dt|deltable) [A-Za-z0-9]+" instruction)
  )

;funcion querry-exp?
;Verifica si la instruccion insertada es valida para consultar una tabla completa retornando #t si lo es #f si no
(define (querry-exp? instruction)
  (isDefinedExpresion "query [A-Za-z0-9]+" instruction)
  )

;funcion querry-specific-exp?
;Verifica si la instruccion insertada es valida para consultar campos especificos de una tabla retornando #t si lo es #f si no
(define (querry-specific-exp? instruction)
  (isDefinedExpresion "query [A-Za-z0-9]+ \\([A-Za-z0-9]+( [A-Za-z0-9]+)*\\)" instruction)
  )

;funcion querry-specific-filtred-exp?
;Verifica si la instruccion insertada es valida para consultar campos especificos ( con algun filtro aplicado) de una tabla retornando #t si lo es #f si no
(define (querry-specific-filtred-exp? instruction)
  (isDefinedExpresion "query [A-Za-z0-9]+ \\([A-Za-z0-9]+( [A-Za-z0-9]+)*\\) [A-Za-z0-9]+ [A-Za-z0-9]+" instruction)
  )


;funcion cprog-exp?
;Verifica si la instruccion insertada es valida para insertar una funcion en el programa retornando #t si lo es #f si no
(define (cprog-exp? instruction)
  ;(isDefinedExpresion "cprog [A-Za-z0-9]+ [A-Za-z0-9]+ [A-Za-z0-9]+" instruction)
  (cprog-exp-aux? (str->lst (string-replace instruction "\n" "")))
  )

(define (cprog-exp-aux? thelist)
  (cond
    ((> (lenght-list? thelist) 3)
     (cond
       ((create-table-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((add-ref-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((insert-record-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((remove-record-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((update-record-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((querry-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((querry-specific-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((querry-specific-filtred-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((specific-insert-record-exp? (lst->str (cdddr thelist) " "))
        #t
        )
       ((myelse)#f)
       )
     )
    ((myelse)
     #f
     )
    )
  )

;funcion eval-expr?
;Verifica si la instruccion insertada es valida para evaluar una instruccion construida por el usuario retornando #t si lo es #f si no
(define (eval-expr? instruction)
  (isDefinedExpresion "eval [A-Za-z0-9]+ \\([A-Za-z0-9]+( [A-Za-z0-9]+)*\\)" instruction)
  )

