#lang racket
(require "logicsentences.scm")

(provide create-table-exp? add-ref-exp? remove-ref-exp? isDefinedExpresion insert-record-exp? 
         specific-insert-record-exp? update-record-exp? remove-record-exp? remove-table-exp?
         querry-exp? cprog-exp? eval-expr? querry-specific-exp? querry-specific-filtred-exp?)

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


;funcion isInsertTable
;Verifica si la instruccion insertada es valida para insertar una tabla retornando #t si lo es #f si no
;La instruccion tiene que contar con la estructura (addt|addtable) [A-Za-z0-9]+( [A-Za-z0-9]+)+ en regex para que sea valida
(define (create-table-exp? instruction)
  (isDefinedExpresion "(addt|addtable) [A-Za-z0-9]+( [A-Za-z0-9]+)+" instruction)
)

;funcion isAddReference
;Verifica si la instruccion insertada es valida para insertar una tabla retornando #t si lo es #f si no
;La instruccion tiene que contar con la estructura (addr|addReference) [A-Za-z0-9]+ [A-Za-z0-9]+ [A-Za-z0-9]Za-z0-9]+ en regex para que sea valida
(define (add-ref-exp? instruction)
  (isDefinedExpresion "(addr|addReference) [A-Za-z0-9]+ [A-Za-z0-9]+ [A-Za-z0-9]+" instruction)
  )

;funcion isRemReference
;Verifica si la instruccion insertada es valida para insertar una tabla retornando #t si lo es #f si no
;La instruccion tiene que contar con la estructura (addt|addtable) [A-Za-z0-9]+( [A-Za-z0-9]+)+ en regex para que sea valida
(define (remove-ref-exp? instruction)
  (isDefinedExpresion "(remr|remReference) [A-Za-z0-9]+ [A-Za-z0-9]+ [A-Za-z0-9]+" instruction)
  )


(define (insert-record-exp? instruction)
  (isDefinedExpresion "(ins|insert) [A-Za-z0-9]Za-z0-9]+ ([A-Za-z0-9]+)+" instruction)
  )

(define (specific-insert-record-exp? instruction)
  (isDefinedExpresion "(ins|insert) [A-Za-z0-9]+ \\([A-Za-z0-9]+( [A-Za-z0-9]+)+\\) [A-Za-z0-9]+( [A-Za-z0-9]+)+" instruction)
  )


(define (update-record-exp? instruction)
  (isDefinedExpresion "(up|update) [A-Za-z0-9]+ ( [A-Za-z0-9]+)+" instruction)
  )

(define (remove-record-exp? instruction)
  (isDefinedExpresion "(rr|remover) [A-Za-z0-9]+ [A-Za-z0-9]+" instruction)
  )

(define (remove-table-exp? instruction)
  (isDefinedExpresion "(dt|deltable) [A-Za-z0-9]+" instruction)
  )

(define (querry-exp? instruction)
  (isDefinedExpresion "query [A-Za-z0-9]+" instruction)
  )
(define (querry-specific-exp? instruction)
  (isDefinedExpresion "query [A-Za-z0-9]+ \\( [A-Za-z0-9]+\\)" instruction)
  )
(define (querry-specific-filtred-exp? instruction)
  (isDefinedExpresion "query [A-Za-z0-9]+ \\( [A-Za-z0-9]+\\) [A-Za-z0-9]+  [A-Za-z0-9]+" instruction)
  )

(define (cprog-exp? instruction)
  (isDefinedExpresion "cprog [A-Za-z0-9]+ [A-Za-z0-9]+ [A-Za-z0-9]+" instruction)
  )

(define (eval-expr? instruction)
  (isDefinedExpresion "eval [A-Za-z0-9]+ \\(([A-Za-z0-9]+( [A-Za-z0-9]+)+\\)( [A-Za-z0-9]+)+ \\(([A-Za-z0-9]+( [A-Za-z0-9]+)+\\)( [A-Za-z0-9]+)" instruction)
  )

