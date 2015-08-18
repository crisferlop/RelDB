#lang racket
(require "logicsentences.scm")
(require "iopatternverificator.scm")

;funcion println
;recibe el objeto que se desea imprimir en pantalla, e imprime un salto de linea
(define (println toprint)
  (display toprint)
  (newline)
  )

;funcion myelse
;es una funcion que sirve para reemplazar a la sentencia else esto para que el programa sea puramente funcional
(define (myelse )
  (> 3 1)
  )

(provide str->lst create-table add-reference-table insert-record delete-record update-record simple-query especific-query 
         especific-query-with-filter insert-record-specific)




;funcion str->lst 
;se encarga de transformar un string a una lista, si el string contiene parentesis se cuenta como una lista dentro de la lista a salir
(define (str->lst data)
  (str->lst-aux (string-split data ) '())
  )

;duncion to-close-pharentesis
;recibe una lista de string la funcion retorna una sublista de la lista, se retorna aquella sublista que esta despues
;de aquel elemento que termina con el caracter ")"
;ejemplo se recibe la lista ("1" "2" "3)" "4") se retorna ("4")
(define (to-close-pharentesis in)
  (cond
    ((equal? (string-ref (car in) (- (string-length (car in)) 1)) #\)) 
     (cdr in)
     ) 
    ((myelse) 
     (to-close-pharentesis (cdr in))
     )
    )
  )

;funcion str->lst-aux
;recibe una lista de string el parametro in y una lista en donde se dara el resultado
;es una funcion de ayuda para str-lst
(define (str->lst-aux in out)
  (cond
    ((null? in)
     out
     )
    ;si el primero es un parentesis
    ((equal? (string-ref (car in) 0) #\()
     (cond 
       ((equal? (string-ref (car in) (- (string-length (car in)) 1)) #\))
        ;(str->lst-aux
        ; (to-close-pharentesis in)
        ; (append (append out (list (string->symbol (substring (car in) 1 (- (string-length (car in)) 1)))))
        ;         (list (str->lst-aux (cdr in) '()))))
        (str->lst-aux (cdr in) (append out (list (list (string->symbol (substring (car in) 1 (- (string-length (car in)) 1)))))))
        )
       ((myelse)
        (str->lst-aux
         (to-close-pharentesis in)
         (append out 
                 (list (str->lst-aux (cdr in) (list (string->symbol (substring (car in) 1 (string-length (car in))))))))
         )
        )
       )
     )
    ;si el ultimo es un parentesis
    ((equal? (string-ref (car in) (- (string-length (car in)) 1)) #\))
     (append out (list (string->symbol (substring (car in) 0 (- (string-length (car in) ) 1)) ))))
    (else
     (str->lst-aux (cdr in) (append out (list (string->symbol (car in)))))
     )
    )
  )


;funcion create-table
;se encarga de crear una tabla en la variable enviroment, recibe dos parametros enviroment el primero y el segundo input-text
;la primera variable de ambiente es donde se guardan las tablas, el segundo parametro input-text es el string de entrada del teclado
;retorna la nueva variable de ambiente
(define (create-table enviroment input-text)
  (create-table-aux enviroment (cdr (str->lst input-text)))
  )

;funcion create-table-aux
;es la funcion de ayuda a la funcion create-table, recibe la variable ambiente y tambien la lista de entrada del comando ingresado
;del teclado
(define (create-table-aux enviroment list-table)
  (create-table-aux-aux enviroment (car list-table) (car (cdr list-table)) (cddr list-table))
  )

;funcion create-table-aux 
;se encarga de verificar si se repite una tabla en la lista de tablas
;recibe la listas de tablas y recibe el nombre de la tabla
;retorna #t si el nombre de la tabla ingresado existe en la lista de tablas, #f si no existe
(define (non-repeated-table-name? tables table-name) 
  (cond
    ((null? tables) #t)
    ((null? (car tables)) #t)
    ((NOT (equal? (caar tables) table-name)) 
     (non-repeated-table-name? (cdr tables) table-name)
     )
    ((myelse) #f)
    )
  )

;funcion non-repeated-table-attribute?
;recibe una lista
;retorna #t si algun elemento se repite #f si todos los elementos de la lista son diferentes
(define (non-repeated-table-attribute? columns) 
  (cond
    ((null? columns) #t)
    ((myelse) 
     (AND (NOT (exist-element-on-table? (car columns) (cdr columns))) (non-repeated-table-attribute? (cdr columns)))
     )
    )
  )

;funcion exist-element-on-table?
;recibe un elemento y una lista y verifica si elemento se encuentra en esa lista
;retorna #t si el elemento existe en la lista, #f si no es asi
(define (exist-element-on-table? element columns)
  (cond
    ((null? columns) #f)
    ((NOT (equal? (car columns) element)) 
     (exist-element-on-table? element (cdr columns))
     )
    ((myelse) #t)
    )
  )

;funcion exist-table?
;recibe una lista de tablas y recibe el nombre de una tabla
;verifica si esa tabla existe
(define (exist-table? tables table-name)
  (cond
    ((null? tables) #f)
    ((null? (car tables)) #f)
    ((NOT (equal? (caar tables) table-name)) 
     (exist-table? (cdr tables) table-name)
     )
    ((myelse) #t)
    )
  )


;funcion get-table
;recibe una lista de tablas y el nombre de una tabla
;retorna la tabla si existe, si esta no existe retorna una lista vacia
(define (get-table tables table-name)
  (cond
    ((null? tables) '())
    ((NOT (equal? (caar tables) table-name)) 
     (get-table (cdr tables) table-name)
     )
    ((myelse) (car tables))
    )
  )

;funcion create-table-aux-aux
;recibe la variable de ambiente, el nombre de la tabla a insertar, el primary key de la tabla y las columnas a insertar
;retorna la variable de ambiente
(define (create-table-aux-aux enviroment tablename primarykey columnames)
  (cond
    ((AND (non-repeated-table-name? (car enviroment) tablename) (non-repeated-table-attribute? (cons primarykey columnames)))
     (append (list (append (car enviroment) (list (list tablename primarykey columnames '())))) (cdr enviroment))
     )
    ((println "El nombre de la tabla esta repetido, o alguna columna de la tabla esta repetida, por favor verifique su entrada.") enviroment)
    )
  )


;funcion add-reference-table
;recibe la variable de ambiente (la base de datos) y el comando ingresado por el usuario
;retorna la nueva variable de ambiente
(define (add-reference-table enviroment input-text)
  (add-reference-aux enviroment (cdr (str->lst input-text)))
  )

;funcion add-reference-table-aux
;recibe la variable de ambiente (la base de datos) y la lista del comando ingresado por el usuario sin el primer elemento
;retorna la nueva variable de ambiente
(define (add-reference-aux enviroment table-list)
  (add-reference-aux-aux enviroment (car table-list) (cadr table-list) (caddr table-list))
  )

;funcion add-reference-table-aux-aux
;recibe la variable de ambiente (la base de datos) el nombre de la tabla en donde se agregara la referencia, el nombre de la llave
;foranea y la tabla de donde se encuentra esa llave foranea, en el caso de que la llave foranea sea incorrecta, o alguna de
;de las tablas no existan no se hara nada sobre la variable de ambiente
;retorna la nueva variable de ambiente
(define (add-reference-aux-aux enviroment table foreing-key source-table)
  (cond
    ((AND (AND (NOT (equal? table source-table)) (exist-table? (car enviroment) table)) (exist-table? (car enviroment) source-table))
     (cond 
       ((exist-element-on-table? foreing-key 
                                 (append (list (cadr (get-table (car enviroment) source-table))) (caddr (get-table (car enviroment) source-table))))
        (append (list (add-reference-aux-aux-aux (car enviroment) (cdr enviroment) table foreing-key source-table)) (cdr enviroment))
        )
       ((myelse)
        (println "La llave foranea es incorrecta verifique su entrada.")
        enviroment
        )
       )
     )
    ((myelse)
     (println "Alguna de las tablas insertadas no existen. Verifique su entrada.")
     enviroment
     )
    )
  )

;funcion add-reference-aux-aux-aux
;recibe la lista de tablas, los procedimientos la lista de ambiente, el nombre de la tabla en donde se hara en la insercion de la referencia
;la llave foranea y el nombre de la tabla donde e encuentra la llave foranea
;retorna la nueva lista de tablas 
(define (add-reference-aux-aux-aux tables procediments table foreing-key source-table)
  (cond
    ((null? tables)
     '()
     )
    ((equal? (caar tables) table)
     (append (list (append-reference (car tables) source-table foreing-key)) (cdr tables)))
    ((myelse)
     (append (append (list (car tables)) (add-reference-aux-aux-aux (cdr tables) procediments table foreing-key source-table))))
    )
  )

;funcion append-reference
;se encarga de insertar  la referencia en la tabla deseada, recibe una tabla, el nombre de una tabla ajena (foranea) y una llave foranea 
;retorna la nueva tabla de datos
(define (append-reference tabla source-table foreing-key)
  (append-reference-aux tabla source-table foreing-key '() 0)
  )



;funcion append-reference-aux
;se encarga de insertar  la referencia en la tabla deseada
;recibe una tabla, el nombre de una tabla ajena (foranea), una llave foranea, la tabla de salida y un contador
;retorna la nueva tabla de datos
(define (append-reference-aux in source-table foreing-key out counter)
  (cond
    ((= counter 4) 
     (append out (list (list source-table foreing-key)))
     )
    ((myelse)
     (append-reference-aux (cdr in) source-table foreing-key (append out (list (car in))) (+ counter 1)) 
     )
    )
  )


;funcion insert-record
;recibe la variable ambiente, y el comando ingresado por el usuario
;retorna la nueva variable de entorno
(define (insert-record enviroment input-text)
  (insert-record-aux enviroment (cdr (str->lst input-text)))
  )

;funcion insert-record-aux
;recibe la variable de ambiente y la lista de datos que fue parseada de string a lista, en caso de no existir la funcion
;retona la nueva variable de ambiente
(define (insert-record-aux enviroment in)
  (cond 
    ((exist-table? (car enviroment) (car in))
     (append (list (insert-record-aux-aux (car enviroment) in)) (cdr enviroment))
     )
    ((myelse)
     (println "La tabla seleccionada no existe.")
     enviroment
     )
    )
  )

;funcion get-columns-table
;obtiene todas las columnas de una tabla
;retorna las columnas de una tabla
(define (get-columns-table table)
  (append (list (cadr table)) (caddr table))
  )


;funcion change-list-position-with
;recibe una lista, una posicion y un elemento
;cambia el elemento que se encuentra en una lista por otro
;retorna la lista con elemento intercambiado
(define (change-list-position-with thelist pos element)
  (cond
    ((= pos 0)
     (append (list element) (cdr thelist))
     )
    ((myelse)
     (append (list (car thelist)) (change-list-position-with (cdr thelist) (- pos 1) element))
     )
    )
  )

;funcion get-pos
;recibe la tabla 
;obtiene un elemento en una posicion indicada
;retorna el elemento en esa posicion
(define (get-pos table pos)
  (cond
    ((= pos 0)
     (car table)
     )
    ((myelse)
     (get-pos (cdr table) (- pos 1))
     )
    )
  )


(define (get-index-of-first-appear thelist element)
  (get-index-of-first-appear-aux thelist element 0)
  )
(define (get-index-of-first-appear-aux thelist element out)
  (cond
    ((null? thelist)
     -1
     )
    ((equal? (car thelist) element)
     out
     )
    ((myelse)
     (get-index-of-first-appear-aux (cdr thelist) element (+ out 1))
     )
    )
  )
;funcion add-nils
;recibe una lista donde se agregaran los nils y un entero que seran los n-nils que se agregaran
;agrega n elementos nulos a una lista
;retorna la lista pasada por argumento con los elementos nil solicitados
(define (add-nils elements to-add)
  (append elements (create-nils-list to-add))
  )
;funcion create-nils-list
;recibe un entero que representa los n  nils que se crearan
;retorna una lista de nils
(define (create-nils-list to-add)
  (cond 
    ((equal? to-add 0)
     '())
    ((myelse)
     (append (create-nils-list (- to-add 1)) '(nil)))
    )
  )


;funcion append-record
;recibe una tabla y una lista de registros a agregar a la tabla
;se encarga si es posible de insertar records en la tabla
;retorna la tabla con los records insertados
(define (append-record table elements)
  (cond
    ((>= (lenght-list? (get-columns-table table)) (lenght-list? elements))
     (cond 
       ((NOT (exist-table? (car (cdddr table)) (car elements)))
        (append (append (append (list (car table) (cadr table)) (list (caddr table)))) (list (append (car (cdddr table)) (list (add-nils elements (- (lenght-list? (get-columns-table table)) (lenght-list? elements)))))))
        )
       ((myelse)
        (println "La clave principal del elemento esta repetida por favor intente de nuevo con una clave diferente")
        table
        )
       )
     )
    ((myelse)
     (println "Usted ha ingresado mas campos de lo que esta tabla soporta. Por favor verifique su entrada.")
     table
     )
    )
  )

;funcion lenght-list?
;retorna el largo de una lista, recibe como argumento la lista a medir el tamanio
(define (lenght-list? thelist)
  (cond
    ((null? thelist)0)
    ((+ 1 (lenght-list? (cdr thelist))))
    )
  )


;funcion insert-record-aux-aux
;es la funcion de auxilio de insert-record, inserta un record en una tabla y luego retorna la nueva tabla
;recibe la tabla y el registro a insertar
;retorna la nueva tabla
(define (insert-record-aux-aux tables in)
  (cond
    ((null? tables)
     '()
     )
    ((equal? (caar tables) (car in))
     (append (list (append-record (car tables) (cdr in))) (cdr tables)))
    ((myelse)
     (append (append (list (car tables)) (insert-record-aux-aux (cdr tables) in))))
    )
  )





;funcion insert-record
;recibe la variable ambiente, y el comando ingresado por el usuario
;retorna la nueva variable de entorno
(define (insert-record-specific enviroment input-text)
  (insert-record-specific-aux enviroment (cdr (str->lst input-text)))
  )

;funcion insert-record-aux
;recibe la variable de ambiente y la lista de datos que fue parseada de string a lista, en caso de no existir la funcion
;retona la nueva variable de ambiente
(define (insert-record-specific-aux enviroment in)
  (cond 
    ((exist-table? (car enviroment) (car in))
     (append (list (insert-record-specific-aux-aux (car enviroment) (cadr in) (append (list (car in)) (cddr in)))) (cdr enviroment))
     )
    ((myelse)
     (println "La tabla seleccionada no existe.")
     enviroment
     )
    )
  )


;funcion insert-record-aux-aux
;es la funcion de auxilio de insert-record, inserta un record en una tabla y luego retorna la nueva tabla
;recibe la tabla y el registro a insertar
;retorna la nueva tabla
(define (insert-record-specific-aux-aux tables filter in)
  (cond
    ((null? tables)
     '()
     )
    ;change-record-columns
    ((equal? (caar tables) (car in))
     (cond
       ((= (lenght-list? filter) (lenght-list? (cdr in)))
        (cond
          ((exist-element-on-table? (cadar tables) filter)
           (append (list (append-record (car tables) (init-record-columns (get-columns-table (car tables)) 
                   (create-nils-list (lenght-list? (get-columns-table (car tables)))) (merge-list filter (cdr in) '()) )) (cdr tables)))
           )
          ((myelse)
           (println "No se ha insertado la la columna de la llave primaria.")
           tables
           )        
          )
        )
       ((myelse)
        (println "La cantidad de elementos a insertar no coincide con las columnas elegidas por favor verifique.")
        tables
        )
       )
     )
     ((myelse)
      (append (append (list (car tables)) (insert-record-specific-aux-aux (cdr tables) filter in))))
     )
    )

(define (merge-list lista listb out)
  (cond
    ((null? lista)
     out
     )
    ((myelse)
     (merge-list (cdr lista) (cdr listb) (append out (list (car lista) (car listb))))
     )
  )
  )


(define (init-record-columns columns record to-update)
  (cond
    ((null? to-update) record)
    ((myelse)
     (init-record-columns columns (change-list-position-with record (get-index-of-first-appear columns (car to-update)) (cadr to-update)) (cddr to-update))
     )
    )
  )










(define (delete-element-list mlist element)
  (cond
    ((null? mlist)
     '()
     )
    ((equal? (car mlist) element)
     (cdr mlist)
     )
    ((myelse)
     (append (list (car mlist)) (delete-element-list (cdr mlist) element))
     )
    )
  )

(define (delete-element-table mlist element)
  (cond
    ((null? mlist)
     (display "La clave ")
     (display element)
     (println " no existe.")
     '()
     )
    ((equal? (caar mlist) element)
     (cdr mlist)
     )
    ((myelse)
     (append (list (car mlist)) (delete-element-table (cdr mlist) element))
     )
    )
  )


;funcion insert-record
;recibe la variable ambiente, y el comando ingresado por el usuario
;retorna la nueva variable de entorno
(define (delete-record enviroment input-text)
  (delete-record-aux enviroment (cdr (str->lst input-text)))
  )

;funcion insert-record-aux
;recibe la variable de ambiente y la lista de datos que fue parseada de string a lista, en caso de no existir la funcion
;retona la nueva variable de ambiente
(define (delete-record-aux enviroment in)
  (cond 
    ((exist-table? (car enviroment) (car in))
     (append (list (delete-record-aux-aux (car enviroment) in)) (cdr enviroment))
     )
    ((myelse)
     (println "La tabla seleccionada no existe.")
     enviroment
     )
    )
  )

(define (delete-record-aux-aux tables in)
  (cond
    ((null? tables)
     '()
     )
    ((equal? (caar tables) (car in))
     (list (list (caar tables) (cadar tables) (caddar tables) (delete-element-table (car (cdddar tables)) (cadr in)))))
    ((myelse)
     (append (append (list (car tables)) (delete-record-aux-aux (cdr tables) in))))
    )
  )



(define (update-element-table columns records to-update)
  (cond
    ((null? records)
     (display "La clave ")
     (display (car to-update))
     (println " no existe.")
     '()
     )
    ((equal? (caar records) (car to-update))
     (append (list(update-element-table-aux columns (car records) (cdr to-update) '())))
     )
    ((myelse)
     (append (list (car records)) (update-element-table columns (cdr records) to-update))
     )
    )
  )



(define (to-simple-form-for-update columns to-update)
  (to-simple-form-for-update-aux columns (cdr to-update) #t "" "" (list (car to-update)))
  )

(define (to-simple-form-for-update-aux columns to-update previous-is-a-column previous-value unificator out)
  (cond
    ((null? to-update) (append out (list (string->symbol previous-value))))
    ((exist-element-on-table? (car to-update) columns)
     (to-simple-form-for-update-aux 
      columns (cdr to-update) #t "" "" (append (append out (list (string->symbol previous-value))) (list (car to-update))))
     )
    ((myelse) 
     (to-simple-form-for-update-aux columns (cdr to-update) #f (string-append previous-value unificator (symbol->string (car to-update))) " " out)
     )
    )
  )

;(valid-form-for-update? '(nombre apellido) '(nombre args argas argum apellido allea))
;(to-simple-form-for-update '(nombre apellido) '(nombre args argas argum apellido allea))
(define (valid-form-for-update? columns to-update)
  (cond
    ((null? to-update) #f)
    ((exist-element-on-table? (car to-update) columns)
     (cond 
       ((>= (lenght-list? to-update) 2)
        (valid-form-for-update-aux? columns (cdr to-update) #t)
        )
       ((myelse) #f)
       )
     )
    ((myelse) #f)
    )
  )


(define (valid-form-for-update-aux? columns to-update previous-is-a-column)
  (cond
    ((null? to-update)
     (cond 
       ((equal? previous-is-a-column #t) #f)
       ((myelse) #t)
       )
     )
    ((exist-element-on-table? (car to-update) columns)
     (cond 
       ((equal? previous-is-a-column #t) #f)
       ((myelse) (valid-form-for-update-aux? columns (cdr to-update) #t))
       )
     )
    ((myelse) (valid-form-for-update-aux? columns (cdr to-update) #f))
    )
  )


(define (update-element-table-aux columns record to-update out)
  (cond 
    ((valid-form-for-update? columns to-update)
     (cond
       ((<= (/ (lenght-list? (to-simple-form-for-update columns to-update)) 2) (lenght-list? columns))
        (change-record-columns columns record (to-simple-form-for-update columns to-update))
        )
       ((myelse)
        (println "Ha ingresado mas columnas de las que tiene la tabla")
        record
        )
       )
     )
    ((myelse)
     (println "El comando de actualizacion no es correcto por favor verifique que sea correcto.")
     (println "Este problema suele suceder si usted no ha insertado alguna columna despues de la clave a buscar")
     (println "tambien puede darse cuando el nombre de dos tablas es consecutivo")
     (println "el ultimo caso es que usted haya puesto el nombre de la tabla pero no el elemento a cambiar o actualizar.")
     record
     )
    )
  )

(define (change-record-columns columns record to-update)
  (cond
    ((null? to-update) record)
    ((myelse)
     (change-record-columns columns (change-list-position-with record (+ (get-index-of-first-appear columns (car to-update)) 1) (cadr to-update)) (cddr to-update))
     )
    )
  )

(define (update-record enviroment input-text)
  (update-record-aux enviroment (cdr (str->lst input-text)))
  )

;funcion insert-record-aux
;recibe la variable de ambiente y la lista de datos que fue parseada de string a lista, en caso de no existir la funcion
;retona la nueva variable de ambiente
(define (update-record-aux enviroment in)
  (cond 
    ((exist-table? (car enviroment) (car in))
     (append (list (update-record-aux-aux (car enviroment) in)) (cdr enviroment))
     )
    ((myelse)
     (println "La tabla seleccionada no existe.")
     enviroment
     )
    )
  )

(define (update-record-aux-aux tables in)
  (cond
    ((equal? (caar tables) (car in))
     (list (list (caar tables) (cadar tables) (caddar tables) (update-element-table (caddar tables) (car (cdddar tables)) (cdr in)))))
    ((myelse)
     (append (append (list (car tables)) (update-record-aux-aux (cdr tables) in))))
    )
  )


(define (printlistrecords thelist)
  (cond 
    ((null? thelist) 0)
    ((myelse) 
     (cond 
       ((equal? (car thelist) 'nil) (display "--------"))
       ((myelse)(display (car thelist)))
       )
     (display " | ")
     (printlistrecords (cdr thelist))
     )
    )
  )

(define (simple-query enviroment text-input)
  (simple-query-aux (car enviroment) (cadr (str->lst text-input)))
  enviroment
  )

(define (simple-query-aux tables table-name)
  (cond
    ((exist-table? tables table-name)
     (println "============================================================================================================")
     (display "Tabla: ")
     (println table-name)
     (printlistrecords (get-columns-table (get-table tables table-name)))
     (println "")
     (println "============================================================================================================")
     (simple-query-aux-aux (cadddr (get-table tables table-name)))
     (println "============================================================================================================")
     )
    ((myelse)(println "La tabla no existe."))
    )
  )

(define (simple-query-aux-aux records)
  (cond
    ((null? records) 0)
    ((myelse)
     (printlistrecords (car records))
     (println "")
     (simple-query-aux-aux (cdr records))
     )
    )
  )


(define (especific-query enviroment text-input)
  (especific-query-aux (car enviroment) (cadr (str->lst text-input)) (caddr (str->lst text-input)))
  enviroment
  )

(define (valid-columns columns especific-columns)
  (cond
    ((null? especific-columns) #t)
    ((exist-element-on-table? (car especific-columns) columns)
     (valid-columns columns (cdr especific-columns))
     )
    ((myelse)
     #f
     )
    )
  )
(define (especific-query-aux tables table-name especific-columns)
  (cond
    ((exist-table? tables table-name)
     (cond 
       ((OR (valid-columns (get-columns-table (get-table tables table-name)) especific-columns) (equal? especific-columns '(all)))
        (println "============================================================================================================")
        (display "Tabla: ")
        (println table-name)
        (cond
          ((equal? especific-columns '(all))
           (printlistrecords (get-columns-table (get-table tables table-name)))
           )
          ((myelse)
           (printlistrecords especific-columns)
           )
          )
        (println "")
        (println "============================================================================================================")
        (cond
          ((equal? especific-columns '(all))
           (especific-query-aux-aux (cadddr (get-table tables table-name)) (get-columns-table (get-table tables table-name)) (get-columns-table (get-table tables table-name)))
           )
          ((myelse)
           (especific-query-aux-aux (cadddr (get-table tables table-name)) especific-columns (get-columns-table (get-table tables table-name)))
           )
          )
        (println "============================================================================================================")
        )
       ((myelse)
        (println "Alguna(s) columna(s) es/son invalida(s), por favor verifique.")
        )
       
       )
     )
    ((myelse)(println "La tabla no existe."))
    )
  )

(define (quit-invalid-columns columns especific-columns record)
  (cond
    ((null? especific-columns)
     '()
     )
    ((myelse)
     (+ (get-index-of-first-appear columns (car especific-columns)) 1)
     (append (list (get-pos record (get-index-of-first-appear columns (car especific-columns))))
             (quit-invalid-columns columns (cdr especific-columns) record))
     )
    )
  )

(define (especific-query-aux-aux records especific-columns columns)
  (cond
    ((null? records) 0)
    ((myelse)
     (printlistrecords (quit-invalid-columns columns especific-columns (car records)))
     (println "")
     (especific-query-aux-aux (cdr records) especific-columns columns)
     )
    )
  )







(define (especific-query-with-filter enviroment text-input)
  (especific-query-with-filter-aux (car enviroment) (cadr (str->lst text-input)) (caddr (str->lst text-input)) (cdddr (str->lst text-input)))
  enviroment
  )

(define (especific-query-with-filter-aux tables table-name especific-columns filter)
  (cond
    ((exist-table? tables table-name)
     (cond 
       (
        (AND 
         (OR (valid-columns (get-columns-table (get-table tables table-name)) especific-columns) (equal? especific-columns '(all)))
         (exist-element-on-table? (car filter) (get-columns-table (get-table tables table-name)))
         )
        (println "============================================================================================================")
        (display "Tabla: ")
        (println table-name)
        (cond
          ((equal? especific-columns '(all))
           (printlistrecords (get-columns-table (get-table tables table-name)))
           )
          ((myelse)
           (printlistrecords especific-columns)
           )
          )
        (println "")
        (println "============================================================================================================")
        (cond
          ((equal? especific-columns '(all))
           (especific-query-with-filter-aux-aux (cadddr (get-table tables table-name)) (get-columns-table (get-table tables table-name)) (get-columns-table (get-table tables table-name)) filter)
           )
          ((myelse)
           (especific-query-with-filter-aux-aux (cadddr (get-table tables table-name)) especific-columns (get-columns-table (get-table tables table-name)) filter)
           )
          )
        (println "============================================================================================================")
        )
       ((myelse)
        (println "Alguna(s) columna(s) es/son invalida(s) o la columna del filtro es incorrecta, por favor verifique.")
        )
       
       )
     )
    ((myelse)(println "La tabla no existe."))
    )
  )

(define (especific-query-with-filter-aux-aux records especific-columns columns filter)
  (cond
    ((null? records) 0)
    ((myelse)
     (cond
       ((equal? (get-pos (car records) (get-index-of-first-appear columns (car filter))) (cadr filter))
        (printlistrecords (quit-invalid-columns columns especific-columns (car records)))
        (println "")
        )
       )
     (especific-query-aux-aux (cdr records) especific-columns columns)
     )
    )
  )



;(especific-query '(((estudiantes id (nombre appel) ((604220930 cristian rivera)))) ()) "query estudiantes (id nombre)")
;(insert-record '(((table id (name appels) ())) ()) "ins table 604220930 cristian rivera")
;(change-list-position-with '(0 1 5 3 4) 2 2)
;(add-reference-table '(((t1 i (1) ()) (t2 i (1) ()) (t3 i (1) ())) ()) "addr t1 1 t2")

