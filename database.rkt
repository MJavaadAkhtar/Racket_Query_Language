#| Assignment 1 - Racket Query Language  (due Oct 4, 11:50pm, on Markus)

***Write the names, CDF accounts and student IDs for each of your group members below.***
***Max group size is two students.***
Mohammad Javaad Akhtar, akhtar34, 1002551838
Kyle Wing-Hoong Tov, tovkyle, 1002269993
|#
#lang racket


; Test helpers - use these instead of the built-in syntactic forms.
(define-syntax If
  (syntax-rules ()
    ((If a b c)
     (if a b c))))
(define (And x y) (and x y))
(define (Or x y) (or x y))
; Please do define And, Or as syntactic forms
; You may use the class code for this.
; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         index-of
         SELECT)
#|
(index-of lst atom)
    - This is the list from which we want the index from
    - Atom is the element we want to find the index of
return the integer number of the index of atom from the lst
|#
(define (indexer atom lst)
        (cond [(empty? lst) 10000]
              [(equal? atom (car lst)) 0]
              [#t (+ 1 (indexer atom (cdr lst)))]))
(define (index-of lst atom)
  (if (< (length lst) (indexer atom lst)) #f (indexer atom lst)))


; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)

  Returns a list of the attributes in 'table', in the order they appear.
|#
(define attributes car)

#|
(tuples table)
  table: a valid table

  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define tuples cdr)

#|
(size table)
  table: a valid table

  Returns the number of tuples in 'table'.
|#
(define (size tuple) (length (tuples tuple)))


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.
>(correspond '("Name" "Age" "LikesChocolate") "Age" '("David" 20 #t))
20
|#

(define (correspond lst str tuple) (list-ref tuple (index-of lst str)))

#|
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.

> (new-table-helper (λ(x) (> (length x) 2)) '((1 2 3  -4) (5 4 3) (7 8 9 -10) (3)))
'((1 2 3 -4) (5 4 3) (7 8 9 -10))

|#

(define (new-table-helper f table)
  (cond [(empty? table) '()]
        [(equal? (f (attributes table)) #f) (new-table-helper f (cdr table))]
        [#t (cons (attributes table) (new-table-helper f (cdr table)))]))

(define (new-table f table)
  (cons (attributes table) (new-table-helper f (cdr table))))

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
> ((replace-attr "Course" '("Name" "Course")) '("Illir" "CSC324"))
"CSC324"
|#

(define (replace-attr x lst)
  (λ(tuple) (if (equal? (index-of lst x) #f) x (if (empty? tuple) '() (list-ref tuple (index-of lst x))))))

#|
A function that takes: 
  - a list of needed attributes
  - a table you want the attributes from
  returns a new table with only needed attributes :)
It will be used for 'SELECT <attrs> FROM <table>

Function Explaination:
--> map1: takes two arguments, table and list. It computes the index of the wanting attributes from the table.
 > (map (λ(x) (index-of (attributes Person) x)) '("Name" "Age"))
'(0 1)

--> map2: takes two argument, table and list. The list is the index of the wanting attribute computed by map 1. It returns
          wanting attribute of one tuple
 >(map (λ(j) (list-ref(attributes Person) j))
              '(0 1))   ;(0 1) is computed by map1
'("Name" "Age")

--> map3: takes two arguments, table and list. It just iterates through all the tuples and give them as a parameter to map1 and map2


> (attribute-table '("Name" "Age") Person)
'(("Name" "Age") ("David" 20) ("Jen" 30) ("Paul" 100))
> (attribute-table '("Course") Teaching)
'(("Course") ("CSC324") ("CSC108") ("CSC343"))
> (attribute-table '("Age") Teaching)
'(("Age") ("Age") ("Age") ("Age"))
> (attribute-table '("Course") Person)
'(("Course") ("Course") ("Course") ("Course")))
|#

(define (attribute-table lst table)
  (map (λ(m)                         
         (map (λ(j) (if (string? j) j (list-ref m j))); Map 2
              (map (λ(x) (if (equal? (index-of (attributes table) x) #f) x (index-of (attributes table) x))) lst))) ;Map1
       table));|#




;------------Function from Ex1,f1---------------------------------;

(define (helper3 lst k) (cond [(empty? lst) #f]
                              [(equal? (car lst) k) #t]
                              [#t (helper3 (rest lst) k)]))

(define (search-table lst item) (cond [(empty? lst) '()]
                                      [(equal? (helper3 (car lst) item) #t) (cons (car lst) (search-table (cdr lst) item))]
                                      [#t (search-table (cdr lst) item)]))
;---------------------------------------------;



#|
(rename-dot lst-attr str-lst)
A function that takes:
  - a list of lists of aatributes
  - a list of strings that can be a potential pre-fixes

Returns a list of attributes with potential renames using the prefix from the str-lst


>(rename-dot '(("Name" "Age") ("Name" "Course"))
            '("P" "T"))

'("P.Name" "Age" "T.Name" "Course")

> (rename-dot '(("Name" "Age" "Course") ("Name" "Course"))
            '("P" "T"))
'("P.Name" "Age" "P.Course" "T.Name" "T.Course")


(define (rename-dot lstlst lst)
  ;(map (λ(x) (temp x lstlst (list-ref lst (index-of lstlst x)))) lstlst))
  (apply append (map (λ(x) (temp x lstlst (list-ref lst
                                                    ;(begin
                                                      (index-of lstlst x)
                                                      ;(append x "1234567880"))
                                                    ))) lstlst)))|#
(define (temp lst lstlst [str ""])
  (map (λ(x) (if (>= (length (search-table lstlst x)) 2)
                 (if (equal? str "") x (string-append str "." x)) x)) lst))

(define (rename-dot lstlst lst)
  (apply append  (rename-dot-help lstlst lstlst lst)))
                            
(define (rename-dot-help lstlst lst2 lst)
  (if (empty? lstlst) '() (append (list (temp (car lstlst) lst2 (car lst))) (rename-dot-help (cdr lstlst) lst2 (cdr lst)))))




;--------------------------------------KYLE TODO-----------------------------------;


#|
(sorting table f)
A function that takes:
  - a function that specifies the #:key for sorting inside the tuple
  - a valid table

  returns a new list of lists (table) in descending order according to keyfunc

> (sorting Person attributes)
'(("Name" "Age" "LikesChocolate") ("Paul" 100 #f) ("Jen" 30 #t) ("David" 20 #t))

> (sorting Person (replace "Age" (car Person)))
'(("Name" "Age" "LikesChocolate") ("Paul" 100 #f) ("Jen" 30 #t) ("David" 20 #t))

> (sorting Teaching (replace (+ (string-length "Name") (string-length "Course")) (car Teaching)))
'(("Name" "Course") ("David" "CSC324") ("David" "CSC343") ("Paul" "CSC108"))

>(sorting Person (replace (string-length "Name") (car Person)))
'(("Name" "Age" "LikesChocolate") ("David" 20 #t) ("Paul" 100 #f) ("Jen" 30 #t))

|#
(define (true? a b) (if (number? a) (> a b) (string>? a b)))
(define (sorting table f)
  (sort (cdr table) true? #:key f))
  


#|
Make a Macro For Order
|#

(define-syntax order-by
  (syntax-rules ()
    [(order-by <expr> <table>) (cons (car <table>) (sorting <table> (replace <expr> (car <table>))))]))



#|
(join tables ...)
   - Takes multiple table that needs to be join as input
and returns a cartesian product (Inner Join) of all the tables

> (join (cdr Person) (cdr Teaching))
'(("David" 20 #t "David" "CSC324")
  ("David" 20 #t "Paul" "CSC108")
  ("David" 20 #t "David" "CSC343")
  ("Jen" 30 #t "David" "CSC324")
  ("Jen" 30 #t "Paul" "CSC108")
  ("Jen" 30 #t "David" "CSC343")
  ("Paul" 100 #f "David" "CSC324")
  ("Paul" 100 #f "Paul" "CSC108")
  ("Paul" 100 #f "David" "CSC343"))
|#


(define (join tables)
       (if (empty? (cdr tables))
      (car tables)
      (map (λ(x) (apply append x)) (cartesian-product (car tables)
                         (join (cdr tables))))));|#

;----------------------------------------------------------------------------------;



(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     (λ(x) (if (empty? x) x 
               (apply (car (list ((replace expr table) x) ...)) (cdr (list ((replace expr table) x) ...)))))]
                                    
    [(replace atom table)
     ; Change this!
     (λ(lst) (if (empty? lst) lst ((replace-attr atom table) lst)))]))


;------ Where--------;
(define-syntax where
  (syntax-rules ()
    [(where <expr> <table>)
     (new-table (replace <expr> (attributes <table>)) <table>)]))
    
;--------------------;

(define (pairs lst)
 (cons
  (rename-dot
   (map car (map car lst))
  (map cdr lst))
  (join (map tuples (map car lst))))
  )

(define-syntax SELECT
  (syntax-rules (* <temp> FROM WHERE ORDER BY)
    
    
    [(SELECT <expr> FROM <table> WHERE <rest>)
     (SELECT  <expr> FROM (where <rest> <table>))]
    
    [(SELECT <expr> FROM <table> ... WHERE <rest>)
    (SELECT <expr> FROM (where <rest> (SELECT * FROM <table> ...)))]


    [(SELECT <expr> FROM <tables> ... ORDER BY <cond>)
     (SELECT <expr> FROM (order-by <cond> (SELECT * FROM <tables> ...)))]

    [(SELECT * FROM <table>)
     <table>]
    
    [(SELECT <expr> FROM <table> ... WHERE <rest> ORDER BY <cond>)
     (SELECT <expr> FROM (order-by <cond> (where <rest> (SELECT * FROM <table> ...))))]
    
    [(SELECT <expr> FROM <table>)
     (attribute-table <expr> <table>)]
    
    [(SELECT * FROM <table1> <table2> ...)
     (pairs (SELECT <temp> <table1> <table2> ...))]

    [(SELECT <expr> FROM <table> <table2> ...)
     (attribute-table <expr> (pairs (SELECT <temp> <table> <table2> ...)))] 

     ;Helper Macro for Select 
    [(SELECT <temp> [table name]) (list (cons table name))] 
    
    [(SELECT <temp> <entry>)(list (cons <entry> ""))]
    
    [(SELECT <temp> <entry> ...) (append (SELECT <temp> <entry>) ...)]
    
    ))
