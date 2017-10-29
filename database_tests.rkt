#| Assignment 1 - Racket Query Language Tests (due Oct 4, 11:50pm on Markus)

***Write the names, CDF accounts and student id for each of your group members below.***
***Max group size is two students.***
Mohammad Javaad Akhtar, akhtar34, 1002551838
Kyle Wing-Hoong Tov, tovkyle, 1002269993
|#

; Note the use of plai here; this is required for "test"
#lang plai
(abridged-test-output #t)

; This imports your file; do not change this line!
(require "database.rkt")

; Test helpers - use these instead of the built-in syntactic forms.
(define-syntax If
  (syntax-rules ()
  ((If a b c)
  (if a b c))))
(define (And x y) (and x y))
(define (Or x y) (or x y))
; Please do define And, Or as syntactic forms
; You may use the class code for this.

; Sample tables - add more tables!
; Ideas: empty table; table with just 1 attribute; 0 attributes; duplicates
(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))


#|
All tests go below. 
We have divided our tests into five sections:
- No WHERE/ORDER BY
- WHERE clause, but no ORDER BY
- ORDER BY clause, but no WHERE
- Both WHERE and ORDER BY
- Nested queries

Please respect this categorization when you add your tests,
and your TAs will appreciate it!
|#


; ---- SELECT/FROM tests ----
; Select all
(test (SELECT * FROM Person)
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Reordering columns
(test (SELECT '("Age" "LikesChocolate" "Name") FROM Person)
      '(("Age" "LikesChocolate" "Name")
        (20 #t "David") 
        (30 #t "Jen") 
        (100 #f "Paul")))

; -- Our test cases start--
(test (SELECT '("Age" "Course") FROM Teaching)
      '(("Age" "Course") ("Age" "CSC324") ("Age" "CSC108") ("Age" "CSC343")))
; -- Our test cases end--

; Select creates duplicates
(test (SELECT '("Name") FROM Teaching)
      '(("Name")
        ("David")
        ("Paul")
        ("David")))

; Select given a literal table
(test
 (SELECT '("A" "B")
   FROM '(("C" "A" "B" "D")
          (1 "Hi" 5 #t)
          (2 "Bye" 5 #f)
          (3 "Hi" 10 #t)))
 '(("A" "B")
   ("Hi" 5)
   ("Bye" 5)
   ("Hi" 10)))

; -- Our test cases start--
(test (SELECT '("Name" "Age") FROM Person)
      '(("Name" "Age") ("David" 20) ("Jen" 30) ("Paul" 100)))

(test (SELECT '("Course") FROM Teaching)
      '(("Course") ("CSC324") ("CSC108") ("CSC343")))

; Select given an attribute that does not exist
(test (SELECT '("Age") FROM Teaching)
      '(("Age") ("Age") ("Age") ("Age")))

(test (SELECT '("Course" "Age") FROM Person)
      '(("Course" "Age") ("Course" 20) ("Course" 30) ("Course" 100)))
; -- Our test cases end--

; Select all from two product of two tables
(test (SELECT * FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #T "David" "CSC343")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")))

; Select some from two tables
(test (SELECT '("P.Name" "Course" "Age") FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Course" "Age")
        ("David" "CSC324" 20)
        ("David" "CSC108" 20)
        ("David" "CSC343" 20)
        ("Jen" "CSC324" 30)
        ("Jen" "CSC108" 30)
        ("Jen" "CSC343" 30)
        ("Paul" "CSC324" 100)
        ("Paul" "CSC108" 100)
        ("Paul" "CSC343" 100)))

; Take the product of a table with itself
(test (SELECT '("E.Course" "E1.Course") FROM [Teaching "E1"] [Teaching "E"])
      '(("E.Course" "E1.Course")
        ("CSC324" "CSC324")
        ("CSC108" "CSC324")
        ("CSC343" "CSC324")
        ("CSC324" "CSC108")
        ("CSC108" "CSC108")
        ("CSC343" "CSC108")
        ("CSC324" "CSC343")
        ("CSC108" "CSC343")
        ("CSC343" "CSC343")))

; Take the product of a literal table with an identifier
(test
 (SELECT *
   FROM ['(("Age" "A" "Name" "D")
           (1 "Hi" 5 #t)
           (2 "Bye" 5 #f)
           (3 "Hi" 10 #t))
         "T1"]
        [Person "T2"])
 '(("T1.Age" "A" "T1.Name" "D" "T2.Name" "T2.Age" "LikesChocolate")
   (1 "Hi" 5 #t "David" 20 #t)
   (1 "Hi" 5 #t "Jen" 30 #t)
   (1 "Hi" 5 #t "Paul" 100 #f)
   (2 "Bye" 5 #f "David" 20 #t)
   (2 "Bye" 5 #f "Jen" 30 #t)
   (2 "Bye" 5 #f "Paul" 100 #f)
   (3 "Hi" 10 #t "David" 20 #t)
   (3 "Hi" 10 #t "Jen" 30 #t)
   (3 "Hi" 10 #t "Paul" 100 #f)))


; ---- WHERE ----
; Attribute as condition, select all
(test (SELECT *
        FROM Person
        WHERE "LikesChocolate")
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Jen" 30 #t)))

; -- Our test cases start--
(test (SELECT *
        FROM Person
        WHERE (< "Age" 25))
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)))

; -- Our test cases end--

; Attribute as condition, select subset
(test (SELECT '("LikesChocolate" "Name")
        FROM Person
        WHERE "LikesChocolate")
      '(("LikesChocolate" "Name")
        (#t "David")
        (#t "Jen")))

; Condition as function of one attribute, select all
(test (SELECT *
        FROM Person
        WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

; Condition as function of one attribute, select nonegit p
(test (SELECT '()
        FROM Teaching
        WHERE (equal? "Name" "David"))
      '(()
        ()
        ()))

; Constant true condition
(test (SELECT *
        FROM Person
        WHERE #t)
      Person)

; Constant false compound condition
(test (SELECT *
        FROM Person
        WHERE (> (string-length "David") 20))
      '(("Name" "Age" "LikesChocolate")))

; Condition on a literal table
(test (SELECT '("C" "B")
        FROM '(("A" "B" "C") 
               (1 2 3)
               (3 10 40)
               (4 4 4)
               (2 3 -1))
        WHERE (odd? "A"))
      '(("C" "B")
        (3 2)
        (40 10)))

; Simple condition on joined tables
(test (SELECT *
        FROM [Person "P"] [Teaching "T"]
        WHERE (equal? "P.Name" "T.Name"))
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "David" "CSC343")
        ("Paul" 100 #f "Paul" "CSC108")))

; Compound condition on three joined tables
(test (SELECT '("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        FROM [Person "P"] [Teaching "T"] [Person "P1"]
        WHERE (And "P.LikesChocolate" (equal? "P1.Name" "T.Name")))
      '(("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        ("David" #t 20 "CSC324")
        ("Paul" #f 20 "CSC108")
        ("David" #t 20 "CSC343")
        ("David" #t 30 "CSC324")
        ("Paul" #f 30 "CSC108")
        ("David" #t 30 "CSC343")))


; ---- ORDER BY ----
; Order by attribute
(test (SELECT *
        FROM Person
        ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order by attribute, not selected
(test (SELECT '("Name")
        FROM Person
        ORDER BY "Age")
      '(("Name")
        ("Paul")
        ("Jen")
        ("David")))

; -- Our test cases start--
(test (SELECT '("Age")
        FROM Person
        ORDER BY "Name")
      '(("Age") (100) (30) (20)))
; -- Our test cases end--

; Order by a function of an attribute
(test (SELECT *
        FROM Person
        ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Paul" 100 #f)
        ("Jen" 30 #t)))

; -- Our test cases start--
(test (SELECT *
        FROM Person
        ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

(test (SELECT *
        FROM Teaching
        ORDER BY "Course")
      '(("Name" "Course")
        ("David" "CSC343")
        ("David" "CSC324")
        ("Paul" "CSC108")))

(test (SELECT *
        FROM Teaching
        ORDER BY "Name")
      '(("Name" "Course")
        ("Paul" "CSC108")
        ("David" "CSC324")
        ("David" "CSC343")))
; -- Our test cases end--

; Order with duplicate
(test (SELECT *
        FROM Teaching
        ORDER BY (+ (string-length "Name") (string-length "Course")))
      '(("Name" "Course")
        ("David" "CSC324")
        ("David" "CSC343")
        ("Paul" "CSC108")))

; Order on a literal table
(test (SELECT *
        FROM '(("A" "B" "C") 
               (1 2 3)
               (3 10 40)
               (4 4 4)
               (2 3 -1))
        ORDER BY "C")
      '(("A" "B" "C")
        (3 10 40)
        (4 4 4)
        (1 2 3)
        (2 3 -1)))

; -- Our test cases start--
(test (SELECT *
        FROM Teaching
        ORDER BY "Course")
      '(("Name" "Course")
        ("David" "CSC343")
        ("David" "CSC324")
        ("Paul" "CSC108")))
; -- Our test cases end--

; Order on two tables
(test (SELECT *
        FROM [Person "P"] [Teaching "T"]
        ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #t "David" "CSC343")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")))


; ---- ORDER BY and WHERE ----
; Use attributes, select all 
(test
 (SELECT * 
   FROM Person 
   WHERE "LikesChocolate" 
   ORDER BY "Age")
 '(("Name" "Age" "LikesChocolate")
   ("Jen" 30 #t)
   ("David" 20 #t)))

; Use attributes, select one unused attribute
(test
 (SELECT '("Name") 
   FROM Person 
   WHERE "LikesChocolate" 
   ORDER BY "Age")
 '(("Name")
   ("Jen")
   ("David")))

; Two joined tables, select all
(test
 (SELECT * 
   FROM [Person "P"] [Teaching "T"] 
   WHERE (equal? "P.Name" "T.Name")
   ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Two joined tables, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
   FROM [Person "P"] [Teaching "T"] 
   WHERE (equal? "P.Name" "T.Name")
   ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))

; -- Our test cases start--
(test
 (SELECT '("P.Name" "T.Course" "LikesChocolate" "Difficulty")
   FROM [Person "P"] [Teaching "T"] ['(("Course" "Difficulty")
                                       ("CSC108" "ExtremelyHard")
                                       ("CSC324" "Easy")
                                       ("CSC343" "MediumSpice")) "K"]
   WHERE (And (equal? "P.Name" "T.Name") (equal? "T.Course" "K.Course"))
   ORDER BY "Age")
 '(("P.Name" "T.Course" "LikesChocolate" "Difficulty")
   ("Paul" "CSC108" #f "ExtremelyHard")
   ("David" "CSC324" #t "Easy")
   ("David" "CSC343" #t "MediumSpice")))
; -- Our test cases end--
 
; ---- Nested queries ----
(test
 (SELECT * 
   FROM (SELECT '("Age" "Name") FROM Person))
 '(("Age" "Name")
   (20 "David")
   (30 "Jen")
   (100 "Paul")))

(test
 (SELECT '("Person.Name" "Course")
   FROM [(SELECT '("Name") FROM Person) "Person"]
        [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                           (equal? "Course" "CSC108")))
         "Teaching"])
 '(("Person.Name" "Course")
   ("David" "CSC108")
   ("David" "CSC343")
   ("Jen" "CSC108")
   ("Jen" "CSC343")
   ("Paul" "CSC108")
   ("Paul" "CSC343")))

; - Our test cases start--
(test
 (SELECT '("Person.Name" "Course" "Age")
   FROM [(SELECT '("Name") FROM Person) "Person"]
        [(SELECT '("Age") FROM Person) "SamePerson"]
        [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                           (equal? "Name" "Paul")))
         "Teaching"]
   WHERE (< "Age" 50)
   ORDER BY "Age")
'(("Person.Name" "Course" "Age")
  ("David" "CSC108" 30)
  ("David" "CSC343" 30)
  ("Jen" "CSC108" 30)
  ("Jen" "CSC343" 30)
  ("Paul" "CSC108" 30)
  ("Paul" "CSC343" 30)
  ("David" "CSC108" 20)
  ("David" "CSC343" 20)
  ("Jen" "CSC108" 20)
  ("Jen" "CSC343" 20)
  ("Paul" "CSC108" 20)
  ("Paul" "CSC343" 20)))
; - Our test cases end--

; Nested query containing a literal
(test
 (SELECT *
   FROM [(SELECT '("A") 
           FROM '(("A" "B") 
                  (1)
                  (10)))
         "Table1"]
        [(SELECT *
           FROM '(("C" "A")
                  ("Hi" "Bye")
                  ("Dog" "Cat")
                  ("Red" "Blue")))
         "Table2"]
   WHERE (And (equal? (string-length "Table2.A") 3) (< 0  "Table1.A")))
 '(("Table1.A" "C" "Table2.A")
   (1 "Hi" "Bye")
   (1 "Dog" "Cat")
   (10 "Hi" "Bye")
   (10 "Dog" "Cat")))

; - Our test cases start--
(test
 (SELECT *
   FROM [(SELECT '("Pronoun" "Verb") 
           FROM '(("Pronoun" "Adverb" "Verb" "Noun") 
                  ("I" "Really" "Hate" "Assignments")
                  ("He" "Intensly" "Like" "Projects")
                  ("She" "Slowly" "Drives" "School")))
         "Table1"]
        [(SELECT '("Verb" "Noun")
           FROM '(("Noun" "Verb")
                  ("Assignments" "Hop")
                  ("Projects" "Sleep")
                  ("School" "Jump")))
         "Table2"]
   WHERE (< (string-length "Table2.Verb") (string-length "Table1.Verb")))
 '(("Pronoun" "Table1.Verb" "Table2.Verb" "Noun")
   ("I" "Hate" "Hop" "Assignments")
   ("He" "Like" "Hop" "Assignments")
   ("She" "Drives" "Hop" "Assignments")
   ("She" "Drives" "Sleep" "Projects")
   ("She" "Drives" "Jump" "School")))
; - Our test cases end--