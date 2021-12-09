;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW10b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Atom is one of
; - Number
; - String
; - Boolean
; - Symbol
; 
; An SExpr is one of
; - Atom
; - ListOfSExpr

; A ListOfSExpr is one of
; - '()
; - (cons SExpr ListOfSExpr)

;; atom? : Any -> Boolean
(define (atom? val)
  (or (number? val)
      (string? val)
      (boolean? val)
      (symbol? val)))

(define (atom-temp a)
  (cond [(number? a) ...]
        [(string? a) ...]
        [(boolean? a) ...]
        [(symbol? a) ...]))

;; sxpr-temp : SExpr -> ?
#;(define (sexpr-temp s)
  (cond [(atom? s) ... (atom-temp s) ...]
        [(list? s) ... (los-temp s) ...]))
                     
;; los-temp : LOS -> ?
#;(define (los-temp los)
  (cond [(empty? los) ...]
        [(cons? los) ... (sexpr-temp (first los))
                     ... (los-temp (rest los)) ...]))

;; EXAMPLES

;; Exercise 2
;; Print-sexpr: (X) [X,Y] -->String
;; Takes in a SExpr and returns a string depending on the atom
(check-expect (print-sexpr "Hi") "Hi")
(check-expect (print-sexpr 2) "2")
(check-expect (print-sexpr #t) "#true")
(check-expect (print-sexpr 'a) "a")
(check-expect (print-sexpr (list 1 2 3)) "(list 1 2 3)")
(check-expect (print-sexpr (list (list 3 "hi" (list 'bye (list 5)))))
              "(list (list 3 \"hi\" (list 'bye (list 5))))")

(define (print-sexpr s)
  (local [(define (list-helper s)
            (foldr (λ (s n) (string-append " " (print-sexpr s) n)) "" s))]
  (cond
    [(number? s) (number->string s)]
    [(string? s) (string-append "\"" s "\"")]
    [(boolean? s) (boolean->string s)]
    [(symbol? s) (symbol->string s)]
    [(list? s) (string-append "(list" (list-helper s) ")")])))

;; Exercise 3
; A [Wide-tree X] is one of
; - X
; - [List-of [Wide-tree X]]

;; sexpr-map : SExpr [Number -> X] [String -> X] [Boolean -> X] [Symbol -> X] -> [Wide-tree X]
;; Takes in a SExpr and four functions and creates a [Wide-tree X] that is the same shape as
;; the original s-expression, but where each Atom in the s-expression has been transformed
;; by the appropriate function.
(define List-Of-SExpr1 (list "hi" 3 'a #f))
(define List-Of-SExpr2 (list "hi"
                             (list #false 4
                                   (list 'apple "bye")
                                   (list ))
                             (list (list ) 'foo)))
(define List-Of-SExpr4 "hello")
(check-expect (sexpr-map List-Of-SExpr4 sub1 string->symbol not symbol->string) 'hello)
(check-expect (sexpr-map List-Of-SExpr1 add1 string-length not symbol->string) (list 2 4 "a" #true))
(check-expect (sexpr-map List-Of-SExpr2 sub1 string->symbol boolean->string symbol?)
              (list 'hi
                    (list "#false" 3
                          (list #true 'bye)
                          (list ))
                    (list (list ) #true)))
              
(define (sexpr-map se n s b sy)
  (local [(define (list-help se)
            (foldr (λ (x y) (append (list (sexpr-map x n s b sy)) y)) '() se))]
 (cond
   [(empty? se) se]
   [(number? se) (n se)]
   [(string? se) (s se)]
   [(boolean? se)(b se)]
   [(symbol? se) (sy se)]
   [(list? se) (list-help se)])))



;; Exercise 4

;; all-numbers? : SExpr --> Boolean
;; Takes in an s-expression and checks whether every Atom in an SExpr is a number
(check-expect (all-numbers? 5) #t)
(check-expect (all-numbers? "aloha") #f)
(check-expect (all-numbers? (list "hi" (list 3 4) 'apple (list ))) #f)
(check-expect (all-numbers? (list 1 2 (list 3 (list 4 5)))) #t)

(define (all-numbers? se)
  (cond
    [(atom? se) (number? se)]
    [(list? se) (listnumber? se)]))

;; listnumber? : SExpr --> Boolean
;; Takes in an s-expression and determines if it contains only numbers or not
(check-expect (listnumber? (list 1 2 (list 3 (list 4 5)))) #t)
(check-expect (listnumber? (list "hi" (list 3 4) 'apple (list ))) #f)
(check-expect (listnumber? empty) #f)
(define (listnumber? los)
  (cond
    [(empty? los) #f]
    [(cons? los) (andmap all-numbers? los)]))



;; Exercise 5

;; all-histrings? : SExpr --> Boolean
;; Takes in an s-expression and returns true if every atom is the string "hi" and false if it
;; contains anything else
(check-expect (all-histrings? 'bye) #f)
(check-expect (all-histrings? "hi") #t)
(check-expect (all-histrings? (list "hi" (list "bye" "hello" (list "world")))) #f)
(check-expect (all-histrings? (list "hi" "hi" (list "hi") "hi" "hi")) #t)
(define (all-histrings? se)
  (cond
    [(atom? se) (atomhi? se)]
    [(list? se) (listhi? se)]))

;; Helpers
;; atomhi? : Atom --> Boolean
;; Takes in an atom and determines if it is the string "hi"
(check-expect (atomhi? "hi") #t)
(check-expect (atomhi? "bonjourno") #f)
(check-expect (atomhi? 'library) #f)
(check-expect (atomhi? 20000) #f)
(check-expect (atomhi? #true) #f)
(define (atomhi? at)
  (cond
   [(string? at) (string=? "hi" at)]
   [else #f]))

;; listhi? : SExpr --> Boolean
;; Takes in a s-expression and determines if it contains only the string "hi"
(check-expect (all-histrings? 'bye) #f)
(check-expect (all-histrings? "hi") #t)
(check-expect (all-histrings? (list "hi" (list "bye" "hello" (list "world")))) #f)
(check-expect (all-histrings? (list "hi" "hi" (list "hi") "hi" "hi")) #t)
(define (listhi? los)
  (cond
    [(empty? los) #f]
    [(cons? los) (andmap all-histrings? los)]))



;; Abstraction Functions

;; all?-abstraction : S-Expr [X -> Boolean] [[List-of X] -> Boolean] -> Boolean
(check-expect (all?-abstraction (list "hi" (list 3 4) 'apple (list )) atomhi? listhi?) #f)
(check-expect (all?-abstraction (list 1 2 (list 3 (list 4 (list 5)) 3)) number? listnumber?) #t)
(check-expect (all?-abstraction (list 'a 'b 'c list ) number? listnumber?) #f)
(define (all?-abstraction se atomhelper listhelper)
  (cond
    [(atom? se) (atomhelper se)]
    [(list? se) (listhelper se)]))

;; all-numbers?/v2 : SExpr --> Boolean
;; Takes in an s-expression and checks whether every Atom in an SExpr is a number
(check-expect (all-numbers?/v2 5) #t)
(check-expect (all-numbers?/v2 "aloha") #f)
(check-expect (all-numbers?/v2 (list "hi" (list 3 4) 'apple (list ))) #f)
(check-expect (all-numbers?/v2 (list 1 2 (list 3 (list 4 5)))) #t)
(define (all-numbers?/v2 se)
  (all?-abstraction se number? listnumber?))


 