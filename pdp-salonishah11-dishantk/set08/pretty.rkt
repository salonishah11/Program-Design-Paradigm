;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; pretty.rkt
;;; The program renders the expression based on the given width.

(require rackunit)
(require "extras.rkt")
(check-location "08" "pretty.rkt")

(provide
 expr-to-strings
 make-sum-exp
 sum-exp-exprs
 make-diff-exp
 diff-exp-exprs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS:

;;; Expr:
;;; An Expr is one of
;;; -- Integer
;;; -- (make-sum-exp NELOExpr)
;;; -- (make-diff-exp NELOExpr)

;;; INTERPRETATION :
;;; -- Integer : is the number in expression
;;; -- a sum-exp represents a sum calculation
;;; -- a diff-exp represents a difference calculation

;;; DESTRUCTOR TEMPLATE :
;;; expr-fn : Expr -> ??
#;(define (expr-fn e)
    (cond
      [(integer? e)...]
      [(sum-exp? e)...]
      [(diff-exp? e)...]))



;;; ListOfExpr (LOE) :
;;; A LOExpr is one of
;;; -- empty
;;; -- (cons Expr LOExpr)

;;; INTERPRETATION : 
;;; empty              : a sequence of Expr with no elements
;;; (cons Expr LOExpr) : a sequence of Expr whose first element is a
;;;                      Expr and whose other elements are
;;;                      represented by LOExpr

;;; DESTRUCTOR TEMPLATE :
;;; loe-fn : LOE -> ??
#;(define (loe-fn loe)
    (cond
      [(empty? loe)...]
      [else
       (... (expr-fn (first loe))
            (loe-fn (rest loe)))]))



;;; A NonEmptyListOfExpr (NELOExpr) :
;;; A NELOExpr is one of
;;; -- (cons Expr empty)
;;; -- (cons Expr NELOExpr)

;;; INTERPRETATION :
;;; (cons Expr empty)    : a sequence of Expr with 1 element
;;; (cons Expr NELOExpr) : a sequence of Expr whose first element is a
;;;                        Expr and whose other elements are
;;;                        represented by LOExpr

;;; DESTRUCTOR TEMPLATE :
;;; neloe-fn : NELOExpr -> ??
#;(define (neloe-fn neloe)
    (cond
      [(empty? (rest neloe))...]
      [else
       (... (expr-fn (first neloe))
            (neloe-fn (rest neloe)))]))



;;; ListOfString (LOS) :
;;; A LOS is one of
;;; -- empty
;;; -- (cons String LOS)

;;; INTERPRETATION : 
;;; empty             : a sequence of String with no elements
;;; (cons String LOS) : a sequence of String whose first element is a
;;;                     String and whose other elements are represented
;;;                     by LOS

;;; DESTRUCTOR TEMPLATE :
;;; los-fn : LOS -> ??
#;(define (los-fn)
    (cond
      [(empty? los)...]
      [else
       (... (... (first los))
            (los-fn (rest los)))]))



;;; A NonEmptyListOfString (NELOS) :
;;; A NELOS is one of
;;; -- (cons String empty)
;;; -- (cons String NELOS)

;;; INTERPRETATION : 
;;; (cons String empty)   : a sequence of String with 1 element
;;; (cons String NELOS)   : a sequence of String whose first element is a
;;;                         String and whose other elements are represented
;;;                         by LOS

;;; DESTRUCTOR TEMPLATE :
;;; nelos-fn : NELOS -> ??
#;(define (nelos-fn)
    (cond
      [(empty? (rest nelos)) ...]
      [else
       (... (... (first nelos))
            (nelos-fn (rest nelos)))]))



;;; Structure Sum-Exp :
(define-struct sum-exp (exprs))

;;; CONSTRUCTOR TEMPLATE :
;;; A Sum-Exp is (make-sum-exp NELOExpr)

;;; INTERPRETATION :
;;; (make-sum-exp NELOExpr) is a Sum-Exp where :
;;; exprs : represents a non-empty list of expressions

;;; DESTRUCTOR TEMPLATE :
;;; sum-expr-fn : Sum-Exp -> ??
#;(define (sum-exp-fn se)
    (...
     (sum-exp-exprs se)))



;;; Structure Diff-Exp :
(define-struct diff-exp (exprs))

;;; CONSTRUCTOR TEMPLATE :
;;; A Diff-Exp is (make-diff-exp NELOExpr)

;;; INTERPRETATION :
;;; (make-diff-exp NELOExpr) is a Diff-Exp where :
;;; exprs : represents a non-empty list of expressions

;;; DESTRUCTOR TEMPLATE :
;;; diff-expr-fn : Diff-Expr -> ??
#;(define (diff-exp-fn de)
    (...
     (diff-exp-exprs de)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS :

;;; default indent
(define DEF-IND "   ")

;;; operator
(define SUM-OP "(+ ")
(define DIFF-OP "(- ")

;;; constants for tests
(define HW-EXAMPLE-1 (make-sum-exp (list 22 333 44)))
(define HW-EXAMPLE-1-W-15 (list "(+ 22 333 44)"))
(define HW-EXAMPLE-1-W-10 (list "(+ 22" "   333" "   44)"))

(define HW-EXAMPLE-2 (make-sum-exp
                      (list
                       (make-diff-exp (list 22 3333 44))
                       (make-diff-exp
                        (list
                         (make-sum-exp (list 66 67 68))
                         (make-diff-exp (list 42 43))))
                       (make-diff-exp (list 77 88)))))

(define HW-EXAMPLE-2-W-100
  (list "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))"))

(define HW-EXAMPLE-2-W-50
  (list "(+ (- 22 3333 44)"
        "   (- (+ 66 67 68) (- 42 43))"
        "   (- 77 88))"))

(define HW-EXAMPLE-2-W-20
  (list "(+ (- 22 3333 44)"
        "   (- (+ 66 67 68)"
        "      (- 42 43))"
        "   (- 77 88))"))

(define HW-EXAMPLE-2-W-15
  (list "(+ (- 22"
        "      3333"
        "      44)"
        "   (- (+ 66"
        "         67"
        "         68)"
        "      (- 42"
        "         43))"
        "   (- 77 88))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; expr-to-strings : Expr NonNegInt -> ListOfString
;;; GIVEN : An expression and a width
;;; RETURNS : A representation of the expression as a sequence of lines, with
;;;           each line represented as a string of length not greater than the
;;;           width. It may also return an error if the expression doesn't fit
;;;           given width
;;; EXAMPLES :
;;; (expr-to-strings (make-sum-exp (list 5 6)) 10) = (list "(+ 5 6)")
;;; (expr-to-strings (make-sum-exp (list 5 6)) 5) = (list "(+ 5" "   6)")
;;; (expr-to-strings (make-sum-exp (list 5 6)) 3) = ERROR ("not enough room")

;;; DESIGN STRATEGY : Call a more General function expr-to-los
(define (expr-to-strings exp w)
  (expr-to-los exp w 0))

;;; TESTS :
(begin-for-test
  (check-equal?
   (expr-to-strings (make-sum-exp (list 5 6)) 10)
   (list "(+ 5 6)")
   "output fits in the specified width")
  (check-equal?
   (expr-to-strings (make-sum-exp (list 5 6)) 5)
   (list "(+ 5" "   6)")
   "output fits in the specified width after breaking input sub-expressions")
  (check-error
   (expr-to-strings (make-sum-exp (list 5 6)) 3)
   "not enough room")
  (check-equal?
   (expr-to-strings HW-EXAMPLE-1 15)
   HW-EXAMPLE-1-W-15
   "output fits in the specified width")
  (check-equal?
   (expr-to-strings HW-EXAMPLE-1 10)
   HW-EXAMPLE-1-W-10
   "output fits in the specified width after breaking input sub-expressions")
  (check-equal?
   (expr-to-strings HW-EXAMPLE-2 100)
   HW-EXAMPLE-2-W-100
   "output fits in the specified width")
  (check-equal?
   (expr-to-strings HW-EXAMPLE-2 50)
   HW-EXAMPLE-2-W-50
   "output fits in the specified width after breaking input sub-expressions")
  (check-equal?
   (expr-to-strings HW-EXAMPLE-2 20)
   HW-EXAMPLE-2-W-20
   "output fits in the specified width after breaking input sub-expressions")
  (check-equal?
   (expr-to-strings HW-EXAMPLE-2 15)
   HW-EXAMPLE-2-W-15
   "output fits in the specified width after breaking input sub-expressions"))



;;; expr-to-los : Expr NonNegInt NonnegInt -> NELOS
;;; GIVEN : an Expr, specified width and indentation level
;;; WHERE : ind is the indentation level of current expression at hand
;;; RETURNS : a non-empty list of Expr consisting Strings for each line in display
;;; EXAMPLES : 
;;; (expr-to-los 3 10 0) = (list "3")
;;; (expr-to-los (make-sum-exp (list 3 5)) 10 0) = (list "(+ 3 5)")
;;; (expr-to-los (make-diff-exp (list 3 5)) 10 0) = (list "(- 3 5)")
;;; (expr-to-los (make-diff-exp (list 3 5)) 5 0) = (list "(- 3" "   5)")
;;; (expr-to-los (make-diff-exp (list 3 5)) 2 0) = ERROR ("not enough room")

;;; DESIGN STRATEGY : Use template for Expr on exp 
(define (expr-to-los exp w ind)
  (cond
    [(integer? exp)
     (exp-to-los-after-int exp w ind)]
    [(sum-exp? exp)
     (exp-to-los-after-sum-diff-exp exp w ind SUM-OP sum-exp-exprs)]
    [(diff-exp? exp)
     (exp-to-los-after-sum-diff-exp exp w ind DIFF-OP diff-exp-exprs)]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (expr-to-los 3 10 0)
   (list "3")
   "outputs non empty list of string")
  (check-equal?
   (expr-to-los (make-sum-exp (list 3 5)) 10 0)
   (list "(+ 3 5)")
   "outputs non empty list of string")
  (check-equal?
   (expr-to-los (make-diff-exp (list 3 5)) 10 0)
   (list "(- 3 5)")
   "outputs non empty list of string")
  (check-equal?
   (expr-to-los (make-diff-exp (list 3 5)) 5 0)
   (list "(- 3" "   5)")
   "outputs non empty list of string")
  (check-error
   (expr-to-los (make-diff-exp (list 3 5)) 2 0)
   "not enough room"))



;;; fits-in? : NonNegInt NonNegInt NonNegInt -> Boolean
;;; GIVEN : A length of a String, width given by the user and indentation level
;;; RETURNS : True if the String, after indenting, fits in the width else false
;;; EXAMPLES :
;;; (fits-in? 5 15 3) = true
;;; (fits-in? 5 15 4) = false

;;; DESIGN STRATEGY : Combine simpler function
(define (fits-in? len w ind)
  (< len (- w (* 3 ind))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (fits-in? 5 15 3)
   true
   "a string of length 5 would fit in 15 characters with indentation of 3")
  (check-equal?
   (fits-in? 5 15 4)
   false
   "a string of length 5 would fit in 15 characters with indentation of 3"))



;;; helper function for expr-to-los
;;; exp-to-los-after-int : Expr NonNegInt NonnegInt -> NELOS
;;; GIVEN : An Expr, specified width and indentation level
;;; WHERE : ind is the indentation level of current expression at hand
;;; RETURNS : A NELOS consisting an Integer converted into a String. If this
;;;           String doesn't fits in width then it raises an error
;;; EXAMPLES :
;;; (expr-to-los 3 10 0) = (list "3")
;;; (expr-to-los 3232 2 0) = ERROR("not enough room")

;;; DESIGN STRATEGY : Divide cases based on whether string fits into the given width
(define (exp-to-los-after-int exp w ind)
  (local
    ((define str (number->string exp))
     (define str-len (string-length str)))
    (if (fits-in? str-len w ind)
        (list str)
        (error "not enough room"))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (exp-to-los-after-int 3 10 0)
   (list "3")
   "outputs non empty list of string")
  (check-error
   (exp-to-los-after-int 3232 2 0)
   "not enough room"))



;;; helper function for expr-to-los
;;; exp-to-los-after-sum-diff-exp : Expr NonNegInt NonnegInt String
;;;                                 (Expr -> NELOExpr) -> NELOS
;;; GIVEN : An Expr, specified width, indentation level, operator used and
;;;         function used to extract values from Expr
;;; WHERE : ind is the indentation level of current expression at hand and exp
;;;         will always be either sum-exp or diff-exp
;;; RETURNS : a non-empty list of Expr consisting Strings for each line in display
;;; EXAMPLES :
;;; (expr-to-los (make-sum-exp (list 3 5)) 10 0 SUM-OP sum-exp-exprs)
;;;   = (list "(+ 3 5)")
;;; (expr-to-los (make-diff-exp (list 3 5)) 10 0 DIFF-OP diff-exp-exprs)
;;;   = (list "(- 3 5)")
;;; (expr-to-los (make-diff-exp (list 3 5)) 5 0 DIFF-OP diff-exp-exprs)
;;;   = (list "(- 3" "   5)")
;;; (expr-to-los (make-diff-exp (list 3 5)) 2 0 DIFF-OP diff-exp-exprs)
;;;   = ERROR ("not enough room")

;;; DESIGN STRATEGY : Divide cases based on whether string fits into the given width
(define (exp-to-los-after-sum-diff-exp exp w ind op fn)
  (local
    ((define str (get-str-rep op (fn exp) w ind))
     (define str-len (string-length str)))
    (if (fits-in? str-len w ind)
        (list str)
        (add-closing-parenthesis (break-exp exp w (+ 1 ind) op fn) w))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (exp-to-los-after-sum-diff-exp (make-sum-exp (list 3 5)) 10 0 SUM-OP sum-exp-exprs)
   (list "(+ 3 5)")
   "outputs non empty list of string")
  (check-equal?
   (exp-to-los-after-sum-diff-exp (make-diff-exp (list 3 5)) 10 0 DIFF-OP diff-exp-exprs)
   (list "(- 3 5)")
   "outputs non empty list of string")
  (check-equal?
   (exp-to-los-after-sum-diff-exp (make-diff-exp (list 3 5)) 5 0 DIFF-OP diff-exp-exprs)
   (list "(- 3" "   5)")
   "outputs non empty list of string")
  (check-error
   (exp-to-los-after-sum-diff-exp (make-diff-exp (list 3 5)) 2 0 DIFF-OP diff-exp-exprs)
   "not enough room"))



;;; helper function for expr-to-los
;;; get-str-rep : String NELOExpr NonNegInt NonNegInt -> String
;;; GIVEN : An operator, non empty list of expressions, width and current
;;;         indentation level
;;; RETURNS : A string representating the given Sum-Exp or Diff-Exp
;;; EXAMPLES :
;;; (get-str-rep SUM-OP (list 3 5) 10 0) = "(+ 3 5)"
;;; (get-str-rep DIFF-OP (list 3 5) 10 0) = "(- 3 5)"

;;; DESIGN STRATEGY : Combine simpler functions
(define (get-str-rep op exps w ind)
  (string-append op
                 (list-of-expr-to-string exps w ind "")))

;;; TESTS :
(begin-for-test
  (check-equal? (get-str-rep SUM-OP (list 3 5) 10 0)
                "(+ 3 5)"
                "returns the list contaning string of given Expr")
  (check-equal? (get-str-rep DIFF-OP (list 3 5) 10 0)
                "(- 3 5)"
                "returns the list contaning string of given Expr"))



;;; helper function for expr-to-los
;;; list-of-expr-to-string : NELOExpr NonNegInt NonNegInt String -> String
;;; GIVEN : A non empty list of Expr, width, level of indentation and string so far
;;; WHERE : str is the String so far
;;; RETURNS : A string of the given list of Expr which fits in width, if it doesn't,
;;;           then error
;;; HALTING MEASURE : Length of NELOExpr
;;; TERMINATION CONDITION :
;;; the length of NELOExpr is reduced at each recursive call by operating
;;; on its first elements and the recuring on rest of its elements
;;; EXAMPLES :
;;; (list-of-expr-to-string (list 3 5) 10 0 "") = "3 5)"
;;; (list-of-expr-to-string (list 3 5) 1 0 "") = ERROR ("not enough room")

;;; DESIGN STRATEGY : Recur using create-string
(define (list-of-expr-to-string exps w ind str)
  (cond
    [(empty? (rest exps))
     (create-string str exps w ind ")")]
    [else
     (list-of-expr-to-string (rest exps) w ind
                             (create-string str exps w ind " "))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-expr-to-string (list 3 5) 10 0 "")
   "3 5)"
   "create a string for the list of expressions")
  (check-error
   (list-of-expr-to-string (list 3 5) 1 0 "")
   "not enough room"))



;;; helper function for list-of-expr-to-string
;;; create-string : String NELOExpr NonNegInt NonNegInt String -> String
;;; GIVEN : String so far, non empty list of Expr, width, current level
;;;         indentation character to be appended at the end
;;; RETURNS : A String with first of Expr appended with the terminating character
;;; EXAMPLES :
;;; (create-string "3 " (list 5 6 7) 10 0 " ") = "3 5 "
;;; (create-string "3 " (list 5) 10 0 ")") = "3 5)"

;;; DESIGN STRATEGY : Combine simpler functions
(define (create-string str exp w ind char)
  (string-append str
                 (first (expr-to-los (first exp) w ind))
                 char))

;;; TESTS :
(begin-for-test
  (check-equal?
   (create-string "3 " (list 5 6 7) 10 0 " ")
   "3 5 "
   "appended first of list to the string and ended it with the character given")
  (check-equal?
   (create-string "3 " (list 5) 10 0 ")")
   "3 5)"
   "appended first of list to the string and ended it with the character given"))



;;; helper function for expr-to-los
;;; break-exp : Expr NonNegInt NonNegInt String (Expr -> NELOExpr) -> NELOS
;;; GIVEN : An Expr, width, current indentation level, operator and function used
;;;         extract values from Expr
;;; WHERE : ind is the indentation level of current expression at hand and exp
;;;         will always be either sum-exp or diff-exp
;;; RETURNS : A non empty list of strings representing each line in the output
;;; HALTING MEASURE : Length of (fn exp)
;;; TERMINATION CONDITION :
;;; The output of this function is composed of three parts. First part is the
;;; first element of NELOS obtained using expr-to-los. Second part is the rest
;;; of NELOS obtained using expr-to-los. Third part is LOS obtained from recuring
;;; on rest of the (fn exp). As the length of NELOExpr reduces by 1 everytime,
;;; Halting measure stands valid.
;;; EXAMPLES :
;;; (break-exp (make-sum-exp (list 3 5)) 5 1 SUM-OP sum-exp-exprs)
;;;   = (list "(+ 3" "   5")
;;; (break-exp (make-sum-exp (list 3 5)) 6 1 DIFF-OP sum-exp-exprs)
;;;   = (list "(- 3" "   5")

;;; DESIGN STARTEGY : Recur on expr-to-los
(define (break-exp exp w ind op fn)
  (append
   (list (string-append op (first (expr-to-los (first (fn exp)) w ind))))
   (append-spaces (rest (expr-to-los (first (fn exp)) w ind)))
   (break-exp-in-list (rest (fn exp)) w ind '())))

;;; TESTS :
(begin-for-test
  (check-equal?
   (break-exp (make-sum-exp (list 3 5)) 5 1 SUM-OP sum-exp-exprs)
   (list "(+ 3" "   5")
   "returns list of strings representating broken expression")
  (check-equal?
   (break-exp (make-sum-exp (list 3 5)) 6 1 DIFF-OP sum-exp-exprs)
   (list "(- 3" "   5")
   "returns list of strings representating broken expression"))



;;; helper function for break-exp
;;; break-exp-in-list : NELOExpr NonNegInt NonNegInt LOS
;;; GIVEN : A non empty list of expressions, width, level of indentation and list
;;;         so far
;;; WHERE : lst is the list constructed so far
;;; RETURNS : A non empty list of strings representing each line in the output if
;;;           each of them can fit in specified width else raises an error
;;; HALTING MEASURE : Length of NELOExpr
;;; TERMINATION CONDITION :
;;; the length of NELOExpr is reduced at each recursive call by operating
;;; on its first elements and the recuring on rest of its elements
;;; EXAMPLES :
;;; (break-exp-in-list (list 5 6 7) 10 1 '()) = (list "   5" "   6" "   7")

;;; DESIGN STRATEGY : Recur using create-element
(define (break-exp-in-list exps w ind lst)
  (cond
    [(empty? (rest exps))
     (create-element lst (first exps) w ind)]
    [else
     (break-exp-in-list (rest exps)
                        w ind
                        (create-element lst (first exps) w ind))]))

;;; TESTS:
(begin-for-test
  (check-equal?
   (break-exp-in-list (list 5 6 7) 10 1 '())
   (list "   5" "   6" "   7")
   "output broken into different strings"))



;;; helper function for break-exp-in-list
;;; create-element : LOS NELOExpr NonNegInt NonNegInt -> NELOS
;;; GIVEN : A list being constructed, Expr, width, current level of indentation
;;; RETURNS : An updated instance of list being constructed by appending Expr in
;;;           string format
;;; EXAMPLES :
;;; (create-element (list "   3" "   5") 6 10 1) = (list "   3" "   5" "   6")

;;; DESIGN STRATEGY : Combine simpler function
(define (create-element lst exp w ind)
  (append lst
          (append-spaces (expr-to-los exp w ind))))

;;; TESTS:
(begin-for-test
  (check-equal?
   (create-element (list "   3" "   5") 6 10 1)
   (list "   3" "   5" "   6")
   "appended element at the end"))



;;; helper function for create-element
;;; append-spaces : NELOS -> NELOS
;;; GIVEN : A non empty list of string
;;; RETURNS : A non empty list of string after prefixing DEF-IND before
;;;           each element
;;; EXAMPLES :
;;; (append-spaces (list "3" "4" "5")) = (list "   3" "   4" "   5")

;;; DESIGN STRATEGY : Use HOF map on exp
(define (append-spaces exp)
  (map
   ;;; String -> String
   ;;; GIVEN : a string for exp
   ;;; RETURNS : the same string after prefixing DEF-IND 
   (lambda (x) (string-append DEF-IND x))
   exp))

;;; TEST :
(begin-for-test
  (check-equal?
   (append-spaces (list "3" "4" "5"))
   (list "   3" "   4" "   5")
   "outputs similar list with spaces appended"))



;;; helper function for append-spaces
;;; add-closing-parenthesis : NELOExpr NonNegInt -> NELOExpr
;;; GIVEN : A non empty list of expression and width
;;; RETURNS : A non empty list of strings with closing paranthesis at end, if
;;;           any of these strings does not fit inside width, then error
;;; EXAMPLES :
;;; (add-closing-parenthesis (list "   5" "   6") 10) = (list "   5" "   6)")
;;; (add-closing-parenthesis (list "   5" "   6") 4) = ERROR ("not enough room")

;;; DESIGN STRATEGY : Use template for NELOExpr on exp
(define (add-closing-parenthesis exp w)
  (cond
    [(empty? (rest exp))
     (check-len (list (string-append (first exp) ")")) w)]
    [else
     (check-len (append (list (first exp))
                        (add-closing-parenthesis (rest exp) w)) w)]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (add-closing-parenthesis (list "   5" "   6") 10)
   (list "   5" "   6)")
   "formated output which fits in the given width")
  (check-error
   (add-closing-parenthesis (list "   5" "   6") 4)
   "not enough room"))



;;; helper function for add-closing-parenthesis
;;; check-len : ListOfString NonNegInt -> ListOfString
;;; GIVEN : A ListOfString and width
;;; RETURNS : A ListOfString if first element of it can fit into
;;;           width else error
;;; EXAMPLES :
;;; (check-len (list "   3" "   4)") 5) = (list "   3" "   4)")
;;; (check-len (list "   3" "   4)") 3) = ERROR ("not enough room")

;;; DESIGN STRATEGY : Divide on cases based on string length and width
(define (check-len lst w)
  (if (<= (string-length (first lst)) w)
      lst
      (error "not enough room")))

;;; TESTS :
(begin-for-test
  (check-equal?
   (check-len (list "   3" "   4)") 5)
   (list "   3" "   4)")
   "outputs list as first element fits in the list")
  (check-error
   (check-len (list "   3" "   4)") 3)
   "not enough room"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;