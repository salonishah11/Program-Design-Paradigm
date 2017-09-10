;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; outlines.rkt
;;; In the first part of the program, legality of given FlatRep is checked.
;;; In second part of program, a given Outline representation is converted to its
;;; Flat representation.

(require rackunit)
(require "extras.rkt")
(check-location "07" "outlines.rkt")

(provide
 legal-flat-rep?
 tree-rep-to-flat-rep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;;; ListOfPosInt (LOPI) : 
;;; A ListOfPosInt is either one of :
;;; -- empty
;;; -- (cons PosInt ListOfPosInt)

;;; INTERPRETATION :
;;; empty                      : a sequence of PosInt with no elements
;;; (cons PosInt ListOfPosInt) : a sequence of PosInt whose first element is a
;;;                              PosInt and whose other elements are represented
;;;                              by ListOfPosInt

;;; DESTRUCTOR TEMPLATE :
;;; lopi-fn : ListOfPosInt -> ??
#;(define (lopi-fn lopi)
    (cond
      [(empty? lopi) ...]
      [else
       (... (... (first lopi)) 
            (lopi-fn (rest lopi)))]))



;;; Structure Line :
(define-struct line (sec-num title))

;;; CONSTRUCTOR TEMPLATE :
;;; A Line is a (make-line ListOfPosInt String)

;;; INTERPRETATION :
;;; (make-line ListOfPosInt String) is a Line where :
;;; sec-num : represents the section number
;;; title   : represents the header text of the section

;;; DECONSTRUCTOR TEMPLATE :
;;; line-fn : Line -> ??
#;(define (line-fn ln)
    (... (lopi-fn (line-sec-num ln))
         (line-title ln)))



;;; ListOfLines (LOL) :
;;; A ListOfLines is either one of :
;;; -- empty
;;; -- (cons Line ListOfLines)

;;; INTERPRETATION :
;;; empty                   : a sequence of Lines with no elements
;;; (cons Line ListOfLines) : a sequence of Lines whose first element is a Line
;;;                           and whose other elements are represented by
;;;                           ListOfLines

;;; DESTRUCTOR TEMPLATE :
;;; lol-fn : ListOfLines -> ??
#;(define (lol-fn lol)
    (cond
      [(empty? lol) ...]
      [else
       (... (line-fn (first lol))
            (lol-fn (rest lol)))]))



;;; Structure Section :
(define-struct section (str secs))

;;; CONSTRUCTOR TEMPLATE :
;;; A Section is a (make-section String ListOfSections)

;;; INTERPRETATION :
;;; (make-section str secs) is a section where
;;; str  : is the header text of the section
;;; secs : is the list of subsections of the section

;;; DESTRUCTOR TEMPLATE :
;;; section-fn : Section -> ??
#;(define (section-fn sec)
    (... (section-str sec)
         (los-fn (section-secs sec))))



;;; ListOfSections (LOS) :
;;; A ListOfSections is either one of :
;;; -- empty
;;; -- (cons Section ListOfSections)

;;; INTERPRETATION :
;;; empty                         : a sequence of Sections with no elements
;;; (cons Section ListOfSections) : a sequence of Sections whose first element is a
;;;                                 Section and whose other elements are represented
;;;                                 by ListOfSections

;;; DECONSTRUCTOR TEMPLATE :
;;; los-fn : ListOfSections -> ??
#;(define (los-fn los)
    (cond
      [(empty? los) ...]
      [else
       (... (section-fn (first los))
            (los-fn (rest los)))]))



;;; An Outline is a ListOfSection
;;; A FlatRep is a ListOfLines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants for tests

(define LIST1 (list
               (make-line (list 1) "The first section")
               (make-line (list 1 1) "A subsection with no subsections")
               (make-line (list 1 2) "Another subsection")
               (make-line (list 1 2 1) "This is a subsection of 1.2")
               (make-line (list 1 2 2) "This is another subsection of 1.2")
               (make-line (list 1 3) "The last subsection of 1")
               (make-line (list 2) "Another section")
               (make-line (list 2 1) "More stuff")
               (make-line (list 2 2) "Still more stuff")))

(define LIST2 (list
               (make-line (list 1) "Section 1")
               (make-line (list 1 1) "Section 1.1")
               (make-line (list 1 1 1) "Section 1.1.1")
               (make-line (list 1 1 2) "Section 1.1.2")
               (make-line (list 1 1 2 1) "Section 1.1.2.1")
               (make-line (list 2) "Section 2")
               (make-line (list 2 1) "Section 2.1")
               (make-line (list 2 2) "Section 2.2")
               (make-line (list 2 2 1) "Section 2.2.1")
               (make-line (list 2 3) "Section 2.3")
               (make-line (list 3) "Section 3")))

(define LIST3 (list
               (make-line (list 3) "Section 1")))

(define LIST-FOR-SEQ-CHECK1
  (list
   (make-line (list 1 1 1) "Another subsection")
   (make-line (list 1 1 1 1) "This is a subsection of 1.2")
   (make-line (list 1 2) "This is another subsection of 1.2")
   (make-line (list 1 3) "The last subsection of 1")
   (make-line (list 2) "Another section")))

(define PREV-SEC-NUM1 (list 1 1))
(define CURR-SEC-NUM1 (list 1 1 1))

(define CURR-SEC-NUM2 (list 1 2))

(define CURR-SEC-NUM3 (list 2))

(define WRONG-CURR-SEC-NUM1 (list 1 1))

(define WRONG-CURR-SEC-NUM2 (list 1 1 1 1))

(define APPENDED-0-TO-PREV1 (list 1 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General Functions

;;; len-equal-less? : ((PosInt PosInt ..+) -> Boolean) ListOfPosInt ListOfPosInt
;;;                   Number -> Boolean 
;;; GIVEN : A operator to compare the length of lists, 2 lists and the
;;;         difference offset
;;; RETURNS : True, if the difference of lengths of given lists is
;;;           (a) equal to given offset, or
;;;           (b) less than the given offset

;;; DESIGN STRATEGY : Combine simpler functions
(define (len-equal-less? op lst1 lst2 offset)
  (op (- (length lst1) (length lst2))
      offset))



;;; sec-num-in-valid-order? : ListOfPosInt ListOfPosInt -> Boolean
;;; GIVEN : 2 list of numbers, curr and prev, representing the current 
;;;         and previous section numbers respectively
;;; WHERE : prev is a legal section number, and curr is the section number
;;;         occuring in the given input list after prev
;;; RETURNS : True, iff curr is a legal section number which can appear
;;;           after prev

;;; DESIGN STRATEGY : Use template for ListOfPosInt on curr and prev
(define (sec-num-in-valid-order? curr prev)
  (cond
    [(empty? (rest curr))
     (= (- (first curr) (first prev)) 1)]
    [else
     (and (= (first prev) (first curr))
          (sec-num-in-valid-order? (rest curr) (rest prev)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------- QUESTION 1 ---------------------------------

;;; legal-flat-rep? : ListOfLines -> Boolean
;;; GIVEN : A list of lines
;;; RETURNS : True iff the given list of lines is a legal flat representation
;;; EXAMPLES :
;;; (legal-flat-rep? LIST1) => true
;;; (legal-flat-rep? LIST3) => false

;;; DESIGN STRATEGY : Call a more General function sub-legal-flat-rep?
(define (legal-flat-rep? lol)
  (sub-legal-flat-rep? lol (list 0)))

;;; TESTS:
(begin-for-test
  (check-equal?
   (legal-flat-rep? LIST1)
   true
   "validates that FlatRep is valid")
  (check-equal?
   (legal-flat-rep? LIST2)
   true
   "validates that FlatRep is valid")
  (check-equal?
   (legal-flat-rep? LIST3)
   false
   "validates that FlatRep is not valid"))



;;; sub-legal-flat-rep? : ListOfLines ListOfPosInt -> Boolean
;;; GIVEN : A sub-list sublol of lol, and section number prev-sec-num
;;; WHERE : prev-sec-num is a legal section/sub-section number,
;;;         occuring before the sublol's section number 
;;; RETURNS : True, iff each sublol is a legal flat representation,
;;;           and section number of each sublol is legal section/sub-section
;;;           that can occur after its prev-sec-num
;;; EXAMPLES :
;;; (sub-legal-flat-rep? LIST-FOR-SEQ-CHECK1 PREV-SEC-NUM1) => true

;;; DESIGN STRATEGY : Use template for ListOfLines on sublol
(define (sub-legal-flat-rep? sublol prev-sec-num)
  (cond
    [(empty? sublol) true]
    [else
     (and (seq-of-sec-num-proper? (line-sec-num (first sublol))
                                  prev-sec-num)
          (sub-legal-flat-rep? (rest sublol)
                               (line-sec-num (first sublol))))]))

;;; TESTS:
(begin-for-test
  (check-equal?
   (sub-legal-flat-rep? LIST-FOR-SEQ-CHECK1 PREV-SEC-NUM1)
   true
   "lists are in valid order"))



;;; seq-of-sec-num-proper? : ListOfPosInt ListOfPosInt -> Boolean
;;; GIVEN : A list curr representing the section number whose legality
;;;         is to be checked , and another list prev, with whose reference
;;;         the legality of curr is to be checked
;;; WHERE : prev is a legal section number, and curr is the section number
;;;         occuring in the given input list after prev
;;; RETURNS : True, iff curr, is a legal section/sub-section number that can occur
;;;           after the prev
;;; EXAMPLES :
;;; (seq-of-sec-num-proper? CURR-SEC-NUM1 PREV-SEC-NUM1) => true
;;; (seq-of-sec-num-proper? WRONG-CURR-SEC-NUM1 PREV-SEC-NUM1) => false

;;; DESIGN STRATEGY : Divide into cases based on difference of length between curr 
;;;                   and prev
(define (seq-of-sec-num-proper? curr prev)
  (cond
    [(or (equal-len-of-curr-prev? curr prev)
         (len-of-curr-less-than-prev? curr prev))
     (sec-num-in-valid-order? curr prev)]
    [(len-of-curr-more-by-1? curr prev)
     (sec-num-in-valid-order? curr
                              (append-0-to-end-of-lst prev))] 
    [else false]))

;;; TESTS:
(begin-for-test
  (check-equal?
   (seq-of-sec-num-proper? CURR-SEC-NUM1 PREV-SEC-NUM1)
   true
   "sections are in valid order")
  (check-equal?
   (seq-of-sec-num-proper? CURR-SEC-NUM2 PREV-SEC-NUM1)
   true
   "sections are in valid order")
  (check-equal?
   (seq-of-sec-num-proper? CURR-SEC-NUM3 PREV-SEC-NUM1)
   true
   "sections are in valid order")
  (check-equal?
   (seq-of-sec-num-proper? WRONG-CURR-SEC-NUM1 PREV-SEC-NUM1)
   false
   "sections are in invalid order")
  (check-equal?
   (seq-of-sec-num-proper? WRONG-CURR-SEC-NUM2 PREV-SEC-NUM1)
   false
   "sections are in invalid order"))



;;; equal-len-of-curr-prev? : ListOfPosInt ListOfPosInt -> Boolean
;;; GIVEN : 2 list of positive integers, curr and prev, representing the current
;;;         and previous section numbers respectively 
;;; RETURNS : True, iff the length of curr and prev is same
;;; EXAMPLES :
;;; (equal-len-of-curr-prev? CURR-SEC-NUM2 PREV-SEC-NUM1) => true

;;; DESIGN STRATEGY : Call a more General function len-equal-less?
(define (equal-len-of-curr-prev? curr prev)
  (len-equal-less? = curr prev 0))

;;; TESTS :
(begin-for-test
  (check-equal?
   (equal-len-of-curr-prev? CURR-SEC-NUM2 PREV-SEC-NUM1)
   true
   "returns true as lists are of same length"))



;;; len-of-curr-more-by-1? : ListOfPosInt ListOfPosInt -> Boolean
;;; GIVEN : 2 list of positive integers, curr and prev, representing the current
;;;         and previous section numbers respectively
;;; RETURNS : True, iff the length of curr is more than that of prev by 1
;;; EXAMPLES :
;;; (len-of-curr-more-by-1? CURR-SEC-NUM1 PREV-SEC-NUM1) => true

;;; DESIGN STRATEGY : Call a more General function len-equal-less?
(define (len-of-curr-more-by-1? curr prev)
  (len-equal-less? = curr prev 1))

;;; TESTS :
(begin-for-test
  (check-equal?
   (len-of-curr-more-by-1? CURR-SEC-NUM1 PREV-SEC-NUM1)
   true
   "the length of new list is 1 more than previous one"))



;;; len-of-curr-less-than-prev? : ListOfPosInt ListOfPosInt -> Boolean
;;; GIVEN : 2 list of positive integers, curr and prev, representing the current
;;;         and previous section numbers respectively
;;; RETURNS : True, iff the length of curr is less than that of prev
;;; EXAMPLES :
;;; (len-of-curr-less-than-prev? CURR-SEC-NUM3 PREV-SEC-NUM1) => true

;;; DESIGN STRATEGY : Call a more General function len-equal-less?
(define (len-of-curr-less-than-prev? curr prev)
  (len-equal-less? < curr prev 0))

;;; TESTS :
(begin-for-test
  (check-equal?
   (len-of-curr-less-than-prev? CURR-SEC-NUM3 PREV-SEC-NUM1)
   true
   "the length of current is more than previous one"))



;;; append-0-to-end-of-lst : ListOfPosInt -> ListOfPosInt
;;; GIVEN : A list of positive integers
;;; RETURNS : A list of positive integers, with 0 appended at the
;;;           end of the given list
;;; EXAMPLES :
;;; (append-0-to-end-of-lst PREV-SEC-NUM1) => APPENDED-0-TO-PREV1

;;; DESIGN STRATEGY : Combine simpler function
(define (append-0-to-end-of-lst lst)
  (append lst (list 0)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (append-0-to-end-of-lst PREV-SEC-NUM1)
   APPENDED-0-TO-PREV1
   "a list with 0 appended in end"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants for tests

(define OUTLINE1
  (list 
   (make-section
    "The first section"
    (list
     (make-section "A subsection with no subsections" empty)
     (make-section "Another subsection"
                   (list
                    (make-section "This is a subsection of 1.2" empty)
                    (make-section "This is another subsection of 1.2" empty)))
     (make-section "The last subsection of 1" empty)))
   (make-section
    "Another section"
    (list
     (make-section "More stuff" empty)
     (make-section "Still more stuff" empty)))))

(define LIST-SECTION2
  (list
   (make-section "Another section"
                 (list
                  (make-section "More stuff" empty)
                  (make-section "Still more stuff" empty)))))

(define PREV-SEC-NUM-FOR-SECTION2 (list 1))

(define SECTION2-TO-FLAT-REP
  (list (make-line (list 2) "Another section")
        (make-line (list 2 1) "More stuff")
        (make-line (list 2 2) "Still more stuff")))

(define SECTION2
  (make-section "Another section"
                (list
                 (make-section "More stuff" empty)
                 (make-section "Still more stuff" empty))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------- QUESTION 2 -------------------------------

;;; tree-rep-to-flat-rep : Outline -> FlatRep
;;; GIVEN : An Outline
;;; RETURNS : A FlatRep of the given Outline
;;; EXAMPLES :
;;; (tree-rep-to-flat-rep OUTLINE1) => LIST1

;;; DESIGN STRATEGY : Call a more General function sub-tree-rep-to-flat-rep
(define (tree-rep-to-flat-rep los)
  (sub-tree-rep-to-flat-rep los (list 0)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-rep-to-flat-rep OUTLINE1)
   LIST1
   "The given Outline is converted to its FlapRep"))



;;; sub-tree-rep-to-flat-rep : ListOfSections ListOfPosInt -> ListOfLines
;;; GIVEN : A sub-list sublos of los, and section number sec-num
;;; WHERE : sec-num is the section number of the section/sub-section
;;;         which occured before sublos
;;; RETURNS : A ListOfLines, corresponding to the given sublos
;;; EXAMPLES :
;;; (sub-tree-rep-to-flat-rep LIST-SECTION2 PREV-SEC-NUM-FOR-SECTION2)
;;;   => SECTION2-TO-FLAT-REP

;;; DESIGN STRATEGY : Use template for ListOfSections on sublos
(define (sub-tree-rep-to-flat-rep sublos sec-num)
  (cond
    [(empty? sublos) empty]
    [else
     (append (section-to-line (first sublos)
                              (next-sec-num (reverse sec-num)))
             (sub-tree-rep-to-flat-rep (rest sublos)
                                       (next-sec-num (reverse sec-num))))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (sub-tree-rep-to-flat-rep LIST-SECTION2
                             PREV-SEC-NUM-FOR-SECTION2)
   SECTION2-TO-FLAT-REP
   "The given sub-list of Outline is converted to its FlatRep"))



;;; next-sec-num : ListOfPosInt -> ListOfPosInt
;;; GIVEN : A list of numbers, representing the section number
;;; WHERE : the lst, is a legal section number
;;; RETURNS : A list of numbers, with 1 added to the last element of the
;;;           given section number, creating a new section/sub-section  
;;; EXAMPLES :
;;; (next-sec-num PREV-SEC-NUM1) => CURR-SEC-NUM2

;;; DESIGN STRATEGY : Combine simpler functions
(define (next-sec-num lst)
  (reverse (cons (+ (first lst) 1)
                 (rest lst))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (next-sec-num PREV-SEC-NUM1)
   CURR-SEC-NUM2
   "The next section number of given one is computed"))



;;; section-to-line : Section ListOfPosInt -> ListOfLines
;;; GIVEN : A Section sec and sec-num which is the section number
;;;         for the given section
;;; RETURNS : A ListOfLines, corresponding to the given section and
;;;           its sub-sections
;;; EXAMPLES :
;;; (section-to-line SECTION2 CURR-SEC-NUM3) => SECTION2-TO-FLAT-REP

;;; DESIGN STRATEGY : Use template for Section on sec
(define (section-to-line sec sec-num)
  (cons (make-line sec-num (section-str sec))
        (sub-tree-rep-to-flat-rep (section-secs sec)
                                  (append-0-to-end-of-lst sec-num))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (section-to-line SECTION2 CURR-SEC-NUM3)
   SECTION2-TO-FLAT-REP
   "The given section and its subsection are converted into their FlatRep"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

