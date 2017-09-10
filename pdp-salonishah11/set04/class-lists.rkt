;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; class-lists
;; Professor Felleisen and Professor Shivers each keep their class lists
;; on slips of paper, one student on each slip. Professor Felleisen maintains
;; names on yellow slip, while Professor Shivers maintains it on blue slip.
;; One fine day, the slips are mixed up.
;; The goal is to form separate lists of yellow and blue colors, with no
;; student being duplicated in the same list.

(require rackunit)
(require "extras.rkt")
(check-location "04" "class-lists.rkt")

(provide
 felleisen-roster
 shivers-roster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants

;; Booleans
(define TRUE #true)
(define FALSE #false)

;; Colors of slip
(define YELLOW "yellow")
(define BLUE "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; Structure Color :
;; A Color is one of
;; -- "yellow"
;; -- "blue"

;; INTERPRETATION :
;; "yellow" is the yellow color slip
;; "blue" is the blue color slip

;; DESTRUCTOR TEMPLATE :
;; color-fn : Color -> ??
#;(define (color-fn c)
    (...
     (string=? c "yellow")
     (string=? c "blue")
     ...))



;; Structure Slip :
(define-struct slip (color name1 name2))

;; CONSTRUCTOR TEMPLATE :
;; A Slip is a (make-slip Color String String)

;; INTERPRETATION :
;; color : it's the Color of the slip
;; name-1 & name2 : they are either the first name and last name or, last name
;;                  and first name, respectively of the student

;; DESTRUCTOR TEMPLATE :
;; slip-fn : Slip -> ??
#;(define (slip-fn s)
    (...
     (slip-color s)
     (slip-name1 s)
     (slip-name2 s)
     ...))



;; Structure ListOfSlips (LOS)
;; A  ListOfSlips (LOS) is either :
;; -- empty
;; -- (cons Slip LOS)

;; INTERPRETATION :
;; empty : a sequence of Slip with no elements
;; (cons Slip LOS) : a sequence of Slip's whose first element is Slip and
;;                   whose other elements are represented by LOS

;; DECONSTRUCTOR TEMPLATE :
;; los-fn : LOS -> ??
#;(define (los-fn los)
  (cond
    [(empty? los) ...]
    [else (...
           (slip-fn (first los))
           (los-fn (rest los)) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples
(define LOS1 (list 
              (make-slip YELLOW "Saloni" "Shah")
              (make-slip YELLOW "Ninad" "Shah")
              (make-slip BLUE "Premal" "Shah")
              (make-slip BLUE "Varsha" "Shah")
              (make-slip YELLOW "Shah" "Ninad")
              (make-slip YELLOW "Shah" "Saloni")
              (make-slip YELLOW "Premal" "Shah")
              (make-slip BLUE "Shah" "Varsha")
              (make-slip BLUE "Premal" "Shah")
              (make-slip BLUE "Shah" "Saloni")
              (make-slip YELLOW "Shah" "Saloni")))

(define LOS2 (list
              (make-slip YELLOW "Richard" "Castle")
              (make-slip YELLOW "Richard" "Castle")
              (make-slip BLUE "Kate" "Beckett")
              (make-slip YELLOW "Castle" "Richard")
              (make-slip BLUE "Beckett" "Kate")
              (make-slip YELLOW "Kevin" "Ryan")
              (make-slip BLUE "Jenny" "Ryan")))

(define FELLEISEN-ROSTER-LOS1 (cons
                               (make-slip YELLOW "Shah" "Ninad")
                               (cons (make-slip YELLOW "Premal" "Shah")
                                     (cons (make-slip YELLOW "Shah" "Saloni")
                                           empty))))

(define SHIVERS-ROSTER-LOS1 (cons
                             (make-slip BLUE "Shah" "Varsha")
                             (cons (make-slip BLUE "Premal" "Shah")
                                   (cons (make-slip BLUE "Shah" "Saloni")
                                         empty))))

(define FELLEISEN-ROSTER-LOS2 (cons
                               (make-slip YELLOW "Castle" "Richard")
                               (cons (make-slip YELLOW "Kevin" "Ryan")
                                     empty)))

(define SHIVERS-ROSTER-LOS2 (cons
                             (make-slip BLUE "Beckett" "Kate")
                             (cons (make-slip BLUE "Jenny" "Ryan")
                                   empty)))

(define LOS3 (cons
              (make-slip YELLOW "Alexis" "Castle")
              empty))

(define S1 (make-slip YELLOW "Saloni" "Shah"))
(define S3 (make-slip YELLOW "Shah" "Saloni"))
(define S4 (make-slip YELLOW "Saloni" "Shah"))

(define S2 (make-slip BLUE "Ninad" "Shah"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; felleisen-roster : ListOfSlips -> ListOfSlips
;; GIVEN : a list of slips
;; RETURNS : a list of slips containing all the students in Professor
;;          Felleisen's class, without duplication
;; EXAMPLES :
;; (felleisen-roster LOS1) => FELLEISEN-ROSTER-LOS1
;; (felleisen-roster LOS2) => FELLEISEN-ROSTER-LOS2

;; DESIGN STRATEGY : Use template for ListOfSlips on los
(define (felleisen-roster los)
  (cond
    [(empty? los) empty]
    [else
     (if (student-name-unique? YELLOW (first los) (rest los))
         (cons (first los) (felleisen-roster (rest los)))
         (felleisen-roster (rest los)))]))

;; TESTS :
(begin-for-test
  (check-equal?
   (felleisen-roster LOS1)
   FELLEISEN-ROSTER-LOS1
   "The list will contain all the yellow slips without duplication
    of student's names")
  (check-equal?
   (felleisen-roster LOS2)
   FELLEISEN-ROSTER-LOS2
   "The list will contain all the yellow slips without duplication
    of student's names"))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shivers-roster : ListOfSlips -> ListOfSlips
;; GIVEN : a list of slips
;; RETURNS : a list of slips containing all the students in Professor
;;           Shivers' class, without duplication
;; EXAMPLES :
;; (shivers-roster LOS1) => SHIVERS-ROSTER-LOS1
;;  (shivers-roster LOS2) => SHIVERS-ROSTER-LOS2

;; DESIGN STRATEGY : Use template for ListOfSlips on los
(define (shivers-roster los)
  (cond
    [(empty? los) empty]
    [else
     (if (student-name-unique? BLUE (first los) (rest los))
         (cons (first los) (shivers-roster (rest los)))
         (shivers-roster (rest los)))]))

;; TESTS :
(begin-for-test
  (check-equal?
   (shivers-roster LOS1)
   SHIVERS-ROSTER-LOS1
   "The list will contain all the blue slips without duplication
    of student's names")
  (check-equal?
   (shivers-roster LOS2)
   SHIVERS-ROSTER-LOS2
   "The list will contain all the blue slips without duplication
    of student's names"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper functions

;; Helper function for felleisen-roster and shivers-roster
;; student-name-unique? : String Slip ListOfSlips
;; GIVEN : A string which represents the color of slip, a slip and
;;         ListOfSlips
;; RETURNS : True if name1 and name1 in given slip are not duplicated
;; EXAMPLE :
;; (student-name-unique? YELLOW S1 LOS1) => FALSE

;; DESIGN STRATEGY : Use of template for Slip on s
(define (student-name-unique? col s los)
  (and (string=? col (color-of-slip s))
              (not (student-duplicated? col s los))))

;; TESTS :
(begin-for-test
  (check-equal?
   (student-name-unique? YELLOW S1 LOS1)
   FALSE
   "Since the name1 and name2 in given slip are present more than
    once in the list, #false is returned"))



;; Helper function for felleisen-roster and shivers-roster
;; color-of-slip : Slip -> String
;; GIVEN : A slip
;; RETURNS : The Color of the slip
;; EXAMPLES :
;; (color-of-slip S1) => YELLOW
;; (color-of-slip S2) => BLUE

;; DESIGN STRATEGY : Use of template for Color on s
(define (color-of-slip s)
  (cond
    [(string=? YELLOW (slip-color s))
     YELLOW]
    [(string=? BLUE (slip-color s))
     BLUE]))

;; TESTS :
(begin-for-test
  (check-equal?
   (color-of-slip S1)
   YELLOW
   "The color of the given slip is returned")
  (check-equal?
   (color-of-slip S2)
   BLUE
   "The color of the given slip is returned"))



;; Helper function for felleisen-roster and shivers-roster
;; student-duplicated? : String Slip ListOfSlips -> Boolean
;; GIVEN : The color for the slip, a slip and ListOfSlips
;; RETURNS : True if there are 2 students with the same name
;;           in ListOfSlips with color specified
;; EXAMPLES :
;; (student-duplicated? YELLOW S1 LOS1) => TRUE
;; (student-duplicated? YELLOW S1 LOS2) => FALSE

;; DESIGN STRATEGY : Use of template for ListOfSlips on los
(define (student-duplicated? col s los)
  (cond
    [(empty? los) FALSE]
    [else
     (if (and (string=? col (color-of-slip (first los)))
          (name-same? s (first los)))
         TRUE
         (student-duplicated? col s (rest los)))]))

;; TESTS :
(begin-for-test
  (check-equal?
   (student-duplicated? YELLOW S1 LOS1)
   TRUE
   "Since the student given in slip is already present in the list,
    #true is returned")
  (check-equal?
   (student-duplicated? YELLOW S1 LOS2)
   FALSE
   "Since the student given in slip is not already present in the list,
    #false is returned"))



;; Helper function for felleisen-roster and shivers-roster
;; name-same? : Slip Slip -> Boolean
;; GIVEN : Two slips, s1 and s2
;; RETURNS : True, if
;;           - name1 and name2 of s1 and s2 and vice-versa are same
;;           - or name1 and name1 of s1 and s2 and name2 and name2
;;             of s1 and s2 are same
;; EXAMPLES :
;; (name-same? S1 S3) => TRUE
;; (name-same? S1 S4) => TRUE

;; DESIGN STRATEGY : Use of template for Slip on s1 and s2
(define (name-same? s1 s2)
  (or (and (string=? (slip-name1 s1) (slip-name2 s2))
           (string=? (slip-name2 s1) (slip-name1 s2)))
      (and (string=? (slip-name1 s1) (slip-name1 s2))
           (string=? (slip-name2 s1) (slip-name2 s2)))))

;; TESTS :
(begin-for-test
  (check-equal?
   (name-same? S1 S3)
   TRUE
   "Since name1 & name2 of s1 & s2 are same and vice-versa,
    #true is returned")
  (check-equal?
   (name-same? S1 S4)
   TRUE
   "Since name1 & name1 of s1 & s2 respectively and name2 & name2
    of s1 & s2 respectively, are same #true is returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;