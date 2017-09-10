;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rainfall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; rainfall.rkt
;;; The program consumes a list of numbers representing daily rainfall amounts .
;;; The list may contain the number -999 indicating the end of the data of interest.
;;; The average of the non-negative values in the list up to the first -999
;;; (if it shows up) is calculated.

(require rackunit)
(require "extras.rkt")
(check-location "07" "rainfall.rkt")

(provide rainfall)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS :

;;; ListOfNumbers (LON) :
;;; A ListOfNumbers is either one of :
;;; -- empty
;;; -- (cons Number ListOfNumbers)

;;; INTERPRETATION :
;;; empty                       : a sequence of Numbers with no elements
;;; (cons Number ListOfNumbers) : a sequence of Numbers whose first element is a
;;;                               Number and whose other elements are represented 
;;;                               by ListOfNumbers

;;; DESTRUCTOR TEMPLATE :
;;; lon-fn : ListOfNumbers -> ??
#;(define (lon-fn lon)
    (cond
      [(empty? lon) ...]
      [else
       (... (... (first lon)))
       (lon-fn (rest lon))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS :

;;; Value when useful data ends :
(define END-OF-USEFUL-DATA -999)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants for tests

;;; Positive values
(define POSVAL1 130)
(define POSVAL2 123)
(define POSVAL3 643)
(define POSVAL4 123)
(define POSVAL5 541)

;;; Negtive values
(define NEGVAL1 -347)
(define NEGVAL2 -23)
(define NEGVAL3 -493)

;;; Sample Lists
(define LIST1 (list POSVAL1 NEGVAL1 POSVAL2 POSVAL4 NEGVAL2 POSVAL3 POSVAL1
                    NEGVAL3 POSVAL5 POSVAL1))
(define LIST2 (list POSVAL1 NEGVAL1 POSVAL2 END-OF-USEFUL-DATA POSVAL4 NEGVAL2
                    POSVAL3 POSVAL1 NEGVAL3 POSVAL5 POSVAL1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General function

;;; update-sum-count : Number Number Number -> Number
;;; GIVEN : 3 numbers num, val and add-offset 
;;; RETURNS : An updated value of num based on whether
;;;           (a) val is non-negative or not
;;;           (b) if val is non-negative, add-offset is added to num 

;;; DESIGN STRATEGY : Divide into cases based on whether val is
;;;                   non-negative or not
(define (update-sum-count val num add-offset)
  (if (>= val 0)
      (+ num add-offset)
      num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rainfall : ListOfNumbers -> Number
;;; GIVEN : A list of numbers representing the daily rainfall amounts
;;; RETURNS : The average of the non-negative values in the list up to
;;;          the first -999 (if it shows up)
;;; EXAMPLES :
;;; (rainfall LIST1) => 260
;;; (rainfall LIST2) => 126.5
;;; (rainfall (list END-OF-USEFUL-DATA)) => 0

;;; DESIGN STRATEGY : Call a more General function calculate-avg
(define (rainfall lon)
  (calculate-avg lon 0 0))

;;; TESTS :
(begin-for-test
  (check-equal?
   (rainfall LIST1)
   260
   "computed average for all valid inputs")
  (check-equal?
   (rainfall LIST2)
   126.5
   "computed average for all valid inputs"))



;;; calculate-avg : ListOfNumbers Number NonNegInt -> Number
;;; GIVEN : A sub-list sublon of lon, sum of all the valid inputs so far
;;;         and count of all the valid inputs
;;; WHERE : sum-so-far and count are the sum and length of all valid inputs
;;;         respectively, which have occured above sublon  
;;; RETURNS : The average of the valid inputs in sublon based on the values of
;;;           sum-so-far and count
;;; EXAMPLE :
;;; (calculate-avg LIST1 0 0) => 260
;;; (calculate-avg LIST2 0 0) => 126.5
;;; (calculate-avg '() 0 0) => 0
;;; (calculate-avg (list END-OF-USEFUL-DATA) 0 0) => 0

;;; DESIGN STRATEGY : Use template for ListOfNumbers on sublon
(define (calculate-avg sublon sum-so-far count)
  (cond
    [(empty? sublon) (output-at-end-of-data sum-so-far count)] 
    [else
     (if (= (first sublon) END-OF-USEFUL-DATA)
         (output-at-end-of-data sum-so-far count)
         (calculate-avg (rest sublon)
                        (update-sum (first sublon) sum-so-far)
                        (update-count (first sublon) count)))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (calculate-avg LIST1 0 0)
   260
   "computed average for all valid inputs")
  (check-equal?
   (calculate-avg LIST2 0 0)
   126.5
   "computed average for all valid inputs")
  (check-equal?
   (calculate-avg '() 0 0)
   0
   "computed average for all valid inputs")
  (check-equal?
   (calculate-avg (list END-OF-USEFUL-DATA) 0 0)
   0
   "computed average for all valid inputs"))



;;; update-sum : Number Number -> Number
;;; GIVEN : 2 numbers, num and sum, where sum is updated based on the value of num
;;; RETURNS : A number, representing the updated sum
;;; EXAMPLES :
;;; (update-sum 23 145) => 168

;;; DESIGN STRATEGY : Call a more general function update-sum-count
(define (update-sum num sum)
  (update-sum-count num sum num))

;;; TESTS :
(begin-for-test
  (check-equal?
   (update-sum 23 145)
   168
   "updates sum as its valid")
  (check-equal?
   (update-sum -23 145)
   145
   "does not update sum as its invalid"))



;;; update-count : Number PosInt -> PosInt
;;; GIVEN : A number num and positive integer ct, where ct is updated
;;;         based on the value of num
;;; RETURNS : A positive integer, representing the updated ct
;;; EXAMPLES :
;;; (update-count 23 15) = 16

;;; DESIGN STRATEGY : Call a more general function update-sum-count
(define (update-count num ct)
  (update-sum-count num ct 1))

;;; TESTS :
(begin-for-test
  (check-equal?
   (update-count 23 15)
   16
   "updates count as its valid")
  (check-equal?
   (update-count -23 15)
   15
   "does not update count as its invalid"))



;;; output-at-end-of-data : Number NonNegInt -> Number
;;; GIVEN : A number representing the sum of valid inputs, and a non-negative 
;;;         value representing the count of valid inputs
;;; RETURNS : A number, representing the average
;;; EXAMPLES :
;;; (output-at-end-of-data 1820 7) => 260

;;; DESIGN STRATEGY : Divide into cases based whether n is 0 or not        
(define (output-at-end-of-data sum n)
  (if (= n 0)
      0
      (/ sum n)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (output-at-end-of-data 1820 7)
   260
   "Sum divide by n is returned"))

