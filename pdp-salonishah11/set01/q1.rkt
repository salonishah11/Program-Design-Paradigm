;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(provide distance-to-origin)

;; DATA DEFINITION:
(define-struct point (x y))

;; CONSTRUCTOR TEMPLATE:
;; A point is (make-point Integer Integer)

;; INTERPRETATION:
;; (make-point Integer Integer) is a point where
;; x is the x-coordinate of point in cm
;; y is the y-coordinate of point in cm

;; DESTRUCTOR TEMPLATE:
;; point-fn : point -> ??
#; (define (point-fn pt)
    (... (point-x pt)
         (point-y pt)))


;; distance-to-origin : point -> PosReal
;; GIVEN : a point
;; RETURNS : the distance of the point from origin (0,0)
;; EXAMPLES :
;; (distance-to-origin (make-point 5 12)) => 13
;; (distance-to-origin (make-point -3 4)) => 5
;; (distance-to-origin (make-point -3 -4)) => 5
;; (distance-to-origin (make-point 5 -12)) => 13

;; STRATEGY : Use point template on pt 
(define (distance-to-origin pt)
  (sqrt (+ (* (point-x pt) (point-x pt)) (* (point-y pt) (point-y pt)))))

;; TESTS:
(begin-for-test
  (check-equal?
   (distance-to-origin (make-point 5 12)) 13)
  (check-equal?
   (distance-to-origin (make-point -3 4)) 5)
  (check-equal?
   (distance-to-origin (make-point -3 -4)) 5)
  (check-equal?
   (distance-to-origin (make-point 5 -12)) 13))
