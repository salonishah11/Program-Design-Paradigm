;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(provide string-insert)

;; string-insert : String NonNegInt -> String
;; GIVEN : a string and number i
;; RETURNS : a string with "_" inserted at ith position in the string from front
;; EXAMPLES :
;; (string-insert "RacketProgramming" 6) => "Racket_Programming"
;; (string-insert "2Example" 1) => "2_Example"

;; STRATEGY : combine simpler functions
(define (string-insert str i) (string-append (substring str 0 i) "_" (substring str i)))

;; TESTS :
(begin-for-test
  (check-equal?
   (string-insert "RacketProgramming" 6) "Racket_Programming")
  (check-equal?
   (string-insert "2Example" 1) "2_Example"))