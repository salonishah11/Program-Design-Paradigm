;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(provide string-first)

;; string-first : String -> String
;; GIVEN : a string
;; RETURNS : the first character of the given string
;; EXAMPLES :
;; (string-first "String") => "S"
;; (string-first "This is example for Question 2") => "T"

;; STRATEGY : Combine simpler functions
(define (string-first str) (string-ith str 0))

;; TESTS :
(begin-for-test
  (check-equal?
   (string-first "String") "S")
  (check-equal?
   (string-first "This is example for Question 2") "T"))