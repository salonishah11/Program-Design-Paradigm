;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(provide string-delete)

;; string-delete : String NonNegInt -> String
;; GIVEN : a string and a number i
;; RETURNS : a string from which the ith character, from front, is deleted
;; EXAMPLES :
;; (string-delete "DeleteeCharacter" 6) => "DeleteCharacter"
;; (string-delete "Questions5" 8) => "Question5"

;; STRATEGY : combine simpler functions
(define (string-delete str i) (string-append (substring str 0 i) (substring str (+ i 1))))

;; TESTS :
(begin-for-test
  (check-equal?
   (string-delete "DeleteeCharacter" 6) "DeleteCharacter")
  (check-equal?
   (string-delete "Questions5" 8) "Question5"))