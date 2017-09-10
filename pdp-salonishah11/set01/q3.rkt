;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(provide image-area)

;; image-area : Image -> NonNegInt
;; GIVEN : the path of an image
;; RETURNS : the number of pixels in the given image
;; EXAMPLES :
;; (image-area (bitmap "cat.png")) => 8775
;; (image-area (bitmap "rocket.jpg")) = 1176

;; STRATEGY : combine simpler functions
(define (image-area image) (* (image-width image) (image-height image)))


;; TESTS :
(begin-for-test
  (check-equal?
   (image-area (bitmap "cat.png")) 8775 "Number of pixels in cat.png should be 8775")
  (check-equal?
   (image-area (bitmap "rocket.jpg")) 1176 "Number of pixels in rocket.jpg should be 1176"))