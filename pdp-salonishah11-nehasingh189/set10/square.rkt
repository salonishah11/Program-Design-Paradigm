;; square.rkt
;; This file contains Square% class and make-square-toy function.
;; It is included in toys.rkt, playgroundstate_toys.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces_toys.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 Square%
 make-square-toy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Square%
;; Interface : Toy<%>
;; Sqaure is and object of class Square%
;; A Square is selectable and draggable
;; A Square is (new Square% [x Int] [y Int]
;;                          [mx-dist Int] [my-dist Int]
;;                          [velocity Int] [selected? Boolean])

(define Square%
  (class* object% (Toy<%>) ;; implement the interface
    
    ; x and y coordinates of center of Square
    (init-field x y)
    
    ; The velocity of the Square
    (init-field velocity)
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; Indicates whether the Square is selected or not
    (init-field [selected? false])
    
    
    ; Private data for objects of this class
    
    ; Width of Square
    (field [SQUARE-WIDTH 40])
    
    ; Half of the width of Square
    (field [HALF-SQUARE-WIDTH (/ SQUARE-WIDTH 2)])
    
    ; Max x-coordinate for Square
    (field [MAX-X-FOR-SQ (- CANVAS-WIDTH HALF-SQUARE-WIDTH)])
    
    ; Min x-coordinate for Square
    (field [MIN-X-FOR-SQ HALF-SQUARE-WIDTH])
    
    ; Mode and color of the square
    (define SQUARE-MODE "outline")
    (define SQUARE-COLOR "red")
    
    ; Image for displaying Square
    (field [SQUARE-IMG (square SQUARE-WIDTH SQUARE-MODE SQUARE-COLOR)])
    
    
    (super-new)
    
    
    ; Methods of Toy<%>
    
    ; toy-x : -> Int
    ; RETURNS : The x position of the center of Square
    (define/public (toy-x) x)
    
    
    ; toy-y : -> Int
    ; RETURNS : The y position of the center of Square
    (define/public (toy-y) y)
    
    
    ; toy-data : -> Int
    ; RETURNS : The velocity of Square
    (define/public (toy-data) velocity)
    
    
    ; Methods of SWidget<%>
    
    ; after-tick : -> Void
    ; GIVEN : no arguments
    ; EFFECTS : updates the state of the Square that should follow 
    ;           after a tick.
    ; STRATEGY : Cases on whether the Square is selected or not 
    (define/public (after-tick)
      (if selected?
          this
          (begin
            (set! x (x-after-tick))
            (set! velocity (velocity-after-tick)))))
    
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the Square that should follow the
    ;          button down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the mouse
    ;                   is inside the Square or not
    (define/public (after-button-down mx my)
      (if (inside-square? mx my)
          (begin
            (set! mx-dist (- mx x))
            (set! my-dist (- my y))
            (set! selected? true))
          this))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the Square that should follow the
    ;           button-up mouse event at the given location
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the Square that should follow the
    ;           drag mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the Square
    ;                   is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx mx-dist))
            (set! y (- my my-dist)) 
            (set! mx-dist mx-dist)
            (set! my-dist my-dist))
          this))  
    
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN : A key event
    ; EFFECTS : Updates the state of this Square that should follow the
    ;           given key event
    ; DETAILS : Square ignores all the key events
    (define/public (after-key-event ke) this)
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this Square
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image SQUARE-IMG
                   x y
                   scene))
    
    
    ; Local functions
    
    ; x-after-tick : -> Int
    ; GIVEN : No arguments
    ; RETURNS : x coordinate of the Square at the next tick
    ; DESIGN STRATEGY : Divide into cases based on the
    ;                   value of x+velocity
    (define (x-after-tick)
      (cond
        [(x-more-than-max-limit?)
         MAX-X-FOR-SQ]
        [(x-less-than-min-limit?)
         MIN-X-FOR-SQ]
        [else (+ x velocity)]))
    
    
    ; velocity-after-tick : -> Int
    ; GIVEN : No arguments
    ; RETURNS : The velocity of Square at the next tick
    ; DESIGN STRATEGY : Divide into cases based on the
    ;                   value of x+velocity
    (define (velocity-after-tick)
      (if (or (x-more-than-max-limit?)
              (x-less-than-min-limit?))
          (- velocity)
          velocity))
    
    
    ; x-more-than-max-limit? : -> Boolean
    ; GIVEN : No arguments
    ; RETURNS : True, if x+velocity is greater that max limit
    ;           for x
    ; DESIGN STRATEGY : Combine simpler functions
    (define (x-more-than-max-limit?)
      (>= (+ x velocity) MAX-X-FOR-SQ))
    
    
    ; x-less-than-min-limit? : -> Boolean
    ; GIVEN : No arguments
    ; RETURNS : True, iff x+velocity is less that min limit for x
    ; DESIGN STRATEGY : Combine simpler functions
    (define (x-less-than-min-limit?)
      (<= (+ x velocity) MIN-X-FOR-SQ))
    
    
    ; inside-square? : NonNegInt NonNegInt -> Boolean
    ; GIVEN :  Mouse coordinates
    ; RETURNS : True, iff the give mouse location is inside the square
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-square? mx my)
      (and (<= (- x HALF-SQUARE-WIDTH)
               mx
               (+ x HALF-SQUARE-WIDTH))
           (<= (- y HALF-SQUARE-WIDTH)
               my
               (+ y HALF-SQUARE-WIDTH))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-square-toy : Int Int PosInt -> Toy<%>
;; GIVEN : An x and a y position, and a speed
;; RETURNS : An object representing a square toy at the given position,
;;           travelling right at the given speed
(define (make-square-toy x y vel)
  (new Square% [x x] [y y] [velocity vel]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-X-CENTER (/ CANVAS-WIDTH 2))
(define CANVAS-Y-CENTER (/ CANVAS-HEIGHT 2))

(begin-for-test
  (local
    ((define MAX-X-FOR-SQ (- CANVAS-WIDTH 20))
     (define MIN-X-FOR-SQ 20)
     (define SQUARE-IMG (square 40 "outline" "red"))
     (define x CANVAS-X-CENTER)
     (define y CANVAS-Y-CENTER)
     (define VELOCITY 10)
     
     ; Square has reached the max value for x
     (define SQUARE-AT-RIGHT-BORDER
       (new Square%
            [x MAX-X-FOR-SQ]
            [y 100]
            [velocity 20]
            [selected? false]))
     
     ; Square has reached the MIN value for x
     (define SQUARE-AT-LEFT-BORDER
       (new Square%
            [x 2]
            [y 100]
            [velocity -20]
            [selected? false]))
     
     ; Selected Square
     (define SELECTED-SQUARE
       (new Square%
            [x 250]
            [y 250]
            [velocity 10]
            [selected? true]))
     
     ; Unselected Square
     (define UNSELECTED-SQUARE
       (new Square%
            [x 470]
            [y 250]
            [velocity 10]
            [selected? false])))
    
    ; Selected Square is tested for mouse, key and tick events
    (send SELECTED-SQUARE after-tick)
    (send SELECTED-SQUARE after-button-down 250 250)
    (send SELECTED-SQUARE  after-drag 300 300)
    (send SELECTED-SQUARE after-button-up 300 300)
    (send SELECTED-SQUARE after-key-event "s")
    (send SELECTED-SQUARE after-tick)
    
    ; Checks the x-coordinate of Square after a tick 
    (check-equal? (send SELECTED-SQUARE toy-x)
                  310
                  "test for after-tick")
    
    ; Checks the y-coordinate of Square after a tick 
    (check-equal? (send SELECTED-SQUARE toy-y)
                  300
                  "test for after-tick")
    
    ; Velocity of selected Square is checked
    (check-equal?
     (send SELECTED-SQUARE toy-data)
     10
     "velocity should be 10")
    
    ; The Square at right border is checked for its x value after tick
    (send SQUARE-AT-RIGHT-BORDER after-tick)
    (check-equal?
     (send SQUARE-AT-RIGHT-BORDER toy-x)
     480
     "test for after-tick")
    
    ; The Square at left border is checked for its x value after tick
    (send SQUARE-AT-LEFT-BORDER after-tick)
    (check-equal?
     (send SQUARE-AT-LEFT-BORDER toy-x)
     20
     "test for after-tick")
    
    ; Unselected Square is selected, and its x value after mouse
    ; events is checked
    (send UNSELECTED-SQUARE after-tick)
    (send UNSELECTED-SQUARE after-button-down 10 10)
    (send UNSELECTED-SQUARE after-drag 20 20)
    (send UNSELECTED-SQUARE after-button-up 20 20)
    (check-equal? (send UNSELECTED-SQUARE toy-x)
                  480
                  "test for tick on unselected square")
    
    ; The image of unselected Square is placed on canvas
    (check-equal?
     (send  (make-square-toy x y VELOCITY)
            add-to-scene EMPTY-CANVAS)
     (place-image SQUARE-IMG x y EMPTY-CANVAS)
     "test for add-to-scene")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;