;; football.rkt
;; This file contains Football% class and make-football function.
;; It is included in toys.rkt, playgroundstate_toys.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces_toys.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 Football%
 make-football)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Football%
;; Interface : Toy<%>
;; Football is and object of class Football%
;; A Football is selectable and draggable
;; A Football is (new Football% [x Int] [y Int] [size PosReal]
;;                              [mx-dist Int] [my-dist Int]
;;                              [selected? Boolean])

(define Football%
  (class* object% (Toy<%>) ;; implement the interface
    
    ; x and y coordinate of center of Football
    (init-field x y)
    
    ; The current size of football, which is the
    ; current scale factor
    ; Default value is 1
    (init-field [size 1])
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; True if Football is selected, else false
    ; Default value is false
    (init-field [selected? false])
    
    
    ; The scaling factor
    (field [SCALE-FACTOR 1.1])
    
    ; Scale value for x
    (field [SCALE-FOR-X 1])
    
    ; Dividing Factor
    (field [DIV-FAC 2])
    
    ; Image of Tootball for displaying
    (field [FOOTBALL-IMG (bitmap "football.png")])
    
    
    (super-new)

    
    ; Methods of Toy<%>
    
    ; toy-x : -> Integer
    ; GIVEN : No Arguments
    ; RETURNS : The x coordinate of the center of the Football.
    (define/public (toy-x) x)
    
    
    ; toy-y : -> Integer
    ; GIVEN : No Arguments
    ; RETURNS : The y coordinate of the center of the Football.
    (define/public (toy-y) y)
    
    
    ; toy-data : -> PosReal
    ; RETURNS : The size of Football
    (define/public (toy-data) size)
    
    
    ; Methods of SWidget<%>
    
    ; after-tick : -> Void
    ; GIVEN : No Arguments
    ; EFFECTS : Updates the state of Football at the next tick
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (set! size (/ size SCALE-FACTOR))))
    
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN : A key event
    ; EFFECTS : Updates the state of football object that should follow the
    ;           given key event
    ; DETAILS : Football ignores all the key events
    (define/public (after-key-event ke) this)   
    
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the football that should follow the
    ;           button-down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the mouse is
    ;                   is inside the football or not
    (define/public (after-button-down mx my)
      (if (inside-football? mx my
                            (half-football-width)
                            (half-football-height))
          (begin
            (set! mx-dist (- mx x))
            (set! my-dist (- my y))
            (set! selected? true))
          this))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the football that should follow the
    ;           button-up mouse event at the given location
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the football that should follow the
    ;           drag mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the football
    ;                   is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx mx-dist))
            (set! y (- my my-dist))
            (set! mx-dist mx-dist)
            (set! my-dist my-dist))
          this))   
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    ; DESIGN STRATEGY : Divide into cases based on the image-height
    ;                   of the image of Football after scaling it
    (define/public (add-to-scene scene)
      (if (<= (image-height (scaled-football-image)) 0)
          scene
          (place-image (scaled-football-image)
                       x y
                       scene)))
    
    ; Local functions
    
    ; scaled-football-image : -> Image
    ; GIVEN : No arguments
    ; RETURNS : A scaled image of the Football
    ; DESIGN STRATEGY : Combine simpler functions
    (define (scaled-football-image)
      (scale/xy SCALE-FOR-X size FOOTBALL-IMG))
    
    
    ; half-football-width : -> PosReal
    ; GIVEN : No arguments
    ; RETURNS : The half width of Football
    ; DESIGN STRATEGY : Combine simpler functions
    (define (half-football-width)
      (/ (image-width (scaled-football-image)) DIV-FAC))
    
    
    ; half-football-height : -> PosReal
    ; GIVEN : No arguments
    ; RETURNS : The half height of Football
    ; DESIGN STRATEGY : Combine simpler functions
    (define (half-football-height)
      (/ (image-height (scaled-football-image)) DIV-FAC))
    
    
    ; inside-football? : NonNegInt NonNegInt PosReal PosReal -> Boolean
    ; GIVEN : Mouse coordinates and width and height of football
    ; RETURNS : True, iff the give mouse location is inside the football
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-football? mx my width height)
      (and (<= (- x width)
               mx
               (+ x width))
           (<= (- y height)
               my
               (+ y height))))    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-football : Int Int -> Toy<%>
;; GIVEN : An x and a y position of the football toy
;; RETURNS : An object representing a football toy at the given position.
(define (make-football x y)
  (new Football% [x x] [y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-X-CENTER (/ CANVAS-WIDTH 2))
(define CANVAS-Y-CENTER (/ CANVAS-HEIGHT 2))


(begin-for-test
  (local
    (; x and y are initialized to centre of the canvas
     (define x CANVAS-X-CENTER)
     (define y CANVAS-Y-CENTER)

     ; Image of football
     (define FOOTBALL-IMG (bitmap "football.png"))
     
     ; height and width of the football image
     (define width (image-width FOOTBALL-IMG))
     (define height (image-height FOOTBALL-IMG))
     
     ; a tiny football that disapperas after few ticks
     (define SMALL-FOOTBALL
       (new Football% [x 100] [y 100] [size 0.001] [selected? false]))

     ; Object of Football%
     (define football (make-football x y)))

    ; Checks the image of football after placing it on empty canvas
    (check-equal? (send football add-to-scene EMPTY-CANVAS)
                  (place-image FOOTBALL-IMG x y EMPTY-CANVAS)
                  "The football should appear at centre of CANVAS")
    
    ; Checks the size of Football after tick, button down, drag,
    ; button up and tick events
    (send football after-button-down (+ x width) y)
    (send football after-drag (add1 x) (sub1 y))
    (send football after-tick)
    (send football after-button-down x y)
    (send football after-drag (add1 x) (sub1 y))
    (send football after-tick)
    (send football after-button-up (add1 x) (sub1 y))
    (send football after-tick)
    
    ; Key event of football
    (send football after-key-event "f")

    ; Checks effect of key events on football
    (check-true (and (equal? (send football toy-x) (add1 x))
                     (equal? (send football toy-y) (sub1 y))
                     (equal? (send football toy-data) 0.8264462809917355))
                "This footbal should have area decreased by rate ^ 4")
    
    ; Football after 3 ticks
    (send SMALL-FOOTBALL after-tick)
    (send SMALL-FOOTBALL after-tick)
    (send SMALL-FOOTBALL after-tick)

    ; Checks the size of Football after 3 ticks
    (check-equal?
     (send SMALL-FOOTBALL add-to-scene EMPTY-CANVAS)
     EMPTY-CANVAS
     "test for add-to-scene")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;