;; throbber.rkt
;; This file contains Throbber% class and make-throbber function.
;; It is included in toys.rkt, playgroundstate_toys.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces_toys.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 Throbber%
 make-throbber)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Throbber%
;; Interface : Toy<%>
;; Throbber is and object of class Throbber%
;; A Throbber is selectable and draggable
;; A Throbber is (new Throbber% [x Int] [y Int]
;;                              [rad PosInt] [inc-rad? Boolean]
;;                              [mx-dist Int] [my-dist Int]
;;                              [selected? Boolean])

(define Throbber%
  (class* object% (Toy<%>) ;; implement the interface
    
    ; x and y coordinate of center of Throbber
    (init-field x y)
    
    ; radius of Throbber
    ; Default value is 5
    (init-field [rad 5])
    
    ; True if the radius of Throbber has to be
    ; incremented by 1, else false
    ; Default value is true
    (init-field [inc-rad? true])
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; True, if the Throbber is selected, else false
    ; Default value is false
    (init-field [selected? false])

    
    ; Private data for objects of this class
    
    ; Max radius of Throbber
    (field [MAX-RAD 20])
    
    ; Min radius of Throbber
    (field [MIN-RAD 5])
    
    ; The mode and color of Throbber, used during displaying
    ; Throbber
    (field [THROBBER-MODE "solid"] [THROBBER-COLOR "green"])

    
    (super-new)

    
    ; Methods of Toy<%>
    
    ; toy-x : -> Int
    ; RETURNS : The x position of the center of Throbber
    (define/public (toy-x) x)
    
    
    ; toy-y : -> Int
    ; RETURNS : The y position of the center of Throbber
    (define/public (toy-y) y)
    
    
    ; toy-data : -> PosInt
    ; RETURNS : The radius of Throbber
    (define/public (toy-data) rad)
    
    
    ; Methods of SWidget<%>
    
    ; after-tick : -> Void
    ; GIVEN : No arguments
    ; EFFECTS : Updates the state of this Throbber at the next tick
    ; DESIGN STRATEGY : Divide into cases based on whether Throbber
    ;                   is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (begin
            (set! inc-rad? (inc-rad-after-tick?))
            (set! rad (rad-after-tick)))))          

    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the Throbber that should follow the
    ;           button down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the mouse
    ;                   is inside the throbber or not
    (define/public (after-button-down mx my)
      (if (inside-throbber? mx my)
          (begin
            (set! mx-dist (- mx x))
            (set! my-dist (- my y))
            (set! selected? true))
          this))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the Throbber that should follow the
    ;           button-up mouse event at the given location
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    
    ; after-drag : NonNegInteger NonNegInteger -> Void
    ; GIVEN : The x and y coordinates of the mouse click.
    ; EFFECTS : Updates the state of the Throbber that should follow the
    ;           given mouse drag event at the given location.
    ; STRATEGY : Divide into cases based on whether throbber is selected
    ;            or not
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
    ; EFFECTS : Updates the state of Throbber that should follow the  
    ;           given key event.
    ; DEATILS : The state of the Throbber does not change with any keyevents
    (define/public (after-key-event) this)    


    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this Throbber
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image (circle rad THROBBER-MODE THROBBER-COLOR)
                   x y
                   scene))

    
    ; Local functions
    
    ; inc-rad-after-tick? : -> Boolean
    ; GIVEN : No arguments
    ; RETURNS : True, iff at next tick the value of radius of Throbber
    ;           is bewtween max and min value of radius for Throbber,
    ;           else false
    ; DESIGN STRATEGY : Divide into cases based on whether the value of
    ;                   rad at next tick is between MAX-RAD and MIN-RAD
    (define (inc-rad-after-tick?)
      (if (and (<= (rad-after-tick) MAX-RAD)
               (<= MIN-RAD (rad-after-tick)))
          inc-rad?
          (not inc-rad?)))

    
    ; rad-after-tick : -> PosInt
    ; GIVEN : No arguments
    ; RETURNS : The value of radius at next tick
    ; DESIGN STRATEGY : Divide into cases based on the value of inc-rad?
    (define (rad-after-tick)
      (if inc-rad?
          (add1 rad)
          (sub1 rad)))
    
    
    ; inside-throbber? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse coordinates 
    ; RETURNS : True, iff the give mouse location is inside the throbber
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-throbber? mx my)
      (<= (sqrt (+ (sqr (- mx x))
                   (sqr (- my y))))
          rad))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-throbber : Int Int -> Toy<%>
;; GIVEN : An x and a y position of the centre of throbber toy.
;; RETURNS : An object representing a throbber at the given position.
(define (make-throbber x y)
  (new Throbber% [x x] [y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(begin-for-test
  (local
    (; A Throbber with radius 18 and inc-rad? true
     (define THROB1
       (new Throbber% [x 40] [y 40] [rad 18] [inc-rad? true])))
    
    ; THROB1 after 3 ticks
    (send THROB1 after-tick)
    (send THROB1 after-tick)
    (send THROB1 after-tick)  
    
    ; THROB1 after 3 ticks should have radius 19
    (check-equal?
     (send THROB1 toy-data)
     19
     "the radius of throb1 after 3 ticks shouls be 19")))

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)
     
     ; Throbber with center at center of canvas
     (define TEST-THROBBER (make-throbber x y)))
    
    ; TEST-THROBBER after mouse events and 1 tick
    (send TEST-THROBBER after-button-down x y)
    (send TEST-THROBBER after-drag (add1 x) (sub1 y))
    (send TEST-THROBBER after-tick)
    (send TEST-THROBBER after-button-up (add1 x) (sub1 y))
    (send TEST-THROBBER after-tick)
    
    ; Checks the x, y and radius of FINAL-THROBBER
    ; Also checks the image of Throbber after placing it on canvas
    (check-true (and (equal? (send TEST-THROBBER toy-x) (add1 x))
                     (equal? (send TEST-THROBBER toy-y) (sub1 y))
                     (equal? (send TEST-THROBBER toy-data) 6)
                     (equal? (send TEST-THROBBER add-to-scene EMPTY-CANVAS)
                             (place-image (circle 6 "solid" "green")
                                          (add1 x) (sub1 y)
                                          EMPTY-CANVAS)))
                "a throbber with radius 6 should be displayed on canvas")))

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)
     
     ; Throbber with center at center of canvas
     (define TEST-THROBBER (make-throbber x y)))
    
    ; TEST-THROBBER after mouse event, key event and 1 tick 
    (send TEST-THROBBER after-button-down 10 10)
    (send TEST-THROBBER  after-drag 20 20)
    (send TEST-THROBBER after-tick)
    (send TEST-THROBBER after-button-up 20 20)
    (send TEST-THROBBER after-tick)
    (send TEST-THROBBER  after-key-event)
    
    ; Checks the x, y and radius of FINAL-THROBBER 
    (check-true (and (equal? (send TEST-THROBBER toy-x) x)
                     (equal? (send TEST-THROBBER toy-y) y)
                     (equal? (send TEST-THROBBER toy-data) 7))
                "a throbber with radius 7 should be displayed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;