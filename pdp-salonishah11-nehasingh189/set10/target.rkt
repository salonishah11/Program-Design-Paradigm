;; target.rkt
;; This file contains Target% class and make-target function.
;; It is included in toys.rkt, playgroundstate_toys.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces_toys.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 Target%
 make-target)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Target%
;; Interface : Target<%>
;; Target is and object of class Target%
;; A Target is selectable and draggable
;; A Target is (new Target% [x Int] [y Int]
;;                          [mx-dist Int] [my-dist Int]
;;                          [selected? Boolean])

(define Target%
  (class* object% (Target<%>) ;; implement the interface
    
    ; x and y coordinates of center of Target
    (init-field [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT])
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; Indicates whether the Target is selected or not
    ; Default value is false
    (init-field [selected? false])
    
    
    ; Private data for objects of this class
    
    ; The radius of Target
    (field [TARGET-RADIUS 10])

    ; Half of the radius of Target
    (field [HALF-TARGET-RADIUS (/ TARGET-RADIUS 2)])

    ; The mode of target for its image
    (define TARGET-MODE "outline")

    ; The color of target for its image
    (define TARGET-COLOR "blue")
    
    ; Image for displaying the Target
    (field [TARGET-IMG (circle TARGET-RADIUS TARGET-MODE TARGET-COLOR)])

    
    (super-new)

    
    ; Methods of Target<%>

    ; get-x : -> Integer
    ; GIVEN : no arguments
    ; RETURNS : The x coordinate of Target's center.
    (define/public (get-x) x)
    
    
    ; get-y : -> Integer
    ; GIVEN : no arguments
    ; RETURNS : The y coordinate of Target's center.
    (define/public (get-y) y)

    
    ; get-selected? : -> Boolean
    ; GIVEN : no arguments
    ; RETURNS : True, iff target is selected
    (define/public (get-selected?) selected?)
    

    ; Methods of SWidget<%>
       
    ; after-tick : -> Void
    ; GIVEN : no arguments
    ; EFFECT : updates the Target to the state that should follow
    ;          given tick
    ; DETAILS : The target's position does not change at each tick
    (define/public (after-tick)  this)
    
    
    ; after-button-down : NonNegInteger NonNegInteger -> Void
    ; GIVEN : The x and y coordinates of the mouse click.
    ; EFFECT : updates the state of the Target that should follow 
    ;          the button down event at the given location.
    ; STRATEGY : Cases on whether mouse is inside the Target or not
    (define/public (after-button-down mx my)
      (if (inside-target? mx my)
          (begin
            (set! selected? true)
            (set! mx-dist (- mx x))
            (set! my-dist (- my y)))
          this))

    
    ; after-button-up : NonNegInteger NonNegInteger -> Void
    ; GIVEN : The x and y coordinates of the mouse click.
    ; EFFECT : updates the state of the Target that should follow
    ;          the button up event at the given location.
    (define/public (after-button-up mx my)
      (set! selected? false))

    
    ; after-drag : NonNegInteger NonNegInteger -> Void
    ; GIVEN : The x and y coordinates of the mouse click.
    ; EFFECT : updates the state of the Target that should follow
    ;          the drag event at the given location.
    ; STRATEGY : Cases on whether Target is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx mx-dist))
            (set! y (- my my-dist))
            (set! mx-dist mx-dist)
            (set! my-dist my-dist))
          this))  

    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN : a key event
    ; EFFECT : updates the state of Target that should follow the 
    ;          given key event. 
    ; DETAILS : Target ignores key events. The state of the Target  
    ;           does not change with any keyevents
    (define/public (after-key-event) this)    

    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image TARGET-IMG x y scene))

    
    ; Local functions
    
    ; inside-target? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse coordinates 
    ; RETURNS : True, iff the give mouse location is inside the target
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-target? mx my)
      (<= (sqrt (+ (sqr (- mx x))
                   (sqr (- my y))))
          TARGET-RADIUS))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-target :  -> Target<%>
;; GIVEN : No arguments
;; RETURNS : A new object of Target% class
(define (make-target)
  (new Target%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-X-CENTER (/ CANVAS-WIDTH 2))
(define CANVAS-Y-CENTER (/ CANVAS-HEIGHT 2))
(define TARGET-RADIUS 10)

(begin-for-test
  (local
    (; x and y coordinates are initialized to centre of the canvas
     (define x CANVAS-X-CENTER)
     (define y CANVAS-Y-CENTER)
     
     ; r is radius of the target
     (define r TARGET-RADIUS)
     
     ; variables for defining target image
     (define TARGET-MODE "outline")
     (define TARGET-COLOR "blue")

     ; target
     (define target (make-target))

     ; Image containing target on empty canvas
     (define scene-with-target
       (place-image (circle r TARGET-MODE TARGET-COLOR)
                    (+ x r)
                    (+ y r)
                    EMPTY-CANVAS)))
    
    ; target after mouse events, key event and ticks
    (send target after-drag (- x r) (- y r))
    (send target after-key-event)
    (send target after-button-down (+ x (add1 r))  y)
    (send target after-button-down x y)
    (send target after-drag (+ x r) (+ y r))
    (send target after-tick)
    (send target after-button-up  (+ x r) (+ y r))

    ; checks the x and y coordinate of target after above events
    (check-true (and (equal? (send target get-x) (+ x r))
                     (equal? (send target get-y) (+ y r))
                     (equal? (send target get-selected?) false))
                "This target should be moved by a radius")

    ; checks the image containing target on empty canvas
    (check-equal? (send target add-to-scene EMPTY-CANVAS)
                  scene-with-target
                  "The scene should contain target at ((x+r), (y+r))")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
