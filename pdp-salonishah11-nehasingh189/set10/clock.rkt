;; clock.rkt
;; This file contains Clock% class and make-clock function.
;; It is included in toys.rkt, playgroundstate_toys.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces_toys.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 Clock%
 make-clock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Clock%
;; Interface : Toy<%>
;; Clock is and object of class Clock%
;; A Clock is selectable and draggable
;; A Clock is (new Clock% [x Int] [y Int]
;;                        [mx-dist Int] [my-dist Int]
;;                        [ticks NonNegInt] [selected? Boolean])

(define Clock%
  (class* object% (Toy<%>) ;; implement the interface
    
    ; x and y coordinate of center of Clock
    (init-field x y)
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; The number of ticks since Clock was created
    ; Default value is 0
    (init-field [ticks 0])
    
    ; True if Clock is selected, else false
    ; Default value is false
    (init-field [selected? false])
     
    
    ; Private data for objects of this class
    
    ; Radius of Clock
    (field [CLOCK-RAD 40])
    
    ; The mode and color of Clock, used during displaying
    ; Clock
    (field [CLOCK-MODE "outline"] [CLOCK-COLOR "black"])
    
    ; Image for displayingg Clock
    (field [CLOCK (circle CLOCK-RAD CLOCK-MODE CLOCK-COLOR)])
    
    ; The color and size of text, used during displaying
    ; the ticks of Clock
    (field [TEXT-COLOR "magenta"] [TEXT-SIZE 20])

    ; Alignment of text to be displayed in clock
    (field [ALIGNMENT "middle"])
    
    
    (super-new)
    
    
    ; Methods of Toy<%>
    
    ; toy-x : -> Int
    ; RETURNS : The x position of the center of Clock
    (define/public (toy-x) x)
    
    
    ; toy-y : -> Int
    ; RETURNS : The y position of the center of Clock
    (define/public (toy-y) y)
    
    
    ; toy-data : -> NonNegInt
    ; RETURNS : The ticks of Clock
    (define/public (toy-data) ticks)

    
    ; Methods of Widget<%>

    ; after-tick : -> Void
    ; GIVEN : No arguments
    ; EFFECTS : Updates the state of this Clock at the next tick.
    (define/public (after-tick)
      (set! ticks (add1 ticks)))

    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the Clock that should follow the
    ;           button down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the mouse
    ;                   is inside the Clock or not
     (define/public (after-button-down mx my)
      (if (inside-clock? mx my)
          (begin
            (set! mx-dist (- mx x))
            (set! my-dist (- my y))
            (set! selected? true))
          this))

    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the Clock that should follow the
    ;           button-up mouse event at the given location
    (define/public (after-button-up mx my)
        (set! selected? false))

    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECTS : Updates the state of the Clock that should follow the
    ;           drag mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the Clock
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
    ; EFFECTS : Updates the state of this Clock that should follow the
    ;           given key event
    ; DETAILS : Clock ignores all the key events
    (define/public (after-key-event ke) this)


    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this Clock
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image
       (overlay/align ALIGNMENT ALIGNMENT
                      (text (number->string ticks)
                            TEXT-SIZE
                            TEXT-COLOR)
                      CLOCK)
       x y
       scene))
    

    ; Local functions

    ; inside-clock? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse coordinates 
    ; RETURNS : True, iff the give mouse location is inside the clock
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-clock? mx my)
      (<= (sqrt (+ (sqr (- mx x))
                   (sqr (- my y))))
          CLOCK-RAD))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-clock : Int Int -> Toy<%>
;; GIVEN : An x and a y position of the center of the clock toy.
;; RETURNS : An object representing a Clock toy at the given position.
(define (make-clock x y)
  (new Clock% [x x] [y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-X-CENTER (/ CANVAS-WIDTH 2))
(define CANVAS-Y-CENTER (/ CANVAS-HEIGHT 2))


(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)
     
     ; Clock with center at center of canvas
     (define TEST-CLOCK (make-clock x y)))
  
  ; TEST-CLOCK after button down, drag, tick, button up and tick events  
  (send TEST-CLOCK after-button-down x  y)
  (send TEST-CLOCK after-drag (add1 x) (sub1 y))
  (send TEST-CLOCK after-tick)
  (send TEST-CLOCK after-button-up (add1 x) (sub1 y))
  (send TEST-CLOCK after-tick)
  
  ; Checks the x, y and ticks of FINAL-CLOCK
  (check-true (and (equal? (send TEST-CLOCK toy-x) (add1 x))
                   (equal? (send TEST-CLOCK toy-y) (sub1 y))
                   (equal? (send TEST-CLOCK toy-data) 2))
              "This clock should display 2")))

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)
     
     ; Clock with center at center of canvas
     (define TEST-CLOCK (make-clock x y)))
    
    ; TEST-CLOCK after button down, drag, 2 ticks, button up, 1 tick
    ; key event and 1 tick
    (send TEST-CLOCK after-button-down 10 10)
    (send TEST-CLOCK after-drag 20 20)
    (send TEST-CLOCK after-tick)
    (send TEST-CLOCK  after-tick)
    (send TEST-CLOCK after-button-up 20 20)
    (send TEST-CLOCK after-tick)
    (send TEST-CLOCK after-key-event "f")
    (send TEST-CLOCK after-tick)
    
    ; Checks the x, y and ticks of FINAL-CLOCK
    (check-true (and (equal? (send TEST-CLOCK toy-x) x)
                     (equal? (send TEST-CLOCK toy-y) y)
                     (equal? (send TEST-CLOCK toy-data) 4))
                "This clock should display 3")))

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)
     
     ; Clock with center at center of canvas
     (define TEST-CLOCK (make-clock x y))
     
     ; Image of Clock
     (define CLOCK (circle 40 "outline" "black")))
    
    ; Checks the image of CLOCK after placing on canvas
    (check-equal?
     (send TEST-CLOCK add-to-scene EMPTY-CANVAS)
     (place-image
      (overlay/align
       "middle" "middle"
       (text (number->string 0) 20 "magenta") CLOCK) x y EMPTY-CANVAS))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;