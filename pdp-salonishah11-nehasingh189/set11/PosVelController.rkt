;; PosVelController.rkt

;; It provides a super class PosVelController%
;; It has subclasses PositionController% and VelocityController%

#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")

(provide PosVelController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class PosVelController%
;; Interface : Controller<%>
;; PosVelController is an object of PosVelController%
;; PosVelController is (new PosVelController% [model Model<%>]
;;                                            [x NonNegInt] [y NonNegInt])

(define PosVelController%
  (class* object% (Controller<%>)
    
    ; The model
    (init-field model)  
    
    ; The position of the center of the controller
    ; Default value is 300 and 250.
    (init-field [x 300] [y 250])
    
    
    
    ; The width and height of controller
    ; Default value is 150 and 50 respectively
    (field [width 150][height 50])    
    
    ; Half of width and height of controller
    (field [HALF-WIDTH (/ width  2)])
    (field [HALF-HEIGHT (/ height 2)])
    
    ; The position of the particle
    ; This position is relative to canvas of controller
    ; Default value is 0
    (field [particle-x 0] [particle-y 0])
    
    ; The velocity of particle
    ; Default value is 0
    (field [particle-vx 0][particle-vy 0])
    
    ; Represents whether the handle is selected or not
    ; Default value is false
    (field [handle-sel? false])
    
    ; Represents whether the controller is selected or not
    ; Default value is false
    (field [controller-sel? false])
    
    ; Represents the relative distance of center of controller
    ; to mx and my
    ; Default value is 0
    (field [saved-mx 0] [saved-my 0])
    
    ; Value by which position or velocity is to be incremented
    ; or decremented
    (field [INC-DEC-FAC 5])
    
    ; Length of Handle
    (field [HANDLE-LEN 10])
    
    ; Mode for rectangles
    (field [MODE-OUTLINE "outline"])
    
    ; Colors for displaying outer rectangles
    (field [COLOR-OUTER-RECT "black"])
    
    ; Alignment for overlay
    (field [ALIGN-LEFT "left"])
    (field [ALIGN-TOP "top"])
    
    ; Font size for text 
    (field [FONT-SIZE 11])
    
    ; Value to round the inexact values
    (field [ROUND-CONST 100])
    
    ; Color for displaying selected and unselected handle
    (field [HANDLE-CONTR-SEL "red"] [HANDLE-CONTR-UNSEL "black"])
    
    ; Arrow key events
    (field [INC-VAL-IN-X-DIR "right"])
    (field [DEC-VAL-IN-X-DIR "left"])
    (field [INC-VAL-IN-Y-DIR "up"])
    (field [DEC-VAL-IN-Y-DIR "down"])
    
    
    (super-new)
    
    
    ; receive-signal : Signal -> Void
    ; GIVEN : A Signal
    ; EFFECT : Decodes the given signal and updates position or velocity
    ;          of particle based on whether given signal is report-position
    ;          or report-velocity
    ; DESIGN STRATEGY : Divide into cases based on type of Signal
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (begin
           (set! particle-x (report-position-x-pos sig))
           (set! particle-y (report-position-y-pos sig)))]
        [(report-velocity? sig)
         (begin
           (set! particle-vx (report-velocity-vx sig))
           (set! particle-vy (report-velocity-vy sig)))]))
    
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN : The mouse location
    ; EFFECT : Updates the state of controller based on whether button is down 
    ;          inside handle or controller. 
    ; DESIGN STRATEGY : Cases on whether mouse is inside handle or controller
    (define/public (after-button-down mx my)
      (cond
        [(inside-handle? mx my)
         (begin
           (set! handle-sel? true)
           (set! saved-mx (- mx x))
           (set! saved-my (- my y)))]
        [(inside-controller? mx my)
         (set! controller-sel? true)] 
        [else this]))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : The mouse location
    ; EFFECT : Updates the controller to state after button up
    ;          event.
    (define/public (after-button-up mx my)
      (begin
        (set! handle-sel? false)
        (set! controller-sel? false)))
    
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN : The mouse location
    ; EFFECT : Updates this controller to the state following the drag event.
    ; DESIGN STRATEGY : Cases on whether handle is selected or not
    (define/public (after-drag mx my)
      (if handle-sel?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          this))
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object painted
    ;           on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image (viewer-image) x y scene))
    
    
    ; after-tick : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates this controller to the state it should have
    ;          following a tick
    ; DETAILS : Tick events are not handle in controller
    (define/public (after-tick) 'viewer2-after-tick-trap)
    
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN : A key event
    ; EFFECT : Updates this controller to the state it should have
    ;          following the given key event
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-key-event kev)
      (if controller-sel?
          (key-event-on-controller kev)
          this))
    
    
    ; Local functions
    
    ; key-event-on-controller : KeyEvent -> Void
    ; GIVEN : A key event
    ; EFFECT : Updates this controller to the state it should have
    ;          following the given key event
    ; DESIGN STRATEGY : Divide into cases based on key event kev
    (define (key-event-on-controller kev)
      (cond
        [(key=? INC-VAL-IN-X-DIR kev)
         (send this increment-pos-vel-x)]
        [(key=? DEC-VAL-IN-X-DIR kev)
         (send this decrement-pos-vel-x)]
        [(key=? INC-VAL-IN-Y-DIR kev)
         (send this decrement-pos-vel-y)]
        [(key=? DEC-VAL-IN-Y-DIR kev)
         (send this increment-pos-vel-y)]))
    
    
    ; to be supplied by subclasses
    (abstract increment-pos-vel-x)
    (abstract decrement-pos-vel-x)
    (abstract increment-pos-vel-y)
    (abstract decrement-pos-vel-y)
    
    
    ; inside-handle? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True, iff the give mouse location is inside the handle
    ; STRATEGY : Combine simpler functions
    (define (inside-handle? other-x other-y)
      (and
       (<= (- x HALF-WIDTH) other-x (+ (- x HALF-WIDTH) HANDLE-LEN))
       (<= (- y HALF-HEIGHT) other-y (+ (- y HALF-HEIGHT) HANDLE-LEN))))
    
    
    ; inside-controller? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True, iff the give mouse location is inside the controller
    ; STRATEGY : Combine simpler functions
    (define (inside-controller? other-x other-y)
      (and
       (<= (- x HALF-WIDTH) other-x (+ x HALF-WIDTH))
       (<= (- y HALF-HEIGHT) other-y (+ y HALF-HEIGHT))))
    
    
    ; current-color-handle : -> String
    ; GIVEN : No arguments
    ; RETURNS : String representing the color of handle
    ; DESIGN STRATEGY : Divide into cases based on handle-sel?
    (define (current-color-handle)
      (if handle-sel? HANDLE-CONTR-SEL HANDLE-CONTR-UNSEL))
    
    
    ; current-color-controller : -> String
    ; GIVEN : No arguments
    ; RETURNS : String representing the color of controller
    ; DESIGN STRATEGY : Divide into cases based on controller-sel?
    (define (current-color-controller)
      (if controller-sel? HANDLE-CONTR-SEL HANDLE-CONTR-UNSEL))
    
    
    ; viewer-image : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : A scene containing the rectangle and text which displays
    ;           the current position and velocity of particle, along with
    ;           the handle displayed on top-left corner of rectangle
    ; DESIGN STRATEGY : Combine simpler functions
    (define (viewer-image)
      (overlay/align ALIGN-LEFT ALIGN-TOP
                     (square HANDLE-LEN MODE-OUTLINE (current-color-handle))
                     (text-inside-rect)))
    
    
    ; text-inside-rect : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : A scene containing the rectangle and text which displays
    ;           the current position and velocity of particle
    ; DESIGN STRATEGY : Combine simpler functions
    (define (text-inside-rect)
      (let (; text that displays the position and velocity of particle
            (the-data-image (data-image)))
        (overlay
         the-data-image
         (rectangle
          (max width (+ (image-width the-data-image) HANDLE-LEN))
          (max height (+ (image-height the-data-image) HANDLE-LEN))
          MODE-OUTLINE
          COLOR-OUTER-RECT))))
    
    
    ; data-image : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : A scene containing the text which displays the current
    ;           position and velocity of particle
    ; DESIGN STRATEGY : Combine simpler functions
    (define (data-image) 
      (above
       (text (send this get-title-text) FONT-SIZE (current-color-controller))
       (text (string-append
              "X = "
              (get-string-rep particle-x)
              " Y = "
              (get-string-rep particle-y))
             FONT-SIZE (current-color-controller))
       (text (string-append
              "VX = "
              (get-string-rep particle-vx)
              " VY = "
              (get-string-rep particle-vy))
             FONT-SIZE (current-color-controller))))
    
    
    ; to be supplied by subclasses
    (abstract get-title-text)
    
    
    ; get-string-rep : PosReal -> String
    ; GIVEN : A number 
    ; RETURNS : String with the given number rounded to 2 decimal
    ;           places
    (define (get-string-rep num)
      (number->string (/ (round (* (exact->inexact num)
                                   ROUND-CONST))
                         ROUND-CONST)))
    
    
    
    ; Functions for testing
    
    (define/public (for-test:get-x) x)
    (define/public (for-test:get-y) y)
    (define/public (for-test:get-particle-vx) particle-vx)
    (define/public (for-test:get-particle-vy) particle-vy)
    (define/public (for-test:get-particle-x) particle-x)
    (define/public (for-test:get-particle-y) particle-y)
    (define/public (for-test:get-handle-sel?) handle-sel?)
    (define/public (for-test:get-controller-sel?) controller-sel?)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;