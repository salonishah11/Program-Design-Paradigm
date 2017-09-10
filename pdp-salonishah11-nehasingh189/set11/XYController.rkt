;; XYController.rkt
;; It displays a rectangle with circle inside it.
;; The circle is a representation of the particle.
;; The motion of particle in both x and y direction is shown.

;; The outer rectangle has a handle which is used to drag the controller.

;; Mouse events are allowed on it.

;; It provides XYController% class. 

#lang racket
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "ParticleViewController.rkt")
(require "ModelForTest.rkt")
(require "extras.rkt")

(provide XYController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class XYController%
;; Interface : XYController<%>
;; It is a subclass of ParticleViewController%
;; XYController is an object of XController%
;; XYController is draggable through its handle.
;; XYController is (new XYController%)

(define XYController%
  (class* ParticleViewController% (XYController<%>) ; implement XYController<%>
    ; interface
    
    ; The model
    (inherit-field model)
    
    ; The position of the center of the controller
    (inherit-field x y)
    
    ; Represents the relative distance of center of controller
    ; to mx and my
    (inherit-field saved-mx saved-my)
    
    ; The position of center of the particle
    ; This position is relative to canvas of controller
    (inherit-field particle-x particle-y)
    
    ; The velocity of particle
    (inherit-field particle-vx particle-vy)
    
    ; Represents whether the handle is selected or not
    ; Represents whether the controller is selected or not
    (inherit-field handle-sel? controller-sel?)
    
    ; Represents the mode displaying rectangles & circles
    (inherit-field MODE-OUTLINE MODE-SOLID)
    
    ; Represents the color displaying rectangles 
    (inherit-field COLOR-OUTER-RECT COLOR-INNER-RECT)
    
    ; Images of inner and outer circle for displaying particle
    (inherit-field INNER-CIRLE-IMG OUTER-CIRLE-IMG)
    
    ; Alignment for overlay
    (inherit-field ALIGN-MIDDLE)
    
    ; Represents the length of Handle
    (inherit-field HANDLE-LEN)
    
    
    
    ; The width and height of controller
    ; Default value is 150 and 100 respectively
    (init-field [width 150][height 100])
    
    
    
    ; Half of width and height of controller
    (field [HALF-WIDTH (/ width 2)])
    (field [HALF-HEIGHT (/ height 2)])
    
    ; Difference of width and height in outer rectangle
    ; and inner rectangle
    (field [OUTER-RECT-WIDTH 40] [OUTER-RECT-HEIGHT 40])
    
    ; Half of OUTER-RECT-WIDTH and OUTER-RECT-HEIGHT
    (field [HALF-OUTER-RECT-WIDTH (/ OUTER-RECT-WIDTH 2)])
    (field [HALF-OUTER-RECT-HEIGHT (/ OUTER-RECT-HEIGHT 2)])
    
    ; The outer rectangle's half width
    ; This is the half width for Controller
    (field [HALF-CONTR-WIDTH (+ HALF-WIDTH HALF-OUTER-RECT-WIDTH)])
    
    ; The outer rectangle's half height
    ; This is the half height for Controller
    (field [HALF-CONTR-HEIGHT (+ HALF-HEIGHT HALF-OUTER-RECT-HEIGHT)])
    
    
    (super-new)
    
    
    ; Registers this controller with the Model
    (send model register this)
    
    
    ; change-saved-mx-my : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : Updates the saved-mx and saved-my field of this Controller
    (define/override (change-saved-mx-my mx my)
      (begin
        (set! saved-mx (- mx particle-x))
        (set! saved-my (- my particle-y))))
    
    
    ; change-particle-x-y : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : Updates the x and y position of particle
    (define/override (change-particle-x-y mx my)
      (begin
        (set! particle-x (within-limits (add1 LOW-X)
                                        (- mx saved-mx)
                                        (sub1 HIGH-X)))
        (set! particle-y (within-limits (add1 LOW-Y)
                                        (- my saved-my)
                                        (sub1 HIGH-Y)))))
    
    
    ; inside-handle? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True, iff the give mouse location is inside the handle
    ; STRATEGY : Combine simpler functions
    (define/override (inside-handle? other-x other-y)
      (and
       (<= (- x HALF-CONTR-WIDTH)
           other-x
           (+ (- x HALF-CONTR-WIDTH) HANDLE-LEN))
       (<= (- y HALF-CONTR-HEIGHT)
           other-y
           (+ (- y HALF-CONTR-HEIGHT) HANDLE-LEN))))
    
    
    ; inside-controller? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True, iff the give mouse location is inside the controller
    ; STRATEGY : Combine simpler functions
    (define/override (inside-controller? other-x other-y)
      (and
       (<= (- x HALF-CONTR-WIDTH) other-x (+ x HALF-CONTR-WIDTH))
       (<= (- y HALF-CONTR-HEIGHT) other-y (+ y HALF-CONTR-HEIGHT))))
    
    
    ; outer-rect-image : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : An image of a rectangle, representing the black
    ;           outer rectangle of XController
    (define/override (outer-rect-image)
      (rectangle (+ width OUTER-RECT-WIDTH)
                 (+ height OUTER-RECT-HEIGHT)
                 MODE-OUTLINE COLOR-OUTER-RECT))
    
    
    ; inner-rect-with-particle : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : An image containg the inner blue rectangle of controller
    ;           with image of particle placed on it
    ; STRATEGY : Combine simpler functions
    (define/override (inner-rect-with-particle)
      (place-image (particle-image)
                   particle-x
                   particle-y
                   (inner-rect-image)))
    
    
    ; Local functions
    
    ; within-limits : PosInt Real PosInt -> PosReal
    ; GIVEN : The low value of boundary, current position of particle and 
    ;         high value of boundary. The values are all x values or y
    ;         values of boundary and particle.
    ; RETURNS : The maximum value among low value and minimum of current
    ;           and high value
    (define/public (within-limits lo val hi)
      (max lo (min val hi)))
    
    
    ; inner-rect-image : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : An image containg the inner blue rectangle of controller
    ; STRATEGY : Combine simpler functions
    (define (inner-rect-image)
      (add-line
       (add-line
        (rectangle width height MODE-OUTLINE COLOR-INNER-RECT)
        width 0 width height COLOR-INNER-RECT)
       0 height width height COLOR-INNER-RECT))
    
    
    ; particle-image : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : An image for particle
    ; STRATEGY : Combine simpler functions
    (define (particle-image)
      (overlay/align ALIGN-MIDDLE ALIGN-MIDDLE
                     INNER-CIRLE-IMG
                     OUTER-CIRLE-IMG))   
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (local
    ((define MODEL (new Model%))
     (define XY-CONTR (new XYController% [model MODEL]))
     
     (define PARTICLE-IMG
       (overlay/align "middle" "middle"
                      (circle 1 "solid" "black")
                      (circle 10 "solid" "red")))
     
     (define INNER-RECT
       (place-image
        PARTICLE-IMG
        125
        99
        (add-line
         (add-line
          (rectangle 150 100 "outline" "blue")
          150 0 150 100 "blue")
         0 100 150 100 "blue")))
     
     (define OUTER-RECT-IMG
       (rectangle 190 140 "outline" "black"))
     
     (define OUTER-INNER-RECT-WITH-PARTICLE
       (overlay/align "left" "top"
                      (square 10 "outline" "black")
                      OUTER-RECT-IMG))
     
     (define XY-CONTR-IMG
       (place-image
        (overlay/align "middle" "middle"
                       OUTER-INNER-RECT-WITH-PARTICLE
                       INNER-RECT)
        444 369
        (empty-scene 600 500))))
    
    
    ; Mouse events on object
    (send XY-CONTR after-button-down 206 181)
    
    ; checks whether handle is selected or not
    (check-equal?
     (send XY-CONTR for-test:get-handle-sel?)
     true)
    
    (send XY-CONTR after-drag 350 300)
    (send XY-CONTR after-button-up 350 300)
    
    ; Checks the x value of object after dragging the handle
    (check-equal?
     (send XY-CONTR for-test:get-x)
     444)
    
    ; Checks the y value of object after dragging the handle
    (check-equal?
     (send XY-CONTR for-test:get-y)
     369)
    
    ; button-down inside the controller
    (send XY-CONTR after-button-down 444 369)
    
    (check-equal?
     (send XY-CONTR for-test:get-controller-sel?)
     true)
    
    ; dragging particle to (125, 99) 
    (send XY-CONTR after-drag 494 419)
    
    (send XY-CONTR after-button-up 494 419)
    
    ; checks the x position of particle
    (check-equal?
     (send XY-CONTR for-test:get-particle-x)
     125)
    
    ; checks the x position of particle
    (check-equal?
     (send XY-CONTR for-test:get-particle-y)
     99)
    
    ; Checks the x value of object after dragging the particle
    (check-equal?
     (send XY-CONTR for-test:get-x)
     444)
    
    ; Checks the y value of object after dragging the particle
    (check-equal?
     (send XY-CONTR for-test:get-y)
     369)
    
    (check-equal?
     (send XY-CONTR add-to-scene (empty-scene 600 500))
     XY-CONTR-IMG)    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
