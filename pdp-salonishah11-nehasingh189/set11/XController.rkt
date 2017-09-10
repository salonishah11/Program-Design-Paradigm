;; XController.rkt
;; It displays a rectangle with circle inside it.
;; The circle is a representation of the particle.
;; Only the motion of particle in x-direction is shown.

;; The outer rectangle has a handle which is used to drag the controller.

;; Mouse events are allowed on it.

;; It provides XController% class.


#lang racket
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "ModelForTest.rkt")
(require "Interfaces.rkt")
(require "ParticleViewController.rkt")

(provide XController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class XController%
;; Interface : XYController<%>
;; It is a subclass of ParticleViewController%
;; XController is an object of XController%
;; XController is draggable through its handle.
;; XController is (new XController%)

(define XController%
  (class* ParticleViewController% (XYController<%>) ; implements XYController<%>
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
    ; Default value is 150 and 30 respectively
    (init-field [width 150][height 30])
    
    
    
    ; Half of width and height of controller
    (field [HALF-WIDTH (/ width 2)])
    (field [HALF-HEIGHT (/ height 2)])
    
    ; Difference of width and height in outer rectangle
    ; and inner rectangle
    (field [OUTER-RECT-WIDTH 40] [OUTER-RECT-HEIGHT 0])
    
    ; Half of OUTER-RECT-WIDTH
    (field [HALF-OUTER-RECT-WIDTH (/ OUTER-RECT-WIDTH 2)])
    
    ; The outer rectangle's half width
    ; This is the half width for Controller
    (field [HALF-CONTR-WIDTH (+ HALF-WIDTH HALF-OUTER-RECT-WIDTH)])
    
    
    (super-new)
    
    
    ; Registers this controller with the Model
    (send model register this)
    
    
    ; change-saved-mx-my : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : Updates the saved-mx field of this Controller
    (define/override (change-saved-mx-my mx my)
      (set! saved-mx (- mx particle-x)))
    
    
    ; change-particle-x-y : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : Updates the x position of particle
    (define/override (change-particle-x-y mx my)
      (set! particle-x (within-limits (add1 LOW-X)
                                      (- mx saved-mx)
                                      (sub1 HIGH-X))))
    
    
    ; inside-handle? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True, iff the give mouse location is inside the handle
    ; STRATEGY : Combine simpler functions
    (define/override (inside-handle? other-x other-y)
      (and
       (<= (- x HALF-CONTR-WIDTH)
           other-x
           (+ (- x HALF-CONTR-WIDTH) HANDLE-LEN))
       (<= (- y HALF-HEIGHT)
           other-y
           (+ (- y HALF-HEIGHT) HANDLE-LEN))))
    
    
    ; inside-controller? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True, iff the give mouse location is inside the controller
    ; STRATEGY : Combine simpler functions
    (define/override (inside-controller? other-x other-y)
      (and
       (<= (- x HALF-CONTR-WIDTH) other-x (+ x HALF-CONTR-WIDTH))
       (<= (- y HALF-HEIGHT) other-y (+ y HALF-HEIGHT))))
    
    
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
                   HALF-HEIGHT
                   (inner-rect-image)))
    
    
    ; Local functions
    
    ; within-limits : PosInt Real PosInt -> PosReal
    ; GIVEN : The low value of boundary, current position of particle and 
    ;         high value of boundary. The values are all x values or y
    ;         values of boundary and particle.
    ; RETURNS : The maximum value among low value and minimum of current
    ;           and high value
    (define (within-limits lo val hi)
      (max lo (min val hi)))
    
    
    ; particle-image : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : An image for particle
    ; STRATEGY : Combine simpler functions
    (define (particle-image)
      (overlay/align ALIGN-MIDDLE ALIGN-MIDDLE
                     INNER-CIRLE-IMG
                     OUTER-CIRLE-IMG))
    
    
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
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (local
    ((define MODEL (new Model%))
     (define X-CONTR (new XController% [model MODEL]))
     
     (define PARTICLE-IMG
       (overlay/align "middle" "middle"
                      (circle 1 "solid" "black")
                      (circle 10 "solid" "red")))
     
     (define INNER-RECT
       (place-image
        PARTICLE-IMG
        125
        15
        (add-line
         (add-line
          (rectangle 150 30 "outline" "blue")
          150 0 150 30 "blue")
         0 30 150 30 "blue")))
     
     (define OUTER-RECT-IMG
       (rectangle 190 30 "outline" "black"))
     
     (define OUTER-INNER-RECT-WITH-PARTICLE
       (overlay/align "left" "top"
                      (square 10 "outline" "black")
                      OUTER-RECT-IMG))
     
     (define X-CONTR-IMG
       (place-image
        (overlay/align "middle" "middle"
                       OUTER-INNER-RECT-WITH-PARTICLE
                       INNER-RECT)
        440 356
        (empty-scene 600 500))))
    
    
    ; Mouse events on object
    (send X-CONTR after-button-down 206 240)
    
    ; checks whether handle is selected or not
    (check-equal?
     (send X-CONTR for-test:get-handle-sel?)
     true)
    
    (send X-CONTR after-drag 346 346)
    (send X-CONTR after-button-up 346 346)
    
    ; Checks the x value of object after dragging the handle
    (check-equal?
     (send X-CONTR for-test:get-x)
     440)
    
    ; Checks the y value of object after dragging the handle
    (check-equal?
     (send X-CONTR for-test:get-y)
     356)
    
    ; button-down inside the controller
    (send X-CONTR after-button-down 440 356)
    
    (check-equal?
     (send X-CONTR for-test:get-controller-sel?)
     true)
    
    ; dragging particle to (125, 50) 
    (send X-CONTR after-drag 490 406)
    
    (send X-CONTR after-button-up 490 406)
    
    ; checks the x position of particle
    (check-equal?
     (send X-CONTR for-test:get-particle-x)
     125)
    
    ; checks the x position of particle
    (check-equal?
     (send X-CONTR for-test:get-particle-y)
     50)
    
    ; Checks the x value of object after dragging the particle
    (check-equal?
     (send X-CONTR for-test:get-x)
     440)
    
    ; Checks the y value of object after dragging the particle
    (check-equal?
     (send X-CONTR for-test:get-y)
     356)
    
    (check-equal?
     (send X-CONTR add-to-scene (empty-scene 600 500))
     X-CONTR-IMG)    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;