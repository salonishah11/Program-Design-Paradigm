;; PositionController.rkt
;; It displays an outline rectangle with text showing the the position
;; and velocity of particle.

;; The rectangle is draggable using the handle.

;; Arrow keys increment/decrement location of the particle by 5.

;; The file provides PositionController% class.

#lang racket
(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "ModelForTest.rkt")
(require "PosVelController.rkt")

(provide PositionController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class PositionController%
;; Interface : PostionVelocity<%>
;; It is a subclass of PosVelController%
;; PositionController is an object of PositionController%
;; PositionController is draggable through its handle.
;; PositionController is (new PositionController%)

(define PositionController%
  (class* PosVelController% (PostionVelocity<%>) ; implement PostionVelocity<%>
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
    
    ; Represents the value by which position or velocity
    ; is to be incremented or decremented
    (inherit-field INC-DEC-FAC)
    
    
    (super-new)
    
    
    ; Registers this controller with the Model
    (send model register this) 
    
    
    ; increment-pos-vel-x : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the position of particle. Here the particle's
    ;          x coordinate will be incremented by 5
    (define/override (increment-pos-vel-x) 
      (send model execute-command
            (make-set-position
             (within-limits (add1 LOW-X)
                            (+ particle-x INC-DEC-FAC)
                            (sub1 HIGH-X))
             particle-y)))
    
    
    ; decrement-pos-vel-x : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the position of particle. Here the particle's
    ;          x coordinate will be decremented by 5
    (define/override (decrement-pos-vel-x)
      (send model execute-command
            (make-set-position
             (within-limits (add1 LOW-X)
                            (- particle-x INC-DEC-FAC)
                            (sub1 HIGH-X))
             particle-y)))
    
    
    ; increment-pos-vel-y : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the position of particle. Here the particle's
    ;          y coordinate will be incremented by 5
    (define/override (increment-pos-vel-y)
      (send model execute-command
            (make-set-position
             particle-x
             (within-limits (add1 LOW-Y)
                            (+ particle-y INC-DEC-FAC)
                            (sub1 HIGH-Y)))))
    
    
    ; decrement-pos-vel-y : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the position of particle. Here the particle's
    ;          y coordinate will be decremented by 5
    (define/override (decrement-pos-vel-y)
      (send model execute-command
            (make-set-position
             particle-x
             (within-limits (add1 LOW-Y)
                            (- particle-y INC-DEC-FAC)
                            (sub1 HIGH-Y)))))
    
    
    ; get-title-text : -> String
    ; GIVEN : No arguments
    ; RETURNS : String representing the title for this controller
    ;           to be displayed in the view
    (define/override (get-title-text) "Arrow keys change position")
    
    
    ; Local functions
    
    ; within-limits : PosInt Real PosInt -> PosReal
    ; GIVEN : The low value of boundary, current position of particle and 
    ;         high value of boundary. The values are all x values or y
    ;         values of boundary and particle.
    ; RETURNS : The maximum value among low value and minimum of current
    ;           and high value
    (define/public (within-limits lo val hi)
      (max lo (min val hi)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (local
    ((define MODEL (new Model%))
     (define POS-CONTR (new PositionController% [model MODEL]))
     
     (define DATA-IMG (above
                       (text (send POS-CONTR get-title-text) 11 "red")
                       (text (string-append "X = " "80.0" " Y = " "45.0")
                             11 "red")
                       (text (string-append "VX = " "0.0" " VY = " "0.0")
                             11 "red")))
     
     (define TEXT-INSIDE-RECT 
       (overlay
        DATA-IMG
        (rectangle
         (max 150 (+ (image-width DATA-IMG) 10))
         (max 50 (+ (image-height DATA-IMG) 10))
         "outline" "black")))
     
     (define VIEWER-IMG (overlay/align "left" "top"
                                       (square 10 "outline" "black")
                                       TEXT-INSIDE-RECT))
     
     (define POS-CONTR-IMG (place-image VIEWER-IMG 420 370
                                        (empty-scene 600 500))))
    
    ; Mouse events on object
    (send POS-CONTR after-button-down 226 226)
    
    (check-equal?
     (send POS-CONTR for-test:get-handle-sel?)
     true)
    
    (send POS-CONTR after-drag 346 346)
    (send POS-CONTR after-button-up 346 346)
    
    ; Checks the x value of object after dragging the handle
    (check-equal?
     (send POS-CONTR for-test:get-x)
     420)
    
    ; Checks the y value of object after dragging the handle
    (check-equal?
     (send POS-CONTR for-test:get-y)
     370)
    
    (send POS-CONTR after-button-down 420 370)
    
    (check-equal?
     (send POS-CONTR for-test:get-controller-sel?)
     true)
    
    ; Series of key events on object
    (send POS-CONTR after-key-event "up")
    (send POS-CONTR after-key-event "up")
    (send POS-CONTR after-key-event "down")
    (send POS-CONTR after-key-event "left")
    (send POS-CONTR after-key-event "right")
    (send POS-CONTR after-key-event "right")
    
    ; check the x position of particle
    (check-equal?
     (send POS-CONTR for-test:get-particle-x)
     80)
    
    ; check the y position of particle
    (check-equal?
     (send POS-CONTR for-test:get-particle-y)
     45)
    
    (check-equal?
     (send POS-CONTR add-to-scene (empty-scene 600 500))
     POS-CONTR-IMG)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;