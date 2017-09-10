;; VelocityController.rkt
;; It displays an outline rectangle with text showing the position
;; and velocity of particle.

;; The rectangle is draggable using the handle.

;; Arrow keys increment/decrement velocity of the particle by 5.

;; The file provides VelocityController% class.

#lang racket
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "PosVelController.rkt")
(require "ModelForTest.rkt")
(require "extras.rkt")

(provide VelocityController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class VelocityController%
;; Interface : PostionVelocity<%>
;; It is a subclass of PosVelController%
;; VelocityController is an object of VelocityController%
;; VelocityController is draggable through its handle.
;; VelocityController is (new VelocityController%)

(define VelocityController%
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
    ;          update the velocity of particle. Here the particle's
    ;          x velocity will be incremented by 5
    (define/override (increment-pos-vel-x)
      (send model execute-command
            (make-set-velocity
             (+ particle-vx INC-DEC-FAC)
             particle-vy)))
    
    
    ; decrement-pos-vel-x : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the velocity of particle. Here the particle's
    ;          x velocity will be decremented by 5
    (define/override (decrement-pos-vel-x)
      (send model execute-command
            (make-set-velocity
             (- particle-vx INC-DEC-FAC)
             particle-vy)))
    
    
    ; increment-pos-vel-y : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the velocity of particle. Here the particle's
    ;          y velocity will be incremented by 5
    (define/override (increment-pos-vel-y)
      (send model execute-command
            (make-set-velocity
             particle-vx
             (+ particle-vy INC-DEC-FAC))))
    
    
    ; decrement-pos-vel-y : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the velocity of particle. Here the particle's
    ;          y velocity will be decremented by 5
    (define/override (decrement-pos-vel-y)
      (send model execute-command
            (make-set-velocity
             particle-vx
             (- particle-vy INC-DEC-FAC))))
    
    
    ; get-title-text : -> String
    ; GIVEN : No arguments
    ; RETURNS : String representing the title for this controller
    ;           to be displayed in the view
    (define/override (get-title-text) "Arrow keys change velocity")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(begin-for-test
  (local
    ((define MODEL (new Model%))
     (define VEL-CONTR (new VelocityController% [model MODEL]))
     
     (define DATA-IMG (above
                       (text (send VEL-CONTR get-title-text) 11 "red")
                       (text (string-append "X = " "75.0" " Y = " "50.0")
                             11 "red")
                       (text (string-append "VX = " "5.0" " VY = " "-5.0")
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
     
     (define VEL-CONTR-IMG (place-image VIEWER-IMG 420 370
                                        (empty-scene 600 500))))
    
    ; Mouse events on object
    (send VEL-CONTR after-button-down 226 226)
    
    (check-equal?
     (send VEL-CONTR for-test:get-handle-sel?)
     true)
    
    (send VEL-CONTR after-drag 346 346)
    (send VEL-CONTR after-button-up 346 346)
    
    ; Checks the x value of object after dragging the handle
    (check-equal?
     (send VEL-CONTR for-test:get-x)
     420)
    
    ; Checks the y value of object after dragging the handle
    (check-equal?
     (send VEL-CONTR for-test:get-y)
     370)
    
    (send VEL-CONTR after-button-down 420 370)
    
    (check-equal?
     (send VEL-CONTR for-test:get-controller-sel?)
     true)
    
    ; Series of key events on object
    (send VEL-CONTR after-key-event "up")
    (send VEL-CONTR after-key-event "up")
    (send VEL-CONTR after-key-event "down")
    (send VEL-CONTR after-key-event "left")
    (send VEL-CONTR after-key-event "right")
    (send VEL-CONTR after-key-event "right")
    
    ; check the x velocity of particle
    (check-equal?
     (send VEL-CONTR for-test:get-particle-vx)
     5)
    
    ; check the y velocity of particle
    (check-equal?
     (send VEL-CONTR for-test:get-particle-vy)
     -5)
    
    (check-equal?
     (send VEL-CONTR add-to-scene (empty-scene 600 500))
     VEL-CONTR-IMG)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;