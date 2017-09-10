;; ParticleController.rkt
;; It is the super class for XController%, YController% and XYController%
;; It provides ParticleController% class.

#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")

(provide ParticleViewController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class ParticleViewController%
;; Interface : Controller<%>
;; ParticleViewController% is an object of ParticleViewController%
;; ParticleViewController% is draggable through its handle.
;; ParticleViewController% is (new ParticleViewController%
;;                                [model Model<%>]
;;                                [x NonNegInt] [y NonNegInt])
;;                                                

(define ParticleViewController%
  (class* object% (Controller<%>) ; implements Controller<%> interface
    
    ; The model
    (init-field model)  
    
    ; The position of the center of the controller
    ; Default value is 300 and 250
    (init-field [x 300] [y 250])
    
    
    
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
    
    ; Mode for rectangles & circle
    (field [MODE-OUTLINE "outline"] [MODE-SOLID "solid"])
    
    ; Colors for displaying outer and inner rectangles
    (field [COLOR-OUTER-RECT "black"])
    (field [COLOR-INNER-RECT "blue"]) 
    
    ; Alignment for overlay
    (field [ALIGN-MIDDLE "middle"])
    (field [ALIGN-LEFT "left"])
    (field [ALIGN-TOP "top"])
    
    ; Images of inner and outer circle for displaying particle
    (field [INNER-CIRLE-IMG (circle 1 "solid" "black")])
    (field [OUTER-CIRLE-IMG (circle 10 "solid" "red")])
    
    ; Length of Handle
    (field [HANDLE-LEN 10])
    
    ; Color for displaying selected and unselected handle
    (field [HANDLE-SEL "red"] [HANDLE-UNSEL "black"])
    
    
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
    ;          inside handle or controller. If button-down is inside controller,
    ;          model is sent a command to change the value for its
    ;          pause-after-tick-? field.
    ; DESIGN STRATEGY : Cases on whether mouse is inside handle or controller
    (define/public (after-button-down mx my)
      (cond
        [(send this inside-handle? mx my)
         (begin
           (set! handle-sel? true)
           (set! saved-mx (- mx x))
           (set! saved-my (- my y)))]
        [(send this inside-controller? mx my)
         (begin
           (set! controller-sel? true)
           (send this change-saved-mx-my mx my)
           (send model execute-command controller-sel?))] 
        [else this]))
    
    
    ; to be supplied by subclasses
    (abstract inside-handle?)
    (abstract inside-controller?)
    (abstract change-saved-mx-my)
    
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN : The mouse location
    ; EFFECT : Updates this controller to the state following the drag event.
    ;          If controller is dragged, it sends model command to update
    ;          the position of particle
    ; DESIGN STRATEGY : Cases on whether handle or controller is selected
    (define/public (after-drag mx my)
      (cond
        [(equal? handle-sel? true)
         (begin
           (set! x (- mx saved-mx))
           (set! y (- my saved-my)))]
        [(equal? controller-sel? true)
         (begin
           (send this change-particle-x-y mx my)
           (send model execute-command (make-set-position
                                        particle-x
                                        particle-y)))]
        [else this]))
    
    
    ; to be supplied by subclasses
    (abstract change-particle-x-y)
    
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : The mouse location
    ; EFFECT : Updates the controller to state after button up
    ;          event. It aslo send model a command to change the value
    ;          of its pause-after-tick? field.
    (define/public (after-button-up mx my)
      (begin
        (set! handle-sel? false)
        (set! controller-sel? false)
        (send model execute-command controller-sel?)))
    
    
    ; after-tick : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates this controller to the state it should have
    ;          following a tick
    ; DETAILS : Tick events are not handle in controller
    (define/public (after-tick) 'viewer2-after-tick-trap)
    
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN : A key event
    ; EFFECT : Updates this object to the state it should have
    ;          following the given key event
    ; DETAILS : Controllers ignore the key events
    (define/public (after-key-event kev) this)
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this controller painted
    ;           on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image
       (overlay/align ALIGN-MIDDLE ALIGN-MIDDLE
                      (outer-inner-rect-with-particle-img)
                      (send this inner-rect-with-particle))
       x y
       scene))
    
    
    ; to be supplied by subclasses
    (abstract inner-rect-with-particle)  
    
    
    ; Local functions
    
    ; current-color-handle : -> String
    ; GIVEN : No arguments
    ; RETURNS : String representing the color of handle
    ; DESIGN STRATEGY : Divide into cases based on handle-sel?
    (define (current-color-handle)
      (if handle-sel? HANDLE-SEL HANDLE-UNSEL))
    
    
    ; outer-inner-rect-with-particle-img : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : Scene containing the outer and inner rectangles,
    ;           along with image of particle painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define (outer-inner-rect-with-particle-img)
      (overlay/align ALIGN-LEFT ALIGN-TOP
                     (square HANDLE-LEN
                             MODE-OUTLINE
                             (current-color-handle))
                     (send this outer-rect-image)))
    
    
    ; to be supplied by subclasses
    (abstract outer-rect-image)
    
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