;; Interfaces.rkt
;; ;; The file contains the constants, interfaces and data definitions for mvc.rkt

;; It provides
;; INTERFACES :
;; SWidget<%>
;; Controller<%>
;; Model<%>
;; PostionVelocity<%>
;; XYController<%>

;; DATA DEFINITIONS :
;; ListOfController<%>

;; CONSTANTS :
;; CANVAS-WIDTH
;; CANVAS-HEIGHT
;; HALF-CANVAS-WIDTH
;; HALF-CANVAS-HEIGHT
;; LOW-X
;; HIGH-X
;; LOW-Y
;; HIGH-Y


#lang racket

(provide
 SWidget<%>
 Controller<%>
 Model<%>
 PostionVelocity<%>
 XYController<%>
 (struct-out set-position) 
 (struct-out set-velocity)
 (struct-out report-position)
 (struct-out report-velocity)
 CANVAS-WIDTH
 CANVAS-HEIGHT
 HALF-CANVAS-WIDTH
 HALF-CANVAS-HEIGHT
 LOW-X
 HIGH-X
 LOW-Y
 HIGH-Y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS :

;; Constants of Canvas :
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;; Boundaries of Controller :
(define LOW-X 0)
(define HIGH-X 150)
(define LOW-Y 0)
(define HIGH-Y 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; Structure set-position :
(define-struct set-position (x-pos y-pos) #:transparent)
;; It is used to set the position of particle after each tick, key event on
;; position controllers, and drag event on x, y or xy controllers

;; CONSTRUCTOR TEMPLATE :
;; set-position is a (make-set-position PosReal PosReal)

;; INTERPRETATION :
;; (make-set-position PosReal PosReal) sets the particle to x-pos and y-pos, where
;; -- x-pos : represents the x-coordinate of particle
;; -- y-pos : represents the y-coordinate of particle

;; DESTRUCTOR TEMPLATE :
;; set-position-fn : set-position -> ??
#;(define (set-position-fn pos)
    (...
     (set-position-x-pos pos)
     (set-position-y-pos pos) ...))



;; Structure set-velocity :
(define-struct set-velocity (vx vy) #:transparent)
;; It is used to set the velocity of particle after each key event on
;; velocity controllers

;; CONSTRUCTOR TEMPLATE :
;; set-velocity is a (make-set-velocity Int Int)

;; INTERPRETATION :
;; (make-set-velocity Int Int) sets the velocity of particle to vx and vy, where
;; -- vx : represents the x velocity of particle
;; -- vy : represents the y velocity of particle

;; DESTRUCTOR TEMPLATE :
;; set-velocity-fn : set-velocity -> ??
#;(define (set-velocity-fn vel)
    (...
     (set-velocity-vx vel)
     (set-velocity-vy vel) ...))



;; Structure report-position :
(define-struct report-position (x-pos y-pos) #:transparent)
;; It is used to report the change in position of particle after tick events to
;; each controller

;; CONSTRUCTOR TEMPLATE :
;; report-position is a (make-report-position PosReal PosReal)

;; INTERPRETATION :
;; (make-report-position PosReal PosReal) reports the change in position particle
;; where, 
;; -- x-pos : represents the x position of particle
;; -- y-pos : represents the y position of particle

;; DESTRUCTOR TEMPLATE :
;; report-position-fn : report-position -> ??
#;(define (report-position-fn pos)
    (...
     (report-position-x-pos pos)
     (report-position-y-pos pos) ...))



;; Structure report-velocity :
(define-struct report-velocity (vx vy) #:transparent)
;; It is used to report the change in velocity of particle after tick events to
;; each controller

;; CONSTRUCTOR TEMPLATE :
;; report-velocity is a (make-report-velocity Int Int)

;; INTERPRETATION :
;; (make-report-velocity Int Int) reports the change in velocity of particle ,where
;; -- vx : represents the x velocity of particle
;; -- vy : represents the y velocity of particle

;; DESTRUCTOR TEMPLATE :
;; report-velocity-fn : report-velocity -> ??
#;(define (report-velocity-fn vel)
    (...
     (report-velocity-vx vel)
     (report-velocity-vy vel) ...))



;; Structure Command :
;; A Command is one of 
;; -- (make-set-position (PosReal PosReal))
;; -- (make-set-velocity (Int Int))

;; INTERPRETATION :
;; A Command is used to tell Model to report the change in position or velocity
;; of particle based on the value after key events on position or velocity
;; controller or after drag event on x, y or xy controllers.

;; DESTRUCTOR TEMPLATE :
;; command-fn : Command ->
#;(define (command-fn cmd)
    (cond
      [(set-position? cmd) ... ]
      [(set-velocity? cmd) ... ]))



;; Structure Signal :
;; A Signal is one of
;; -- (make-report-position (NonNegInt NonNegInt))
;; -- (make-report-velocity (Int Int))

;; INTERPRETATION :
;; A Signal is used to tell each controller to report the change in position or
;; velocity of particle based on the value after tick event in Model.

;; DESTRUCTOR TEMPLATE :
;; signal-fn : Signal ->
#;(define (signal-fn sig)
    (cond
      [(report-position? sig) ... ]
      [(report-velocity? sig) ... ]))



;; Structure ListOfController<%> :
;; ListOfController<%> contains the list of controllers present in the Model
;; A ListOfController<%> (LOC) is either
;; -- empty
;; -- (cons Controller<%> LOC)

;;; INTERPRETATION :
;;; empty : a sequence of Controller<%> with no elements
;;; (cons Controller<%> LOC) : a sequence of Controller<%> whose first element is a
;;                             Controller<%> and whose other elements are
;;                             represented by LOC

;;; DESTRUCTOR TEMPLATE :
;;; loc-fn : LOC -> ??
#;(define (loc-fn loc)
    (cond
      [(empty? loc) ...]
      [else
       (... (... (first loc))
            (loc-fn (rest loc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES :

;; SWidget<%> :
;; Every stateful object that lives in the Model must implement the SWidget<%>
;; interface.
;; It provides methods for tick, mouse, key and add to scene events.
(define SWidget<%>
  (interface ()               

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this widget to the state it should have
    ; following a tick
    after-tick

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location
    after-button-up        
    after-button-down      
    after-drag

    ; KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    after-key-event

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it
    add-to-scene
    ))



;; Controller<%> :
;; Every object the lives in Model implements Contoller<%>
;; It provides funtion to recieve a signal from Model, and based
;; on value of signal it changes the velocity or position of particle.
(define Controller<%>    
  (interface (SWidget<%>)  ;; include all methods of SWidget<%>

    ; Signal -> Void
    ; EFFECT : Receives a signal from the model and adjust controller
    ;          accordingly 
    receive-signal
    ))



;; PostionVelocity<%> :
;; Every subclass of PosVelController% implements both Controller<%>
;; and PostionVelocity<%> interfaces.
(define PostionVelocity<%>
  (interface (Controller<%>)  ;; include all methods of Controller<%>

    ; : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the velocity of particle. Here the particle's
    ;          x velocity will be incremented by 5
    increment-pos-vel-x

    ; : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the velocity of particle. Here the particle's
    ;          x velocity will be decremented by 5
    decrement-pos-vel-x

    ; : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the velocity of particle. Here the particle's
    ;          y velocity will be incremented by 5
    increment-pos-vel-y

    ; : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the model by sending it a command to
    ;          update the velocity of particle. Here the particle's
    ;          y velocity will be decremented by 5
    decrement-pos-vel-y

    ; : -> String
    ; GIVEN : No arguments
    ; RETURNS : String representing the title for this controller
    ;           to be displayed in the view
    get-title-text
    ))



;; XYController<%> :
;; Every subclass of ParticleViewController% implements both Controller<%>
;; and XYController<%>.
(define XYController<%>
  (interface (Controller<%>)  ;; include all methods of Controller<%>

    ; : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : Updates the saved-mx field of this Controller
    change-saved-mx-my

    ; : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : Updates the x position of particle
    change-particle-x-y

    ; : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True, iff the give mouse location is inside the handle
    ; STRATEGY : Combine simpler functions
    inside-handle?

    ; : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True, iff the give mouse location is inside the controller
    ; STRATEGY : Combine simpler functions
    inside-controller?

    ; : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : An image of a rectangle, representing the black
    ;           outer rectangle of XController
    outer-rect-image

    ; : -> Scene
    ; GIVEN : No arguments
    ; RETURNS : An image containg the inner blue rectangle of controller
    ;           with image of particle placed on it
    ; STRATEGY : Combine simpler functions
    inner-rect-with-particle
    ))

    
    
;; Model<%> :
;; Every object of Model% class implements the Model<%>
;; It provides functions for tick, registering the controller with model and
;; executing commands that controller sends to it.
(define Model<%>
  (interface ()

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this model to the state it should have
    ; following a tick
    after-tick        

    ; Controller<%> -> Void
    ; Registers the given controller to receive signal
    register          

    ; Command -> Void
    ; Executes the given command
    execute-command
))



;; PROTOCOL : 
;; -- Model sends the controller an initialization signal as soon as it registers.
;; -- Model receives commands to update the position or velocity of particle after
;;    key events on position or velocity controllers and mouse events on x, y or
;;    xy controllers.
;; -- Each controller receives a signal to change the state of postion or velocity
;;    of particle after each ticks.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;