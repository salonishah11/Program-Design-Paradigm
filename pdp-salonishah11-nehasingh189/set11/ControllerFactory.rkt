;; ControllerFactory.rkt
;; It provides ControllerFactory%.
;; It has information about world and model.

#lang racket
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XYController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "ModelForTest.rkt")
(require "ParticleWorld.rkt")


(provide ControllerFactory%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class ControllerFactory%
;; Interface : SWidget<%>
;; A ControllerFactory is an object of ControllerFactory%
;; It contains the information about the world, in which controllers live and
;; model, to which the controllers are connected.

;; A ControllerFactory is (new ControllerFactory% [world World<%>]
;;                                                [model Model<%>])

(define ControllerFactory%
  (class* object% (SWidget<%>) ; implement SWidget<%> interface
    
    ; the world in which the controllers will live
    (init-field world)   
    
    ; the model to which the controllers will be connected
    (init-field model)
    
    
    ; Key events
    (field [NEW-VEL-CONT "v"])
    (field [NEW-POS-CONT "p"])
    (field [NEW-X-CONT "x"])
    (field [NEW-Y-CONT "y"])
    (field [NEW-XY-CONT "z"])
    
    
    (super-new)
    
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN: A key event
    ; EFFECT: Updates this controller factory to the state it should have
    ;         following the given key event
    ; DESIGN STRATEGY : Divide into cases based on key event kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-VEL-CONT) (add-viewer VelocityController%)]
        [(key=? kev NEW-POS-CONT) (add-viewer PositionController%)]
        [(key=? kev NEW-X-CONT) (add-viewer XController%)]
        [(key=? kev NEW-Y-CONT) (add-viewer YController%)]
        [(key=? kev NEW-XY-CONT) (add-viewer XYController%)]))
    
    
    ; add-viewer : Controller<%> -> Void
    ; GIVEN : Class that implements Controller<%> interface, whose
    ;         new object is to be created
    ; EFFECT : Updates the world by adding an object of the given class
    (define/public (add-viewer viewer-class)
      (send world add-widget (new viewer-class [model model])))
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : The given scene
    (define/public (add-to-scene scene) scene)
    
    
    ; after-tick : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates this ControllerFactory to the state it should have
    ;          following a tick
    ; DETAILS : Tick events are not handle in ControllerFactory
    (define/public (after-tick) 'controller-factory-after-tick-trap)
    
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN : A nouse location
    ; EFFECT : Updates this ControllerFactory to the state it should have
    ;          following a button-down mouse event
    ; DETAILS : Mouse events are not handle in ControllerFactory
    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap)
    
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates this ControllerFactory to the state it should have
    ;          following a drag mouse event
    ; DETAILS : Mouse events are not handle in ControllerFactory
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap)
    
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates this ControllerFactory to the state it should have
    ;          following a button-up mouse event
    ; DETAILS : Mouse events are not handle in ControllerFactory
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (local
    ((define test-model (new Model%))
     (define test-world (make-world test-model 500 500))
     (define event-fact (new ControllerFactory%
                             [model test-model][world test-world]))
     (define EMPTY-CANVAS (empty-scene 600 500)))
    
    ; testing key events
    (send event-fact after-key-event "v")
    (send event-fact after-key-event "p")
    (send event-fact after-key-event "x")
    (send event-fact after-key-event "y")
    (send event-fact after-key-event "z")
    ;    (check-equal? (length (get-field widgets test-world)) 5
    ;                  "There are 5 objects in the world")))
    
    ; test for add-to-scene
    (check-equal?
     (send event-fact add-to-scene EMPTY-CANVAS)
     EMPTY-CANVAS)
    
    
    ;test for mouse events
    (send event-fact after-tick)
    (send event-fact after-button-down 300 250)
    (send event-fact after-drag 350 300)
    
    (check-equal?
     (send event-fact after-button-up 350 300)
     'controller-factory-after-button-up-trap
     "Mouse events are not handle in ControllerFactory")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  