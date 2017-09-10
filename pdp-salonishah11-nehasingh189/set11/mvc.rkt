;; mvc.rkt
;; The system simulates a dimensionless particle bouncing in a 150 x 100 rectangle.
;; System consists of 5 view controllers
;; - position : arrow keys control the position of particle
;; - velocity : arrow keys control the velocity of particle
;; - x : display's the motion of particle only in x direction
;; - y : display's the motion of particle only in y direction
;; - xy : display's the motion of particle in both x & y direction

;; Hitting 'p', 'v', 'x', 'y' and 'z' creates the above view controllers.

;; Each controller has a 10x10 handle. Dragging on the handle moves the controller
;; around the canvas.

;; A button-down inside a controller selects the controller for input.

;; In the position or velocity controller, the arrow keys are used for input.
;; The arrow keys alter the position or velocity of the particle in the indicated
;; direction. Each press of an arrow key alters the appropriate quantity by 5.

;; In the X, Y, or XY controller, the mouse drags the particle via smooth drag.
;; The mouse need not be in the representation of the particle; it need only be in
;; the controller.

;; This a top level file.
;; It provides run function.


#lang racket
(require rackunit)
(require "Model.rkt")
(require "ParticleWorld.rkt")
(require "ControllerFactory.rkt")

(provide run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosReal -> Void
;; GIVEN : a frame rate, in sec/tick
;; EFFECT : Creates and runs the MVC simulation with the given frame rate.
(define (run rate)
  (let* ((model (new Model%))
         (world (make-world model 600 500)))
    (begin
      (send world add-widget
            (new ControllerFactory% [model model][world world]))
      (send world run rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;