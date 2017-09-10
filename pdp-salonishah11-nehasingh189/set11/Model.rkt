;; Model.rkt
;; The model consists of a particle, bouncing with its inside a 150 x 100
;; rectangle. It accepts commands and reports when its status changes.

;; The file provide the Model% class.


#lang racket
(require rackunit)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "VelocityController.rkt")

(provide Model%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class Model%
;; Interface : Model<%>
;; Model is an object of Model%
;; A Model is (new Model% [x PosReal] [vx Int]
;;                        [y PosReal] [vy Int]
;;                        [pause-after-tick? Boolean]
;;                        [controllers ListOfController<%>])

(define Model%
  (class* object% (Model<%>)  ; implements Model<%> interface
    
    ; x position and velocity of the particle
    ; By default x is at center of canvas of controller, ie 75
    (init-field [x (/ (+ LOW-X HIGH-X) 2)])
    (init-field [vx 0])
    
    ; y position and velocity of the particle
    ; By default y is at center of canvas of controller, ie 50
    (init-field [y (/ (+ LOW-Y HIGH-Y) 2)])
    (init-field [vy 0])
    
    ; Represents whether the tick event has to be paused or not
    ; If there is a button-down or drag event on x, y or xy controllers
    ; tick event in Model has to be paused
    ; True indicates that tick event has to be paused
    (init-field [pause-after-tick? false])
    
    ; ListOfController<%> in Model
    (init-field [controllers empty])   
    
    
    (super-new)
    
    
    ; after-tick : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Moves the object by vx and vy.
    ;          If the resulting
    ;          x is >= 150 or <= 0 and y is >=100 and <= 0
    ;          -- it reports x and y at each tick
    ;          -- it reports velocity vx and vy at each tick
    (define/public (after-tick)
      (if (not pause-after-tick?)
          (local
            (; x-coordinate of particle after limiting its value within x-boundaries
             (define limited-x (within-limits (add1 LOW-X) x (sub1 HIGH-X)))
             ; y-coordinate of particle after limiting its value within y-boundaries
             (define limited-y (within-limits (add1 LOW-Y) y (sub1 HIGH-Y)))
             ; Particle structure
             (define particle (make-particle limited-x limited-y vx vy))
             ; Rectangle structure
             (define rect (make-rect LOW-X HIGH-X LOW-Y HIGH-Y))
             ; Particle at next tick
             (define p-at-next-tick (particle-after-tick particle rect)))
            (begin
              (set! x (particle-x p-at-next-tick))
              (set! y (particle-y p-at-next-tick))
              (set! vx (particle-vx p-at-next-tick))
              (set! vy (particle-vy p-at-next-tick))
              ; publishes new position of particle to each controller
              (publish-position)
              ; publishes new velocity of particle to each controller
              (publish-velocity)))
          this))
    
    
    ; register : Controller<%> -> Void
    ; GIVEN : An object of class that implements Controller<%>
    ; EFFECT : Registers the new controller and sends it the current
    ;          position and velocity of controller
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position x y))
        (send c receive-signal (make-report-velocity vx vy))))
    
    
    ; execute-command : Command -> Void
    ; GIVEN : A Command
    ; EFFECT : Decodes the command, executes it, and sends updates to each
    ;          controllers if the command is set-position or set-velocity.
    ;          If command is Boolean, it updates the pause-after-tick?
    ;          field of Model.
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           (set! x (set-position-x-pos cmd))
           (set! y (set-position-y-pos cmd))
           (publish-position))]
        [(set-velocity? cmd)
         (begin
           (set! vx (set-velocity-vx cmd))
           (set! vy (set-velocity-vy cmd))
           (publish-velocity))]
        [(boolean? cmd)
         (set! pause-after-tick? cmd)]))
    
    
    ; Local functions
    
    ; within-limits : PosInt Real PosInt -> PosReal
    ; GIVEN : The low value of boundary, current position of particle and 
    ;         high value of boundary. The values are all x values or y
    ;         values of boundary and particle.
    ; RETURNS : The maximum value among low value and minimum of current
    ;           and high value
    (define (within-limits lo val hi)
      (max lo (min val hi)))
    
    
    ; publish-position :  -> Void
    ; GIVEN : No arguments
    ; EFFECT : Reports position to each controller
    (define (publish-position)
      (let ((msg-pos (make-report-position x y)))
        (for-each
         ; Controller<%> -> Void
         ; GIVEN : An object of class that implements Controller<%>
         ; EFFECT : Updates the fields in controller which contains the
         ;          x and y values of particle
         (lambda (sobjs) (send sobjs receive-signal msg-pos))
         controllers)))
    
    
    ; publish-velocity : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Reports velocity to each controller
    (define (publish-velocity)
      (let ((msg-vel (make-report-velocity vx vy)))
        (for-each
         ; Controller<%> -> Void
         ; GIVEN : An object of class that implements Controller<%>
         ; EFFECT : Updates the fields in controller which contains the
         ;          vx and vy values of particle
         (lambda (sobjs) (send sobjs receive-signal msg-vel))
         controllers)))
    
    
    
    ; Functions for testing
    
    (define/public (for-test:get-x) x)
    (define/public (for-test:get-y) y)
    (define/public (for-test:get-vx) vx)
    (define/public (for-test:get-vy) vy)
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  (local
    ((define MODEL (new Model%))
     (define VEL-CONTR (new VelocityController% [model MODEL])))
    
    (send MODEL register VEL-CONTR)
    
    ; Send set-position and set-velocity commands 
    (send MODEL execute-command (make-set-position 80 80))
    (send MODEL execute-command (make-set-velocity 5 -5))
    
    ; Model after 3 ticks
    (send MODEL after-tick)
    (send MODEL after-tick)
    (send MODEL after-tick)
    
    ; Check the x position of particle
    (check-equal?
     (send MODEL for-test:get-x)
     95)
    
    ; Check the y position of particle
    (check-equal?
     (send MODEL for-test:get-y)
     65)
    
    ; Check the x velocity of particle
    (check-equal?
     (send MODEL for-test:get-vx)
     5)
    
    ; Check the y velocity of particle
    (check-equal?
     (send MODEL for-test:get-vy)
     -5)
    
    ; Send boolean command to pause-after-tick?
    (send MODEL execute-command true)
    
    ; Model after 1 ticks
    (send MODEL after-tick)
    
    ; Check the x position of particle
    (check-equal?
     (send MODEL for-test:get-x)
     95)
    
    ; Check the y position of particle
    (check-equal?
     (send MODEL for-test:get-y)
     65)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;