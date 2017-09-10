;; cubelets.rkt
;; Cubelets are square blocks that stick together.
;; When the child types "b", a new block pops up on the screen at the location of
;; the last button-up. The block appears as a 20x20 outline square.
;; The square is initially green.

;; A block does not move by itself, but the child can move it around using
;; Smooth Drag. When the block is selected, it appears as red rather than green.
;; If a block is dragged so that it contacts or overlaps another block,
;; the two blocks become connected. We say that the blocks are teammates.

;; The property of being a teammate is symmetric and transitive. So if block A
;; is moved to touch block B, then a new team is formed consisting of A and all
;; its teammates, and B and all its teammates.

;; This a top-level file.

;; This file provides the function run.


#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "playgroundstate_cubelets.rkt")
(require "block.rkt")
(require "interfaces_cubelets.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "10" "cubelets.rkt")

(provide run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : -> StatefulWorld<%> 
;; GIVEN : No arguments
;; RETURNS : An object of class that implements StatefulWorld<%>

(define (run rate)
  (local
    ((define CANVAS-WIDTH 500)
     (define CANVAS-HEIGHT 600)
     ; Object of class that implements StatefulWorld<%>
     (define world-object (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     ; Object of class that implemets PlaygroundState<%>
     (define playground-object (new PlaygroundState%)))
    (begin
      ; Add playground-object to stateful objects of world
      (send world-object add-stateful-widget playground-object)
      ; Runs the world
      (send world-object run rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;