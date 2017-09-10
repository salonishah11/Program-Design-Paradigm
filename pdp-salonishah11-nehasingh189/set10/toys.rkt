;; toys.rkt                                                                  
;; Simulate the following marvelous toy using the a "framework" from          
;; WidgetWorks International.                                                 
;; 1) The toy consists of a canvas that is 600 pixels high and 500 pixels wide
;; 2) On the canvas, the system displays a circle of radius 10 in outline mode
;; 3) The circle initially appears in the center of the canvas. We call this  
;;    circle the "target."                                                    
;; 4) The child interacts with the toy by dragging the target                 
;;    using smooth drag, as usual) and by typing characters into the system.  
;; 5) Each of the characters listed below causes a new toy to be created with 
;;    its center located at the center of the target.                         
;; 6) Toys are also moveable using smooth drag.                               
;; 7) When the child types "s", a new square-shaped toy pops up.              
;;    It is represented as a 40x40 pixel outline square.                      
;; 8) When a square-shaped toy appears, it begins travelling rightward at a   
;;    constant rate.                                                          
;; 9) When the child types "t", a new throbber appears. A throbber starts     
;;    as a solid green circle of radius 5. At every tick, it expands gradually
;;    until it reaches a radius of 20. Once it reaches a radius of 20, it     
;;    contracts gradually until it reaches a radius of 5.                     
;; 10) When the child types "w", a clock appears and displays the number of   
;;     ticks since it was created.                                            
;; 11) When the child types "f", a Official Tom Brady Deflatable Football(TM) 
;;     appears.It gets smaller with every tick until it reaches size 0.

;; This a top-level file.

;; It provides the functions make-playground and run.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require rackunit)
(require "WidgetWorks.rkt")
(require "extras.rkt")
(require "target.rkt")
(require "square.rkt")
(require "football.rkt")
(require "clock.rkt")
(require "throbber.rkt")
(require "interfaces_toys.rkt")
(require "playgroundstate_toys.rkt")
(require 2htdp/universe)   
(require 2htdp/image)
(check-location "10" "toys.rkt")

(provide
 make-playground
 run) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-playground : PosInt  -> PlaygroundState<%>
;; GIVEN : The speed with which the square toy travels
;; RETURNS : A PlaygroundState<%> with a target, but no toys, and in
;;           which any square toys created in the future will travel 
;;           at the given speed (in pixels/tick).
(define (make-playground speed)
  (new PlaygroundState%
       [target (make-target)]
       [sq-toy-speed speed]
       [toys empty]))

;; TESTS :
(begin-for-test
  (local
    (; Initial playground state
     (define PLAYGROUND-1 (make-playground 10)))

    ; Checks list of toys in PLAYGROUND-1
    (check-equal?
     (send PLAYGROUND-1 get-toys)
     empty)

    ; Checks the x-ccordinate of target
    (check-equal?
     (send PLAYGROUND-1 target-x)
     250)

    ; Checks the y-ccordinate of target
    (check-equal?
     (send PLAYGROUND-1 target-y)
     300)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosNum PosInt -> StatefulWorld<%>
;; GIVEN : The frame rate in secs/tick and the speed with which the
;;         square toy travels
;; RETURNS : An object of class that implements StatefulWorld<%>
(define (run rate speed)
  (local
    (; Object of class that implements StatefulWorld<%>
     (define the-world (make-world CANVAS-WIDTH CANVAS-HEIGHT)))
    (begin
      ; Add playground-object to stateful objects of world
      (send the-world add-stateful-widget (make-playground speed))
      ; Runs the world
      (send the-world run rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;