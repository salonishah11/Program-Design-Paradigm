;; playgroundstate_toys.rkt
;; The file contains the PlaygroundState% class.
;; It is included in toys.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "target.rkt")
(require "square.rkt")
(require "throbber.rkt")
(require "clock.rkt")
(require "football.rkt")
(require "interfaces_toys.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide PlaygroundState%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A PlaygroundState is a (new PlaygroundState% [target SWidget<%>]
;;                                              [sq-toy-speed PosInt]
;;                                              [toys ListOfToy<%>])
;; A PlaygroundState represents the playground where the toys are formed.
;; A PlaygroundState behaves as the place of interaction between the WorldState
;; and the child.

(define PlaygroundState%
  (class* object% (PlaygroundState<%>) ;; implement the interface

    ; Object of class Target%
    (init-field target)

    ; PosInt, which represents the speed of square toy
    (init-field sq-toy-speed)

    ; ListOfToy<%>
    (init-field toys)

    
    ; Private data for objects of this class

    ; Key events
    (field [SQUARE-KEY-EVT "s"])
    (field [THROBBER-KEY-EVT "t"])
    (field [CLOCK-KEY-EVT "w"])
    (field [FOOTBALL-KEY-EVT "f"])

    ; Image of empty Canvas
    (define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
    
    (super-new)

    ; Methods of PlaygroundState<%>
    
    ; target-x : -> Integer
    ; GIVEN : No arguments
    ; RETURNS : the x coordinate of the target.
    (define/public (target-x)
      (send target get-x))

    
    ; target-y : -> Integer
    ; GIVEN : No arguments
    ; RETURNS : the y coordinate of the target
    (define/public  (target-y)
       (send target get-y))

    
    ; target-selected? : -> Boolean
    ; GIVEN : No arguments
    ; RETURNS : A Boolean representing whether the target is
    ;           selected or not.
    (define/public (target-selected?)
        (send target get-selected?))
      
      
    ; get-toys : -> ListOfToy<%>
    ; GIVEN : No arguments
    ; RETURNS : A list of toys maintained as stateful widgets
    (define/public (get-toys) toys)


    ; Methods of SWidget<%>
    
    ; after-tick : -> Void
    ; GIVEN : No arguments
    ; RETURNS : The state of the world at the next tick
    ; DESIGN STRATEGY : Use of for-each on toys
    (define/public (after-tick)
      (process-toys
       ; Toy<%> -> Void
       ; GIVEN : An object of class that implements Toy<%>
       ; EFFECT : Updates the given toy after a tick
       (lambda (toy) (send toy after-tick))))       

    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene that depicts this World
    ; DESIGN STRATEGY : Use HOF foldr on toys
    (define/public (add-to-scene scene)
      (foldr
       ; Toy<%> Scene -> Scene
       ; GIVEN : An object of class that implements Toy<%>
       ;         and a scene
       ; RETURNS : A scene with given toy painted on it
       (lambda (toy scene) (send toy add-to-scene scene))
       (send target add-to-scene EMPTY-CANVAS) 
       toys))    

    
    ; after-button-down : NonNegInteger NonNegInteger -> Void
    ; GIVEN : A mouse location
    ; EFFECT : updates the PlaygroundState to the state it should have
    ;          following given button down event at the given location
    ; STRATEGY : Use HOF for-each on toys
    (define/public (after-button-down mx my)
      (begin
        (send target after-button-down mx my)
        (process-toys
         ; Toy<%> -> Void
         ; GIVEN : An object of class that implements Toy<%>
         ; EFFECT : Updates the given toy after button down event
         (lambda (toy) (send toy after-button-down mx my)))))

    
    ; after-button-up : NonNegInteger NonNegInteger -> Void
    ; GIVEN : A mouse location
    ; EFFECT : updates the PlaygroundState to the state it should have
    ;          following given button up event at the given location
    ; STRATEGY : Use HOF for-each on toys
    (define/public (after-button-up mx my)
      (begin
        (send target after-button-up mx my)
        (process-toys
         ; Toy<%> -> Void
         ; GIVEN : An object of class that implements Toy<%>
         ; EFFECT : Updates the given toy after button up event
         (lambda (toy) (send toy after-button-up mx my)))))

    
    ; after-drag : NonNegInteger NonNegInteger -> Void
    ; GIVEN : A mouse location
    ; EFFECT : updates the PlaygroundState to the state it should have
    ;          following given drag event at the given location
    ; STRATEGY : Use HOF for-each on toys
    (define/public (after-drag mx my)
      (begin
        (send target after-drag mx my)
        (process-toys
         ; Toy<%> -> Void
         ; GIVEN : An object of class that implements Toy<%>
         ; EFFECT : Updates the given toy after drag event
         (lambda (toy) (send toy after-drag mx my)))))

    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN : A key event, 'kev'.
    ; EFFECT : updates the PlaygroundState to the state it should
    ;          have following the given key event
    ; STRATEGY : Divide into cases on KeyEvent 'kev'.
    (define/public (after-key-event kev)
      (cond
        [(key=? kev SQUARE-KEY-EVT)
         (set! toys (cons (make-square-toy (target-x) (target-y)
                                           sq-toy-speed) toys))]
        [(key=? kev THROBBER-KEY-EVT)
         (set! toys (cons (make-throbber (target-x) (target-y)) toys))]
        [(key=? kev CLOCK-KEY-EVT)
         (set! toys (cons (make-clock (target-x) (target-y)) toys))]
        [(key=? kev FOOTBALL-KEY-EVT)
         (set! toys (cons (make-football (target-x) (target-y)) toys))]
        [else this]))

    ; Local functions

    ; process-toys : (Toy<%> -> Void) -> Void
    ; GIVEN : A function which takes an object of class that implements
    ;         Toy<%> and effects the block based on function to be
    ;         implemented
    ; EFFECT : Updates each toys in toys based on the given function
    (define (process-toys fn)
      (for-each fn toys))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST CASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TARGET (new Target%))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS-HALF-WIDTH (/ CANVAS-WIDTH 2))
(define CANVAS-HALF-HEIGHT (/ CANVAS-HEIGHT 2))
(define SQUARE-EVENT "s")
(define THROBBER-EVENT "t")
(define CLOCK-EVENT "w")
(define FOOTBALL-EVENT "f")
(define NOT-VALID-KEYEVENT " ")

;; TESTS :
(begin-for-test
  (local
    (; Initial playground state
     (define initial-playground
       (new PlaygroundState%  [target TARGET] [sq-toy-speed 10] [toys empty]))
     (define x CANVAS-HALF-WIDTH)
     (define y CANVAS-HALF-HEIGHT)
     (define speed 10)
     
     ; Other key event
     (define key-wrong-event "a")
     
     ; Other mouse event
     (define mouse-wrong-event "leave")
     
     ; Image of playground containing football, clock, throbber, square
     ; and target
     (define playground-image
       (send (make-football x y) add-to-scene
             (send (make-clock x y) add-to-scene         
                   (send (make-throbber x y) add-to-scene
                         (send (make-target) add-to-scene
                               (send (make-square-toy x y speed)
                                     add-to-scene EMPTY-CANVAS)))))))
    
    ; initially there are no toys in playgroundstate
    (check-equal? (send initial-playground get-toys)
                  empty
                  "Initial world shouldn't have toys")
    
    ; PlaygroundState after key events and mouse events
    (send initial-playground after-key-event SQUARE-EVENT)
    (send initial-playground after-key-event THROBBER-EVENT)
    (send initial-playground after-key-event CLOCK-EVENT)
    (send initial-playground after-key-event FOOTBALL-EVENT)
    (send initial-playground after-key-event key-wrong-event)
    (send initial-playground after-button-down x y)
    (send initial-playground after-drag x y)
    (send initial-playground after-tick)
    (send initial-playground after-button-up x y)
    
    ; test for add-to-scene
    (check-equal?
     (send initial-playground add-to-scene EMPTY-CANVAS)
     playground-image)
    
    ; check for target-x and target-y of the playgroundState
    (check-true (and (equal? (send initial-playground target-x) x)
                     (equal? (send initial-playground target-y) y)
                     (equal? (send initial-playground target-selected?) false))
                "This world is empty")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
