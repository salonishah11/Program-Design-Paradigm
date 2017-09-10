;; toys.rkt
;; The toy consists of a canvas that is 600 pixels high and 500 pixels wide.
;; On the canvas, the system displays a circle of radius 10 in outline mode.
;; The circle initially appears in the center of the canvas.We call this circle
;; the "target." The child interacts with the target by dragging the toy and
;; typing characters in the system.

#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "09" "toys.rkt")

(provide
 make-world
 run
 make-square-toy
 make-throbber
 make-clock
 make-football
 WorldState<%> 
 Widget<%> 
 PlaygroundState<%>
 Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS :

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;; Mouse events
(define BUTTON-DOWN "button-down")
(define DRAG "drag")
(define BUTTON-UP "button-up")

;; Key events
(define NEW-SQUARE "s")
(define NEW-THROBBER "t")
(define NEW-CLOCK "w")
(define NEW-FOOTBALL "f")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; ListOfToy<%> :
;; A ListOfToy<%> is one of :
;; -- empty
;; -- (cons Toy<%> ListOfToy<%>)

;; INTERPRETATION :
;; empty                      : a sequence of Toy<%> with no elements
;; (cons Toy<%> ListOfToy<%>) : a sequence of Toy<%> whose first element is a
;;                              Toy<%> and whose other elements are
;;                              represented by ListOfToy<%>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES :

;; The object of class Target% implements Target<%>
(define Target<%>
  (interface ()
    
    ; -> Int
    ; RETURNS : The x and y coordinates of the target
    get-target-x
    get-target-y
    
    ; -> Boolean
    ; RETURNS : True if the target is selected, else false
    get-target-selected?
    
    ; NonNegInt NonNegInt -> Target<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of this object that should follow the
    ;           specified mouse event at the given location
    after-button-down
    after-button-up
    after-drag
    
    ; Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    add-to-scene
    ))



;; Every object that lives in the Toy must implement the Widget<%> interface
(define Widget<%>
  (interface ()
    
    ; -> Widget<%>
    ; GIVEN : No arguments
    ; RETURNS : The state of this object at the next tick
    after-tick
    
    ; NonNegInt NonNegInt -> Widget<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of this object that should follow the
    ;           specified mouse event at the given location
    after-button-down
    after-button-up
    after-drag
    
    ; KeyEvent -> Widget<%>
    ; GIVEN : A key event
    ; RETURNS : The state of this object that should follow the
    ;           given key event
    after-key-event
    
    ; Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    add-to-scene
    ))



;; Every object that lives in the World must implement the Toy<%> interface
(define Toy<%>
  (interface (Widget<%>)  ; this means : include all the methods of
                          ; Widget<%>.
    
    ; -> Int
    ; RETURNS : The x or y position of the center of the toy
    toy-x
    toy-y
    
    ; -> Int
    ; RETURNS : Some data related to the toy.  The interpretation of
    ;           this data depends on the class of the toy. For
    ;           -- Square   : it is the velocity of the square (rightward is
    ;                         positive)
    ;           -- Throbber : it is the current radius of the throbber
    ;           -- Clock    : it is the current value of the clock
    ;           -- Football : it is the current size of the football (in
    ;                         arbitrary units; bigger is more)
    toy-data
    ))



;; Every object that implements PlaygroundState<%> must implement WorldState<%>
(define WorldState<%>
  (interface ()
    
    ; -> WorldState<%>
    ; GIVEN : No arguments
    ; RETURNS : The state of the world at the next tick
    after-tick
    
    ; NonNegInt NonNegInt MouseEvent-> WorldState<%>
    ; GIVEN : A location and mouse event
    ; RETURNS : The state of the world that should follow the
    ;           given mouse event at the given location
    after-mouse-event
    
    
    ; KeyEvent : KeyEvent -> WorldState<%>
    ; GIVEN : A key event
    ; RETURNS : The state of the world that should follow the
    ;           given key event
    after-key-event
    
    ; -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene that depicts this World
    to-scene
    ))



;; The World implements the PlaygroundState<%> interface
(define PlaygroundState<%>
  (interface (WorldState<%>) ; this means: include all the methods of
                             ; WorldState<%>
    
    ; -> Int
    ; RETURNS : The x and y coordinates of the target
    target-x
    target-y
    
    ; -> Boolean
    ; RETURNS : True if the target is selected, else false
    target-selected?
    
    ; -> ListOfToy<%>
    ; RETURNS : The list of toys
    get-toys
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-world : PosInt -> PlaygroundState<%>
;; GIVEN : A PosInt representing the speed of square toy
;; RETURNS : A world with a target, but no toys, and in which any
;;           square toys created in the future will travel at the given speed
;;             (in pixels/tick)

;; DESIGN STRATEGY : Call a more General function make-worldstate
(define (make-world sq-speed)
  (make-worldstate
   (make-target HALF-CANVAS-WIDTH
                HALF-CANVAS-HEIGHT
                0 0
                false)
   sq-speed
   empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosNum PosInt -> PlaygroundState<%>
;; GIVEN : A frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;;         creates and runs a world in which square toys travel at the given
;;         speed
;; RETURNS : The final state of the world

(define (run rate sq-speed)
  (big-bang
   (make-world sq-speed)
   (on-tick
    (lambda (w) (send w after-tick))
    rate)
   (on-mouse
    (lambda (w mx my mev) (send w after-mouse-event mx my mev)))
   (on-key
    (lambda (w kev) (send w after-key-event kev)))
   (on-draw
    (lambda (w) (send w to-scene)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS World%
;; Interface : PlaygroundState<%>
;; World is an object of class World%
;; World is (new World% [target Target] [sq-toy-speed PosInt] [toys ListOfToy])

(define World%
  (class* object% (PlaygroundState<%>)
    
    ; Object of class Target%
    (init-field target)
    
    ; PosInt, which represents the speed of square toy
    (init-field sq-toy-speed)
    
    ; ListOfToy<%>
    (init-field toys)
    
    
    (super-new)
    
    
    ; Methods of PlaygroundState<%>
    
    ; target-x : -> Int
    ; -> Int
    ; RETURNS : The x coordinate of the target
    (define/public (target-x) (send target get-target-x))
    
    
    ; target-y : -> Int
    ; -> Int
    ; RETURNS : The y coordinate of the target
    (define/public (target-y) (send target get-target-y))
    
    
    ; target-selected? : -> Boolean
    ; RETURNS : True if the target is selected, else false
    (define/public (target-selected?) (send target get-target-selected?))
    
    
    ; get-toys : -> ListOfToy<%>
    ; RETURNS : The list of toys
    (define/public (get-toys) toys)
    
        
    ; Methods of WorldState<%>
    
    ; after-tick : -> PlaygroundState<%>
    ; GIVEN : No arguments
    ; RETURNS : The state of the world at the next tick
    ; DESIGN STRATEGY : Call a more General function make-worldstate
    (define/public (after-tick)
      (make-worldstate
       target
       sq-toy-speed
       (map
        (lambda (obj) (send obj after-tick))
        toys)))
    
    
    ; after-mouse-event : NonNegInt NonNegInt MouseEvent -> PlaygroundState<%>
    ; GIVEN : A location and mouse event
    ; RETURNS : The state of the world that should follow the given mouse event
    ;           at the given location
    ; DESIGN STRATEGY : Divide into cases on Mouse Event me
    (define/public (after-mouse-event mx my me)
      (cond
        [(mouse=? me BUTTON-DOWN)
         (world-after-button-down mx my)]
        [(mouse=? me DRAG)
         (world-after-drag mx my)]
        [(mouse=? me BUTTON-UP)
         (world-after-button-up mx my)]
        [else this]))
    
    
    ; after-key-event : KeyEvent -> PlaygroundState<%>
    ; GIVEN : A key event
    ; RETURNS : The state of the world that should follow the
    ;           given key event
    ; DESIGN STRATEGY : Divide into cases on Key Event ke
    (define/public (after-key-event ke)
      (cond
        [(key=? ke NEW-SQUARE)
         (world-after-sq-keyevent)]
        [(key=? ke NEW-THROBBER)
         (world-after-throb-clock-fb-keyevent make-throbber)]
        [(key=? ke NEW-CLOCK)
         (world-after-throb-clock-fb-keyevent make-clock)]
        [(key=? ke NEW-FOOTBALL)
         (world-after-throb-clock-fb-keyevent make-football)]
        [else this]))
    
    
    ; to-scene : -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene that depicts this World
    ; DESIGN STRATEGY : Use HOF foldr on toys
    (define/public (to-scene)
      (foldr
       (lambda (obj scene) (send obj add-to-scene scene))
       (send target add-to-scene EMPTY-CANVAS)
       toys))
    
    
    ; Local functions
    
    ; world-after-button-down : NonNegInt NonNegInt -> PlaygroundState<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the world after button down mouse event
    ; DESIGN STRATEGY : Call a more General function make-worldstate
    (define (world-after-button-down mx my)
      (make-worldstate
       (send target after-button-down mx my)
       sq-toy-speed
       (map
        (lambda (obj) (send obj after-button-down mx my))
        toys)))
    
    
    ; world-after-drag : NonNegInt NonNegInt -> PlaygroundState<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the world after drag mouse event
    ; DESIGN STRATEGY : Call a more General function make-worldstate
    (define (world-after-drag mx my)
      (make-worldstate
       (send target after-drag mx my)
       sq-toy-speed
       (map
        (lambda (obj) (send obj after-drag mx my))
        toys)))
    
    
    ; world-after-button-up : NonNegInt NonNegInt -> PlaygroundState<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the world after button up mouse event
    ; DESIGN STRATEGY : Call a more General function make-worldstate
    (define (world-after-button-up mx my)
      (make-worldstate
       (send target after-button-up mx my)
       sq-toy-speed
       (map
        (lambda (obj) (send obj after-button-up mx my))
        toys)))
    
    
    ; world-after-sq-keyevent : KeyEvent -> PlaygroundState<%>
    ; GIVEN : A key event
    ; RETURNS : The state of world after new square event
    ; DESIGN STRATEGY : Call a more General function make-worldstate
    (define (world-after-sq-keyevent)
      (make-worldstate
       target
       sq-toy-speed
       (cons (make-square-toy (send this target-x)
                              (send this target-y)
                              sq-toy-speed)
             toys)))
    
    
    ; world-after-throb-clock-fb-keyevent : (Int Int -> Toy<%>) KeyEvent
    ;                                        -> PlaygroundState<%>
    ; GIVEN : A function which takes 2 Ints as input an returns the object
    ;         of class which implements the Toy<%>, and key event
    ; RETURNS : The state of world after given key event
    ; DESIGN STRATEGY : Call a more General function make-worldstate
    (define (world-after-throb-clock-fb-keyevent obj-fn-name)
      (make-worldstate
       target
       sq-toy-speed
       (cons (obj-fn-name (send this target-x)
                          (send this target-y))
             toys)))
    ))



;; make-worldstate : Target PosInt ListOfToy<%> -> PlaygroundState<%>
;; GIVEN : An object whose class implements Target<%>, speed of square toy and
;;         list of objects whose class implements Toy<%>
;; RETURNS : An object of class which implements PlaygroundState<%>
(define (make-worldstate target sq-toy-speed objs)
  (new World%
       [target target]
       [sq-toy-speed sq-toy-speed]
       [toys objs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Target%
;; Interface : Target<%>
;; Target is and object of class Target%
;; A Target is selectable and draggable
;; A Target is (new Target% [x Int] [y Int]
;;                          [mx-dist Int] [my-dist Int]
;;                          [selected? Boolean])

(define Target%
  (class* object% (Target<%>)
    
    ; x and y coordinates of center of Target
    (init-field [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT])
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; Indicates whether the Target is selected or not
    ; Default value is false
    (init-field [selected? false])
    
    
    ; Private data for objects of this class
    
    ; The radius of Target
    (field [TARGET-RADIUS 10])
    
    ; Half of the radius of Target
    (field [HALF-TARGET-RADIUS (/ TARGET-RADIUS 2)])
    
    ; Image for displaying the Target
    (field [TARGET-IMG (circle TARGET-RADIUS "outline" "blue")])
    
    
    (super-new)
    
    
    ; Methods of Target<%>
    
    ; -> Int
    ; RETURNS : The x and y coordinates of the target
    (define/public (get-target-x) x)
    (define/public (get-target-y) y)
    
    
    ; -> Boolean
    ; RETURNS : True if the target is selected, else false
    (define/public (get-target-selected?) selected?)
    
    
    ; after-button-down : NonNegInt NonNegInt -> Target<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the mouse
    ;                   is inside the object or not
    (define/public (after-button-down mx my)
      (if (inside-target? x y mx my)
          (make-target x y
                       (- mx x)
                       (- my y)
                       true)
          this))
    
    
    ; after-drag : NonNegInt NonNegInt -> Target<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           drag mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (make-target (- mx mx-dist)
                       (- my my-dist)
                       mx-dist
                       my-dist
                       selected?)
          this))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Target<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button-up mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (make-target x y
                       0 0
                       false)
          this))
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image TARGET-IMG x y scene))

    
    ; Local functions

    ; inside-target? : Int Int NonNegInt NonNegInt -> Boolean
    ; GIVEN : x and y coordinate of target and mouse coordinates 
    ; RETURNS : True, iff the give mouse location is inside the target
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-target? x y mx my)
      (<= (sqrt (+ (sqr (- mx x))
                   (sqr (- my y))))
          TARGET-RADIUS))
    ))



;; make-target : Int Int Int Int Boolean -> Target<%>
;; GIVEN : The x and y coordinates of Target, the distance of x and y from
;;         mx and my respectively, and boolean
;; RETURNS : A new object of Target% class
(define (make-target x y mx-dist my-dist sel?)
  (new Target%
       [x x]
       [y y]
       [mx-dist mx-dist]
       [my-dist my-dist]
       [selected? sel?]))



;;=============================================================================
;; TEST CASES FOR TARGET
;;=============================================================================

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)
     (define TARGET-IMG (circle 10 "outline" "blue"))

     ; Target with default values
     (define TEST-TARGET (make-target x y 0 0 #f))

     ; TEST-TARGET after mouse events
     (define FINAL-TARGET
       (send
        (send
         (send
          (send
           (send
            (send TEST-TARGET after-button-down x y)
            after-drag (add1 x) (sub1 y))
           after-button-up (add1 x) (sub1 y))
          after-button-down 10 10)
         after-drag 5 5)
        after-button-up 5 5)))

    ; The values of x, y and selected? of FINAL-TARGET is checked
    ; The image of FINAL-TARGET is checked
    (check-true (and (equal? (send FINAL-TARGET get-target-x) (add1 x))
                     (equal? (send FINAL-TARGET get-target-y) (sub1 y))
                     (equal? (send FINAL-TARGET get-target-selected?) #f)
                     (equal? (send FINAL-TARGET add-to-scene EMPTY-CANVAS)
                             (place-image TARGET-IMG (add1 x) (sub1 y)
                                          EMPTY-CANVAS)))
                "a target with radius 10 should be displayed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Square%
;; Interface : Toy<%>
;; Sqaure is and object of class Square%
;; A Square is selectable and draggable
;; A Square is (new Square% [x Int] [y Int]
;;                          [mx-dist Int] [my-dist Int]
;;                          [velocity Int] [selected? Boolean])

(define Square%
  (class* object% (Toy<%>)
    
    ; x and y coordinates o center of Square
    (init-field x y)
    
    ; The velocity of the Square
    (init-field velocity)
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; Indicates whether the Square is selected or not
    (init-field [selected? false])
    
    
    ; Private data for objects of this class
    
    ; Width of Square
    (field [SQUARE-WIDTH 40])
    
    ; Half of the width of Square
    (field [HALF-SQUARE-WIDTH (/ 40 2)])
    
    ; Max x-coordinate for Square
    (field [MAX-X-FOR-SQ (- CANVAS-WIDTH HALF-SQUARE-WIDTH)])
    
    ; Min x-coordinate for Square
    (field [MIN-X-FOR-SQ HALF-SQUARE-WIDTH])
    
    ; Image for displaying Square
    (field [SQUARE-IMG (square SQUARE-WIDTH "outline" "red")])
    
    
    (super-new)
    
    
    ; Methods of Toy<%>
    
    ; toy-x : -> Int
    ; RETURNS : The x position of the center of Square
    (define/public (toy-x) x)
    
    
    ; toy-y : -> Int
    ; RETURNS : The y position of the center of Square
    (define/public (toy-y) y)
    
    
    ; toy-data : -> Int
    ; RETURNS : The velocity of Square
    (define/public (toy-data) velocity)
    
    
    
    ; Methods of Widget<%>
    
    ; after-tick : -> Toy<%>
    ; GIVEN : No arguments
    ; RETURNS : The state of this object at the next tick
    ; DESIGN STRATEGY : Divide into cases based on whether Square
    ;                   is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (new Square%
               [x (x-after-tick x velocity)]
               [y y]
               [mx-dist mx-dist]
               [my-dist my-dist]
               [velocity (velocity-after-tick x velocity)]
               [selected? selected?])))
    
    
    ; after-button-down : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the mouse
    ;                   is inside the object or not
    (define/public (after-button-down mx my)
      (if (inside-square? x y mx my)
          (new Square%
               [x x]
               [y y]
               [mx-dist (- mx x)]
               [my-dist (- my y)]
               [velocity velocity]
               [selected? true])
          this))
    
    
    ; after-drag : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           drag mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new Square%
               [x (- mx mx-dist)]
               [y (- my my-dist)]
               [mx-dist mx-dist]
               [my-dist my-dist]
               [velocity velocity]
               [selected? selected?])
          this))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button-up mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (new Square%
               [x x]
               [y y]
               [velocity velocity]
               [selected? false])
          this))
    
    
    ; after-key-event : KeyEvent -> Toy<%>
    ; GIVEN : A key event
    ; RETURNS : The state of this object that should follow the
    ;           given key event
    ; DETAILS : Square ignores all the key events
    (define/public (after-key-event ke) this)
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image SQUARE-IMG
                   x y
                   scene))
    
    
    ;; Local functions
    
    ; x-after-tick : Int Int -> Int
    ; GIVEN : x coordinate and velocity of Square
    ; RETURNS : x coordinate of the Square at the next tick
    ; DESIGN STRATEGY : Divide into cases based on the
    ;                   value of x+vx
    (define (x-after-tick x vel)
      (cond
        [(x-more-than-max-limit? x vel)
         MAX-X-FOR-SQ]
        [(x-less-than-min-limit? x vel)
         MIN-X-FOR-SQ]
        [else (+ x vel)]))
    
    
    ; velocity-after-tick : Int Int -> Int
    ; GIVEN : x coordinate and velocity of Square
    ; RETURNS : The velocity of Square at the next tick
    ; DESIGN STRATEGY : Divide into cases based on the
    ;                   value of x+vx
    (define (velocity-after-tick x vel)
      (if (or (x-more-than-max-limit? x vel)
              (x-less-than-min-limit? x vel))
          (- vel)
          vel))
    
    
    ; x-more-than-max-limit? : Int Int -> Boolean
    ; GIVEN : x coordinate and velocity of Square
    ; RETURNS : True, if x+vx is greater that max limit for x
    ; DESIGN STRATEGY : Combine simpler functions
    (define (x-more-than-max-limit? x vel)
      (> (+ x vel) MAX-X-FOR-SQ))
    
    
    ; x-less-than-min-limit? : Int Int -> Boolean
    ; GIVEN : x coordinate and velocity of Square
    ; RETURNS : True, if x+vx is less that min limit for x
    ; DESIGN STRATEGY : Combine simpler functions
    (define (x-less-than-min-limit? x vel)
      (< (+ x vel) MIN-X-FOR-SQ))


    ; inside-square? : Int Int NonNegInt NonNegInt -> Boolean
    ; GIVEN :  x and y coordinate of square and mouse coordinates
    ; RETURNS : True, iff the give mouse location is inside the square
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-square? x y mx my)
      (and (<= (- x HALF-SQUARE-WIDTH)
               mx
               (+ x HALF-SQUARE-WIDTH))
           (<= (- y HALF-SQUARE-WIDTH)
               my
               (+ y HALF-SQUARE-WIDTH))))
    ))



;; make-square-toy : Int Int PosInt -> Toy<%>
;; GIVEN : An x and a y position, and a speed
;; RETURNS : An object representing a square toy at the given position,
;;           travelling right at the given speed
(define (make-square-toy x y vel)
  (new Square% [x x] [y y] [velocity vel]))



;;==============================================================================
;; TEST CASES FOR SQUARE
;;==============================================================================

(begin-for-test
  (local
    ((define MAX-X-FOR-SQ (- CANVAS-WIDTH 20))
     (define MIN-X-FOR-SQ 20)
     (define SQUARE-IMG (square 40 "outline" "red"))
     
     ; Square has reached the max value for x
     (define SQUARE-AT-RIGHT-BORDER
       (new Square%
            [x MAX-X-FOR-SQ]
            [y 100]
            [velocity 20]
            [selected? false]))

     ; Square has reached the MIN value for x
     (define SQUARE-AT-LEFT-BORDER
       (new Square%
            [x 2]
            [y 100]
            [velocity -20]
            [selected? false]))
     
     ; Selected Square
     (define SELECTED-SQUARE
       (new Square%
            [x 250]
            [y 250]
            [velocity 10]
            [selected? true]))
     
     ; Unselected Square
     (define UNSELECTED-SQUARE
       (new Square%
            [x 470]
            [y 250]
            [velocity 10]
            [selected? false])))
    
    ; Selected Square is tested for mouse, key and tick events
    (check-equal?
     (send
      (send
       (send
        (send
         (send
          (send
           (send SELECTED-SQUARE after-tick)
           after-button-down 250 250)
          after-drag 300 300)
         after-button-up 300 300)
        after-key-event "s")
       after-tick)
      toy-x)
     310
     "test for after-tick")
    
    ; Velocity of selected Square is checked
    (check-equal?
     (send SELECTED-SQUARE toy-data)
     10
     "velocity should be 10")
    
    ; The y-coordinate of Square after a tick is checked
    (check-equal?
     (send (send (make-square-toy 20 20 30) after-tick) toy-y)
     20
     "test for boundary conditions")
    
    ; The Square at right border is checked for its x value after tick
    (check-equal?
     (send
      (send SQUARE-AT-RIGHT-BORDER after-tick) toy-x)
     480
     "test for after-tick")

    ; The Square at left border is checked for its x value after tick
    (check-equal?
     (send
      (send SQUARE-AT-LEFT-BORDER after-tick) toy-x)
     20
     "test for after-tick")
    
    ; Unselected Square is slected, and its x value after mouse
    ; events is checked
    (check-equal?
     (send
      (send
       (send
        (send
         (send UNSELECTED-SQUARE after-tick)
         after-button-down 10 10)
        after-drag 20 20)
       after-button-up 20 20)
      toy-x)
     480
     "test for tick on unselected square")
    
    ; The image of unselected Square is placed on canvas
    (check-equal?
     (send UNSELECTED-SQUARE add-to-scene EMPTY-CANVAS)
     (place-image SQUARE-IMG 470 250 EMPTY-CANVAS)
     "test for add-to-scene")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Throbber%
;; Interface : Toy<%>
;; Throbber is and object of class Throbber%
;; A Throbber is selectable and draggable
;; A Throbber is (new Throbber% [x Int] [y Int]
;;                              [rad PosInt] [inc-rad? Boolean]
;;                              [mx-dist Int] [my-dist Int]
;;                              [selected? Boolean])

(define Throbber%
  (class* object% (Toy<%>)
    
    ; x and y coordinate of center of Throbber
    (init-field x y)
    
    ; radius of Throbber
    ; Default value is 5
    (init-field [rad 5])
    
    ; True if the radius of Throbber has to be
    ; incremented by 1, else false
    ; Default value is true
    (init-field [inc-rad? true])
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; True, if the Throbber is selected, else false
    ; Default value is false
    (init-field [selected? false])
    
    
    ; Private data for objects of this class
    
    ; Max radius of Throbber
    (field [MAX-RAD 20])
    
    ; Min radius of Throbber
    (field [MIN-RAD 5])
    
    ; The mode and color of Throbber, used during displaying
    ; Throbber
    (field [THROBBER-MODE "solid"] [THROBBER-COLOR "green"])
    
    
    (super-new)
    
    
    ; Methods of Toy<%>
    
    ; toy-x : -> Int
    ; RETURNS : The x position of the center of Throbber
    (define/public (toy-x) x)
    
    
    ; toy-y : -> Int
    ; RETURNS : The y position of the center of Throbber
    (define/public (toy-y) y)
    
    
    ; toy-data : -> PosInt
    ; RETURNS : The radius of Throbber
    (define/public (toy-data) rad)
    
    
    ; Methods of Widget<%>
    
    ; after-tick : -> Toy<%>
    ; GIVEN : No arguments
    ; RETURNS : The state of this object at the next tick
    ; DESIGN STRATEGY : Divide into cases based on whether Throbber
    ;                   is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (new Throbber%
               [x x]
               [y y]
               [inc-rad? (inc-rad-after-tick?)]
               [rad (rad-after-tick)]
               [mx-dist mx-dist]
               [my-dist my-dist]
               [selected? selected?])))
    
    
    ; after-button-down : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the mouse
    ;                   is inside the object or not
    (define/public (after-button-down mx my)
      (if (inside-throbber? x y mx my)
          (new Throbber%
               [x x]
               [y y]
               [inc-rad? inc-rad?]
               [rad rad]
               [mx-dist (- mx x)]
               [my-dist (- my y)]
               [selected? true])
          this))
    
    
    ; after-drag : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           drag mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%
               [x (- mx mx-dist)]
               [y (- my my-dist)]
               [inc-rad? inc-rad?]
               [rad rad]
               [mx-dist mx-dist]
               [my-dist my-dist]
               [selected? selected?])
          this))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button-up mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (new Throbber%
               [x x]
               [y y]
               [inc-rad? inc-rad?]
               [rad rad]
               [selected? false])
          this))
    
    
    ; after-key-event : KeyEvent -> Toy<%>
    ; GIVEN : A key event
    ; RETURNS : The state of this object that should follow the
    ;           given key event
    ; DETAILS : Throbber ignores all the key events
    (define/public (after-key-event ke) this)
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image (circle rad THROBBER-MODE THROBBER-COLOR)
                   x y
                   scene))
    
    
    ;; Local functions
    
    ; inc-rad-after-tick? : -> Boolean
    ; GIVEN : No arguments
    ; RETURNS : True, if at next tick the value of radius of Throbber
    ;           is bewtween max and min value of radius for Throbber,
    ;           else false
    ; DESIGN STRATEGY : Divide into cases based on whether the value of
    ;                   rad at next tick is between MAX-RAD and MIN-RAD
    (define (inc-rad-after-tick?)
      (if (and (< (rad-after-tick) MAX-RAD)
               (< MIN-RAD (rad-after-tick)))
          inc-rad?
          (not inc-rad?)))
    
    
    ; rad-after-tick : -> PosInt
    ; GIVEN : No arguments
    ; RETURNS : The value of radius at next tick
    ; DESIGN STRATEGY : Divide into cases based on the value of inc-rad?
    (define (rad-after-tick)
      (if inc-rad?
          (add1 rad)
          (sub1 rad)))


    ; inside-throbber? : Int Int NonNegInt NonNegInt -> Boolean
    ; GIVEN : x and y coordinate of throbber and mouse coordinates 
    ; RETURNS : True, iff the give mouse location is inside the throbber
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-throbber? x y mx my)
      (<= (sqrt (+ (sqr (- mx x))
                   (sqr (- my y))))
          rad))
    ))



;; make-throbber : Int Int -> Toy<%>
;; GIVEN : An x and a y position
;; RETURNS : An object representing a throbber at the given position
(define (make-throbber x y)
  (new Throbber% [x x] [y y]))



;;=============================================================================
;; TEST CASES FOR THROBBER
;;=============================================================================

(begin-for-test
  (local
    (; A Throbber with radius 18 and inc-rad? true
     (define THROB1
       (new Throbber% [x 40] [y 40] [rad 18] [inc-rad? true]))
     
     ; throb1 after 3 ticks
     (define THROB2
       (send (send (send THROB1 after-tick) after-tick) after-tick)))
    
    ; throb1 after 3 ticks should have radius 19
    (check-equal?
     (send THROB2 toy-data)
     19
     "the radius of throb1 after 3 ticks shouls be 19")))

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)
     
     ; Throbber with center at center of canvas
     (define TEST-THROBBER (make-throbber x y))
     
     ; TEST-THROBBER after mouse events and 1 tick
     (define FINAL-THROBBER
       (send
        (send
         (send
          (send
           (send TEST-THROBBER after-button-down x y)
           after-drag (add1 x) (sub1 y))
          after-tick)
         after-button-up (add1 x) (sub1 y))
        after-tick)))
    
    ; Checks the x, y and radius of FINAL-THROBBER
    ; Also checks the image of Throbber after placing it on canvas
    (check-true (and (equal? (send FINAL-THROBBER toy-x) (add1 x))
                     (equal? (send FINAL-THROBBER toy-y) (sub1 y))
                     (equal? (send FINAL-THROBBER toy-data) 6)
                     (equal? (send FINAL-THROBBER add-to-scene EMPTY-CANVAS)
                             (place-image (circle 6 "solid" "green")
                                          (add1 x) (sub1 y)
                                          EMPTY-CANVAS)))
                "a throbber with radius 6 should be displayed on canvas")))

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)
     
     ; Throbber with center at center of canvas
     (define TEST-THROBBER (make-throbber x y))
     
     ; TEST-THROBBER after mouse, key and 1 tick events
     (define FINAL-THROBBER
       (send
        (send
         (send
          (send
           (send
            (send TEST-THROBBER after-button-down 10 10)
            after-drag 20 20)
           after-tick)
          after-button-up 20 20)
         after-tick)
        after-key-event "s")))
    
    ; Checks the x, y and radius of FINAL-THROBBER 
    (check-true (and (equal? (send FINAL-THROBBER toy-x) x)
                     (equal? (send FINAL-THROBBER toy-y) y)
                     (equal? (send FINAL-THROBBER toy-data) 7))
                "a throbber with radius 7 should be displayed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Clock%
;; Interface : Toy<%>
;; Clock is and object of class Clock%
;; A Clock is selectable and draggable
;; A Clock is (new Clock% [x Int] [y Int]
;;                        [mx-dist Int] [my-dist Int]
;;                        [ticks NonNegInt] [selected? Boolean])

(define Clock%
  (class* object% (Toy<%>)
    
    ; x and y coordinate of center of Clock
    (init-field x y)
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; The number of ticks since Clock was created
    ; Default value is 0
    (init-field [ticks 0])
    
    ; True if Clock is selected, else false
    ; Default value is false
    (init-field [selected? false])
    
    
    ; Private data for objects of this class
    
    ; Radius of Clock
    (field [CLOCK-RAD 40])
    
    ; The mode and color of Clock, used during displaying
    ; Clock
    (field [CLOCK-MODE "outline"] [CLOCK-COLOR "black"])
    
    ; Image for displayingg Clock
    (field [CLOCK (circle CLOCK-RAD CLOCK-MODE CLOCK-COLOR)])
    
    ; The color and size of text, used during displaying
    ; the ticks of Clock
    (field [TEXT-COLOR "magenta"] [TEXT-SIZE 20])
    
    
    (super-new)
    
    
    ; Methods of Toy<%>
    
    ; toy-x : -> Int
    ; RETURNS : The x position of the center of Clock
    (define/public (toy-x) x)
    
    
    ; toy-y : -> Int
    ; RETURNS : The y position of the center of Clock
    (define/public (toy-y) y)
    
    
    ; toy-data : -> NonNegInt
    ; RETURNS : The ticks of Clock
    (define/public (toy-data) ticks)
    
    
    ; Methods of Widget<%>
    
    ; after-tick : -> Toy<%>
    ; GIVEN : No arguments
    ; RETURNS : The state of this object at the next tick
    (define/public (after-tick)
      (new Clock%
           [x x]
           [y y]
           [mx-dist mx-dist]
           [my-dist my-dist]
           [ticks (add1 ticks)]
           [selected? selected?]))
    
    
    ; after-button-down : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the mouse
    ;                   is inside the object or not
    (define/public (after-button-down mx my)
      (if (inside-clock? x y mx my)
          (new Clock%
               [x x]
               [y y]
               [mx-dist (- mx x)]
               [my-dist (- my y)]
               [ticks ticks]
               [selected? true])
          this))
    
    
    ; after-drag : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           drag mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%
               [x (- mx mx-dist)]
               [y (- my my-dist)]
               [mx-dist mx-dist]
               [my-dist my-dist]
               [ticks ticks]
               [selected? selected?])
          this))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button-up mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (new Clock%
               [x x]
               [y y]
               [ticks ticks]
               [selected? false])
          this))
    
    
    ; after-key-event : KeyEvent -> Toy<%>
    ; GIVEN : A key event
    ; RETURNS : The state of this object that should follow the
    ;           given key event
    ; DETAILS : Clock ignores all the key events
    (define/public (after-key-event ke) this)
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image
       (overlay/align "middle" "middle"
                      (text (number->string ticks)
                            TEXT-SIZE
                            TEXT-COLOR)
                      CLOCK)
       x y
       scene))

    
    ; Local functions

    ; inside-clock? : Int Int NonNegInt NonNegInt -> Boolean
    ; GIVEN : x and y coordinate of clock and mouse coordinates 
    ; RETURNS : True, iff the give mouse location is inside the clock
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-clock? x y mx my)
      (<= (sqrt (+ (sqr (- mx x))
                   (sqr (- my y))))
          CLOCK-RAD))
    ))



;; make-clock : Int Int -> Toy<%>
;; GIVEN : An x and a y position
;; RETURNS : An object representing a Clock at the given position
(define (make-clock x y)
  (new Clock% [x x] [y y]))



;;=============================================================================
;; TEST CASES FOR CLOCK
;;=============================================================================
(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)

     ; Clock with center at center of canvas
     (define TEST-CLOCK (make-clock x y))

     ; TEST-CLOCK after button down, drag, tick, button up and tick events
     (define FINAL-CLOCK
       (send
        (send
         (send
          (send
           (send TEST-CLOCK after-button-down x  y)
           after-drag (add1 x) (sub1 y))
          after-tick)
         after-button-up (add1 x) (sub1 y))
        after-tick)))

    ; Checks the x, y and ticks of FINAL-CLOCK
    (check-true (and (equal? (send FINAL-CLOCK toy-x) (add1 x))
                     (equal? (send FINAL-CLOCK toy-y) (sub1 y))
                     (equal? (send FINAL-CLOCK toy-data) 2))
                "This clock should display 2")))

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)

     ; Clock with center at center of canvas
     (define TEST-CLOCK (make-clock x y))

     ; TEST-CLOCK after button down, drag, 2 ticks, button up, 1 tick
     ; key event and 1 tick
     (define FINAL-CLOCK
       (send
        (send
         (send
          (send
           (send
            (send
             (send
              (send TEST-CLOCK after-button-down 10 10)
              after-drag 20 20)
             after-tick)
            after-tick)
           after-button-up 20 20)
          after-tick)
         after-key-event "f")
        after-tick)))

    ; Checks the x, y and ticks of FINAL-CLOCK
    (check-true (and (equal? (send FINAL-CLOCK toy-x) x)
                     (equal? (send FINAL-CLOCK toy-y) y)
                     (equal? (send FINAL-CLOCK toy-data) 4))
                "This clock should display 3")))

(begin-for-test
  (local
    ((define x HALF-CANVAS-WIDTH)
     (define y HALF-CANVAS-HEIGHT)

     ; Clock with center at center of canvas
     (define TEST-CLOCK (make-clock x y))

     ; Image of Clock
     (define CLOCK (circle 40 "outline" "black")))

    ; Checks the image of CLOCK after placing on canvas
    (check-equal?
     (send TEST-CLOCK add-to-scene EMPTY-CANVAS)
     (place-image
      (overlay/align "middle" "middle"
                     (text (number->string 0)
                           20
                           "magenta")
                     CLOCK)
      x y
      EMPTY-CANVAS))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLASS Football%
;; Interface : Toy<%>
;; Football is and object of class Football%
;; A Football is selectable and draggable
;; A Football is (new Football% [x Int] [y Int] [size PosReal]
;;                              [mx-dist Int] [my-dist Int]
;;                              [selected? Boolean])

(define Football%
  (class* object% (Toy<%>)
    
    ; x and y coordinate of center of Football
    (init-field x y)
    
    ; The current size of football, which is the
    ; current scale factor
    ; Default value is 1
    (init-field [size 1])
    
    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])
    
    ; True if Football is selected, else false
    ; Default value is false
    (init-field [selected? false])
    
    ; Private data for objects of this class
    
    ; The scaling factor
    (field [SCALE-FACTOR 1.1])

    ; Scale value for x
    (field [SCALE-FOR-X 1])

    ; Dividing Factor
    (field [DIV-FAC 2])
    
    ; Image of Tootball for displaying
    (field [FOOTBALL-IMG (bitmap "football.png")])
    
    (super-new)
    
    ; Methods of Toy<%>
    
    ; toy-x : -> Int
    ; RETURNS : The x position of the center of Football
    (define/public (toy-x) x)
    
    ; toy-y : -> Int
    ; RETURNS : The y position of the center of Football
    (define/public (toy-y) y)
    
    ; toy-data : -> PosReal
    ; RETURNS : The size of Football
    (define/public (toy-data) size)
    
    ; Methods of Widget<%>
    
    ; after-tick : -> Toy<%>
    ; GIVEN : No arguments
    ; RETURNS : The state of this object at the next tick
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (new Football%
               [x x]
               [y y]
               [size (/ size SCALE-FACTOR)]
               [mx-dist mx-dist]
               [my-dist my-dist]
               [selected? selected?])))
    
    
    ; after-button-down : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button-down mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-button-down mx my)
      (if (inside-football? x y mx my
                            (half-football-width)
                            (half-football-height))
          (new Football%
               [x x]
               [y y]
               [size size]
               [mx-dist (- mx x)]
               [my-dist (- my y)]
               [selected? true])
          this))
    
    
    ; after-drag : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           drag mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new Football%
               [x (- mx mx-dist)]
               [y (- my my-dist)]
               [size size]
               [mx-dist mx-dist]
               [my-dist my-dist]
               [selected? selected?])
          this))
    
    
    ; after-button-up : NonNegInt NonNegInt -> Toy<%>
    ; GIVEN : A mouse location
    ; RETURNS : The state of the object that should follow the
    ;           button-up mouse event at the given location
    ; DESIGN STRATEGY : Divide into cases based on whether the object
    ;                   is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (new Football%
               [x x]
               [y y]
               [size size]
               [selected? false])
          this))
    
    
    ; after-key-event : KeyEvent -> Toy<%>
    ; GIVEN : A key event
    ; RETURNS : The state of this object that should follow the
    ;           given key event
    ; DETAILS : Football ignores all the key events
    (define/public (after-key-event ke) this)
    
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this object
    ;           painted on it
    ; DESIGN STRATEGY : Divide into cases based on the image-height
    ;                   of the image of Football after scaling it
    (define/public (add-to-scene scene)
      (if (<= (image-height (scaled-football-image)) 0)
          scene
          (place-image (scaled-football-image)
                       x y
                       scene)))
    
    ; Local functions
    
    ; scaled-football-image : -> Image
    ; GIVEN : No arguments
    ; RETURNS : A scaled image of the Football
    ; DESIGN STRATEGY : Combine simpler functions
    (define (scaled-football-image)
      (scale/xy SCALE-FOR-X size FOOTBALL-IMG))
    
    
    ; half-football-width : -> PosReal
    ; GIVEN : No arguments
    ; RETURNS : The half width of Football
    ; DESIGN STRATEGY : Combine simpler functions
    (define (half-football-width)
      (/ (image-width (scaled-football-image)) DIV-FAC))
    
    
    ; half-football-height : -> PosReal
    ; GIVEN : No arguments
    ; RETURNS : The half height of Football
    ; DESIGN STRATEGY : Combine simpler functions
    (define (half-football-height)
      (/ (image-height (scaled-football-image)) DIV-FAC))


    ; inside-football? : Int Int NonNegInt NonNegInt PosReal PosReal -> Boolean
    ; GIVEN :  x and y coordinate of football, mouse coordinates and
    ;          width and height of football
    ; RETURNS : True, iff the give mouse location is inside the football
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-football? x y mx my width height)
      (and (<= (- x width)
               mx
               (+ x width))
           (<= (- y height)
               my
               (+ y height))))

    ))



;; make-football : Int Int -> Toy<%>
;; GIVEN : An x and a y position
;; RETURNS : An object representing a football at the given position
(define (make-football x y)
  (new Football% [x x] [y y]))



;;=============================================================================
;; TEST CASES FOR FOOTBALL
;;=============================================================================

(begin-for-test
  (local
    (; Selected Football
     (define SELECTED-FOOTBALL
       (new Football% [x 100][y 100][size 1][selected? true]))

     ; Unselected Football
     (define UNSELECTED-FOOTBALL
       (new Football% [x 100][y 100][size 1][selected? false]))

     ; Unselected Football at (200, 200)
     (define FOOTBALL-AT-200
       (new Football% [x 200] [y 200]))

     ; Unselected scaled Football, with center at (100, 100)
     (define SMALL-FOOTBALL
       (new Football% [x 100] [y 100] [size 0.001] [selected? false]))

     ; Image of Football
     (define FOOTBALL-IMG (bitmap "football.png")))

    ; Checks the x coordinate of Football
    (check-equal?
     (send  (make-football 200 200) toy-x)
     200
     "checking for toy-x")

    ; Checks the size of Football after 3 ticks
    (check-equal?
     (send
      (send
       (send
        (send UNSELECTED-FOOTBALL after-tick)
        after-tick) after-tick) toy-data)
     .7513148009015777
     "checking for after-tick")

    ; Checks the size of Football after tick, button down, drag,
    ; button up and tick events
    (check-equal?
     (send
      (send
       (send
        (send
         (send
          (send SELECTED-FOOTBALL after-tick)
          after-button-down 100 100)
         after-drag 150 150)
        after-button-up 150 150)
       after-tick)
      toy-data)
     0.9090909090909091
     "the football should be of same size at 250 250")

    ; Checks the y coordinate of unselected Football after
    ; mouse and key events
    (check-equal?
     (send
      (send
       (send
        (send
         (send UNSELECTED-FOOTBALL after-button-down 10 10)
         after-drag 15 15)
        after-button-up 15 15)
       after-key-event "s")
      toy-y)
     100
     "checking for mouse-events")

    ; Checks the image of unselected Football after placing
    ; it on canvas
    (check-equal?
     (send UNSELECTED-FOOTBALL add-to-scene EMPTY-CANVAS)
     (place-image FOOTBALL-IMG 100 100 EMPTY-CANVAS)
     "test for add-to-scene")

    ; Checks the image of scaled Football after placing
    ; it on canvas
    (check-equal?
     (send
      (send
       (send
        (send SMALL-FOOTBALL after-tick) after-tick)
                 after-tick) add-to-scene EMPTY-CANVAS)
     EMPTY-CANVAS
     "test for add-to-scene")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;=============================================================================
;; TEST CASES FOR WORLD
;;=============================================================================

; Initial World with speed of square toy as 10
(define TEST-WORLD (make-world 10))

; TEST-WORLD after button down event on Target
(define TEST-WORLD-TARGET-SELECTED
  (send TEST-WORLD after-mouse-event
        HALF-CANVAS-WIDTH
        HALF-CANVAS-HEIGHT
        BUTTON-DOWN)) 

; TEST-WORLD after button up on Target
(define TEST-WORLD-TARGET-UNSELECTED
  (send TEST-WORLD after-mouse-event
        HALF-CANVAS-WIDTH
        HALF-CANVAS-HEIGHT
        BUTTON-UP))

; TEST-WORLD-TARGET-SELECTED after dragging the Target to (250, 250)
(define TEST-WORLD-TARGET-DRAGGED
  (send TEST-WORLD-TARGET-SELECTED
        after-mouse-event
        250
        250
        DRAG))

; Dragging unselected Target 
(define TEST-WORLD-TARGET-NOT-DRAGGED
  (send TEST-WORLD-TARGET-UNSELECTED
        after-mouse-event
        250
        250
        DRAG))

; Button down event on Target with mouse outside the Target
(define TEST-WORLD-TARGET-REMAINS-UNSELECTED
  (send TEST-WORLD-TARGET-UNSELECTED
        after-mouse-event
        300
        300
        BUTTON-DOWN))

; Initial world after add new square key event
(define TEST-WORLD-SQUARE
  (send TEST-WORLD-TARGET-UNSELECTED
        after-key-event
        NEW-SQUARE))

; Initial world after add new throbber key event
(define TEST-WORLD-THROBBER
  (send TEST-WORLD-TARGET-UNSELECTED
        after-key-event
        NEW-THROBBER))

; Initial world after add new clock key event
(define TEST-WORLD-CLOCK
  (send TEST-WORLD-TARGET-UNSELECTED
        after-key-event
        NEW-CLOCK))

; Initial world after add new football key event
(define TEST-WORLD-FOOTBALL
  (send TEST-WORLD-TARGET-UNSELECTED
        after-key-event
        NEW-FOOTBALL))

; Testing invalid key event
(define TEST-WORLD-TARGET-INVALID-KEYEVENT
  (send TEST-WORLD-TARGET-UNSELECTED
        after-key-event
        "r"))

; Initial world after 1 tick
(define TEST-WORLD-AFTER-TICK (send TEST-WORLD after-tick))

; World with square after 1 tick
(define TEST-WORLD-AFTER-TICK-WITH-SQUARE
  (send TEST-WORLD-SQUARE after-tick))

; World with throbber after 1 tick
(define TEST-WORLD-AFTER-TICK-WITH-THROBBER
  (send TEST-WORLD-THROBBER after-tick))

; World with football after 1 tick
(define TEST-WORLD-AFTER-TICK-WITH-FOOTBALL
  (send TEST-WORLD-FOOTBALL after-tick))

; World with clock after 1 tick
(define TEST-WORLD-AFTER-TICK-WITH-CLOCK
  (send TEST-WORLD-CLOCK after-tick))

; World after button down on square
(define TEST-WORLD-AFTER-MOUSE-DOWN-WITH-TOY
  (send TEST-WORLD-SQUARE
        after-mouse-event
        250 250
        BUTTON-DOWN ))

; World after button up on selected square
(define TEST-WORLD-AFTER-MOUSE-UP-WITH-TOY
  (send TEST-WORLD-SQUARE
        after-mouse-event
        250 250
        BUTTON-UP))

; World after drag event on selected square
(define TEST-WORLD-SQUARE-DRAG
  (send TEST-WORLD-SQUARE
        after-mouse-event
        250
        250
        DRAG))

; New object of Target
(define TARGET (new Target%))

; Initial playground with speed of square toy as 10 and clock
(define PLAYGROUND
  (new World%
       [target TARGET]
       [sq-toy-speed 10]
       [toys (send TEST-WORLD-CLOCK get-toys)]))

; Image of Clock with 0 ticks
(define CLOCKS
  (overlay/align "middle" "middle"
                 (circle 40 "outline" "black")
                 (text (number->string 0) 20 "magenta")))

(begin-for-test
  ; Testing image of Target after placing it on empty canvas
  (check-equal?
   (send TARGET add-to-scene EMPTY-CANVAS)
   (place-image (circle 10 "outline" "blue")
                HALF-CANVAS-WIDTH
                HALF-CANVAS-HEIGHT
                EMPTY-CANVAS))

  ; Testing image of PLAYGROUND 
  (check-equal?
   (send PLAYGROUND to-scene)
   (place-image
    CLOCKS HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT
    (place-image
     (circle 10 "outline" "blue")
     HALF-CANVAS-WIDTH
     HALF-CANVAS-HEIGHT
     EMPTY-CANVAS)))

  ; Testing the selected? value of selected Target
  (check-equal?
   (send TEST-WORLD-TARGET-SELECTED target-selected?)
   true
   "target is selected")

  ; Testing the x coordinate of Traget after dragging it
  ; to (250, 250)
  (check-equal?
   (send TEST-WORLD-TARGET-DRAGGED target-x)
   250 
   "target is moved")

  ; Testing the y coordinate of Traget after dragging it
  ; to (250, 250)
  (check-equal?
   (send TEST-WORLD-TARGET-DRAGGED target-y)
   250 
   "target is moved")

  ; Testing the value of selected? of unselected Target
  (check-equal?
   (send TEST-WORLD-TARGET-UNSELECTED target-selected?)
   false 
   "target is unselected")

  ; Testing the x coordinate of unselected Target placed
  ; at center of canvas
  (check-equal?
   (send TEST-WORLD-TARGET-REMAINS-UNSELECTED target-x)
   HALF-CANVAS-WIDTH 
   "x coordinate of unselected target is equal to HALF-CANVAS-WIDTH")

  ; Testing the x coordinate of Target which is unselected
  ; and dragged, originally placed at center of canvas
  (check-equal?
   (send TEST-WORLD-TARGET-NOT-DRAGGED target-x)
   HALF-CANVAS-WIDTH 
   "x coordinate of unselected dragged target is equal to HALF-CANVAS-WIDTH")

  ; Testing x coordinate of square
  (check-equal?
   (send TEST-WORLD-SQUARE target-x)
   HALF-CANVAS-WIDTH 
   "x coordinate of Square is same as that of Target")

  ; Testing x coordinate of Target after 1 tick
  (check-equal?
   (send TEST-WORLD-AFTER-TICK target-x)
   HALF-CANVAS-WIDTH 
   "Since target is not affected by tick events, its x and y coordinate
    remains unchanged")

  ; Testing the x coordinate of Target with world having a square
  ; after 1 tick
  (check-equal?
   (send TEST-WORLD-AFTER-TICK-WITH-SQUARE target-x)
   HALF-CANVAS-WIDTH 
   "Even after 1 tick, the x coordinate of square is changed, but that
    of target remains unchanged")

  ; Testing x coordinate of Target, after 1 tick with world containing
  ; throbber
  (check-equal?
   (send TEST-WORLD-AFTER-TICK-WITH-THROBBER target-x)
   HALF-CANVAS-WIDTH 
   "After 1 tick the radius of throbber increases, but x coordinate of
    both target and throbber remains same")

  ; Testing x coordinate of Target, after 1 tick with world containing
  ; clock
  (check-equal?
   (send TEST-WORLD-AFTER-TICK-WITH-CLOCK target-x)
   HALF-CANVAS-WIDTH 
   "After 1 tick the ticks of clock increments by 1, but x coordinate of
    both target and clock remains same")

  ; Testing x coordinate of Target, after 1 tick with world containing
  ; football
  (check-equal?
   (send TEST-WORLD-AFTER-TICK-WITH-FOOTBALL target-x)
   HALF-CANVAS-WIDTH 
   "After 1 tick the size of football decreases, but x coordinate of
    both target and football remains same")

  ; Testing the x coordinate of Target after mouse down event on
  ; square
  (check-equal?
   (send TEST-WORLD-AFTER-MOUSE-DOWN-WITH-TOY target-x)
   HALF-CANVAS-WIDTH 
   "The x coordinate after button down event square remains unchanged")

  ; Testing the x coordinate of Target after button up event on
  ; square
  (check-equal?
   (send TEST-WORLD-AFTER-MOUSE-UP-WITH-TOY target-x)
   HALF-CANVAS-WIDTH 
   "The x coordinate after button up event square remains unchanged")

  ; Testing the x coordinate of Target after dragging the square
  (check-equal?
   (send TEST-WORLD-SQUARE-DRAG target-y)
   300 
   "The x coordinate of Target remains unchanged after dragging the square")

  ; Testing the world after enter mouse event
  (check-equal?
   (send TEST-WORLD-TARGET-UNSELECTED after-mouse-event 0 0 "enter")
   TEST-WORLD-TARGET-UNSELECTED
   "The world remains unchanged after enter mouse event since its not allowed
    in system"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
