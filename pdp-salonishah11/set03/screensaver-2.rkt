;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; screensaver-2.rkt
;; Screensaver-2 is a program with two rectangles moving around the canvas.
;; The rectangles bounce smoothly on touching the sides of canvas. The rectangles
;; have a fixed velocity, and their directions change on touching the sides of canvas.
;; When a rectangle passes the corner, it bounces back with x- and y- velocities
;; reversed.
;; Also, the user is able to select the rectangle and drag it around the canvas. The
;; location of mouse is indicated by a red circle.



(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "03" "screensaver-2.rkt")

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-rect1 
 world-rect2 
 world-paused?
 rect-x 
 rect-y 
 rect-vx
 rect-vy
 rect-selected?
 world-after-mouse-event
 rect-after-mouse-event
 new-rectangle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS :

;; Dimensions of canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; Dimensions of rectangle
(define RECT-WIDTH 60)
(define RECT-HEIGHT 50)
(define HALF-RECT-WIDTH (/ RECT-WIDTH 2))
(define HALF-RECT-HEIGHT (/ RECT-HEIGHT 2))
(define BLUE-RECT (rectangle RECT-WIDTH RECT-HEIGHT "outline" "blue"))
(define RED-RECT (rectangle RECT-WIDTH RECT-HEIGHT "outline" "red"))

;; Circle
(define CIRCLE-RADIUS 5)
(define RED-CIRCLE(circle CIRCLE-RADIUS "outline" "red"))

;; Initial rectangle coordinates and velocities
(define INITIAL-RECT1-X 200)
(define INITIAL-RECT1-Y 100)
(define INITIAL-RECT2-X 200)
(define INITIAL-RECT2-Y 200)
(define INITIAL-RECT1-VX -12)
(define INITIAL-RECT1-VY 20)
(define INITIAL-RECT2-VX 23)
(define INITIAL-RECT2-VY -14)

;; Constants used for painting velocities on rectangle
(define OPEN-BRACKET "(")
(define CLOSE-BRACKET ")")
(define COMMA ",")
(define TEXT-HEIGHT 12)
(define TEXT-COLOR-BLUE "blue")
(define TEXT-COLOR-RED "red")

;; Limits for rectangle to stay inside the canvas
(define LEFT-WALL-LIMIT (+ 0 1))
(define RIGHT-WALL-LIMIT (- 400 1))
(define TOP-WALL-LIMIT (+ 0 1))
(define BOTTOM-WALL-LIMIT (- 300 1))
(define MAX-X-FOR-RECT (- CANVAS-WIDTH HALF-RECT-WIDTH 1))
(define MIN-X-FOR-RECT (+ HALF-RECT-WIDTH 1))
(define MAX-Y-FOR-RECT (- CANVAS-HEIGHT HALF-RECT-HEIGHT 1))
(define MIN-Y-FOR-RECT (+ HALF-RECT-HEIGHT 1))

;; KeyEvents
(define PAUSE-KEY-EVENT " ")
(define NONPAUSE-KEY-EVENT "a")

;; Mouse events
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")
(define OTHER-EVENT "enter")

;; Booleans
(define TRUE #true)
(define FALSE #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; Structure Rect :
(define-struct rect (x y vx vy mx-dist my-dist selected?))

;; CONSTRUCTOR TEMPLATE :
;; A rect is (make-rect NonNegInt NonNegInt Int Int Int Int Boolean)

;; INTERPRETATION :
;; (make-rect NonNegInt NonNegInt Integer Integer Int Int Boolean)
;; is a rect where
;; x, y : are the coordinates of the center of rectangle
;; vx, vy : are the velocities of rectangle in x and y direction respectively
;; mx-dist : is the distance from mx(mouse x-coordinate) to x at button-down event
;; my-dist : is the distance from my(mouse y-coordinate) to y at button-down event
;; selected? is a Boolean indicating whether the rectangle is selected or not

;; DESTRUCTOR TEMPLATE :
;; rect-fn : Rect -> ??
#;(define (rect-fn rt)
    (...
     (rect-x rt)
     (rect-y rt)
     (rect-vx rt)
     (rect-vy rt)
     (rect-mx-dist rt)
     (rect-my-dist rt)
     (rect-selected? rt)
     ...))



;; Structure World :
(define-struct world (rect1 rect2 paused?))

;; CONSTRUCTOR TEMPLATE :
;; A world is (make-world Rect Rect Boolean)

;; INTERPRETATION :
;; (make-world Rect Rect Boolean) is a world where
;; rect1 and rect2 are rectangles of structure Rect
;; paused? is a boolean which represents whether the simulation is paused
;;         or not

;; DESTRUCTOR TEMPLATE :
;; world-fn : World -> ??
#;(define (world-fn ws)
    (...
     (world-rect1 ws)
     (world-rect2 ws)
     (world-paused? ws)
     ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples

;; Unselected Rectangles
(define RECT1 (make-rect 200 100 -12 20 0 0 FALSE))
(define RECT2 (make-rect 200 200 23 -14 0 0 FALSE))
(define PAUSED-WORLD (make-world RECT1 RECT2 TRUE))
(define PAUSED-WORLD-TOGGLED (make-world RECT1 RECT2 FALSE))

(define UNPAUSED-WORLD (make-world RECT1 RECT2 FALSE))
(define UNPAUSED-WORLD-TOGGLED (make-world RECT1 RECT2 TRUE))
(define RECT1-AFTER-1-TICK (make-rect 188 120 -12 20 0 0 FALSE))
(define RECT2-AFTER-1-TICK (make-rect 223 186 23 -14 0 0 FALSE))
(define UNPAUSED-WORLD-AFTER-1-TICK (make-world RECT1-AFTER-1-TICK
                                                RECT2-AFTER-1-TICK
                                                FALSE))

(define RECT1-PAST-TOP-LEFT-CORNER (make-rect 0 0 -12 -20 0 0 FALSE))
(define RECT2-PAST-BOTTOM-LEFT-CORNER (make-rect 0 300 -23 14 0 0 FALSE))
(define WORLD1-WITH-RECT-PAST-CORNERS (make-world RECT1-PAST-TOP-LEFT-CORNER
                                                  RECT2-PAST-BOTTOM-LEFT-CORNER
                                                  FALSE))
(define RECT1-BACK-INSIDE-CANVAS (make-rect 31 26 12 20 0 0 FALSE))
(define RECT2-BACK-INSIDE-CANVAS (make-rect 31 274 23 -14 0 0 FALSE))
(define WORLD1-WITH-RECT-BACK-INSIDE-CANVAS (make-world RECT1-BACK-INSIDE-CANVAS
                                                        RECT2-BACK-INSIDE-CANVAS
                                                        FALSE))

(define RECT3-OUTSIDE-RIGHT-WALL (make-rect 390 200 12 -20 0 0 FALSE))
(define RECT4-OUTSIDE-LEFT-WALL (make-rect -10 150 -23 -14 0 0 FALSE))
(define WORLD2-WITH-RECT-OUTSIDE-SIDE-WALLS (make-world RECT3-OUTSIDE-RIGHT-WALL
                                                        RECT4-OUTSIDE-LEFT-WALL
                                                        FALSE))
(define RECT3-BACK-INSIDE-CANVAS (make-rect 369 180 -12 -20 0 0 FALSE))
(define RECT4-BACK-INSIDE-CANVAS (make-rect 31 136 23 -14 0 0 FALSE))
(define WORLD2-WITH-RECT-BACK-INSIDE-CANVAS (make-world RECT3-BACK-INSIDE-CANVAS
                                                        RECT4-BACK-INSIDE-CANVAS
                                                        FALSE))

(define RECT5-OUTSIDE-TOP-WALL (make-rect 50 -10 12 -20 0 0 FALSE))
(define RECT6-OUTSIDE-BOTTOM-WALL (make-rect 70 310 23 14 0 0 FALSE))
(define WORLD3-WITH-RECT-OUTSIDE-TOPBOTTOM-WALLS (make-world RECT5-OUTSIDE-TOP-WALL
                                                             RECT6-OUTSIDE-BOTTOM-WALL
                                                             FALSE))
(define RECT5-BACK-INSIDE-CANVAS (make-rect 62 26 12 20 0 0 FALSE))
(define RECT6-BACK-INSIDE-CANVAS (make-rect 93 274 23 -14 0 0 FALSE))
(define WORLD3-WITH-RECT-INSIDE-CANVAS (make-world RECT5-BACK-INSIDE-CANVAS
                                                   RECT6-BACK-INSIDE-CANVAS
                                                   FALSE))

(define RECT7-PAST-TOP-RIGHT-CORNER (make-rect 400 0 12 -20 0 0 FALSE))
(define RECT8-PAST-BOTTOM-RIGHT-CORNER (make-rect 400 300 23 14 0 0 FALSE))
(define RECT7-BACK-INSIDE-CANVAS (make-rect 369 26 -12 20 0 0 FALSE))
(define RECT8-BACK-INSIDE-CANVAS (make-rect 369 274 -23 -14 0 0 FALSE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; screensaver : PosReal -> WorldState
;; GIVEN : The speed of the simulation, in seconds/tick
;; EFFECT : Runs the simulation, starting with the initial state as
;;         specified in the problem set
;; RETURNS : The final state of the world

(define (screensaver rate)
  (big-bang (initial-world 1)
            (on-tick world-after-tick rate)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN : Any value (ignored)
;; RETURNS : The initial world specified in the problem set
;; EXAMPLE :
;; (initial-world 1) => PAUSED-WORLD

;; DESIGN STRATEGY : Use template for World 
(define (initial-world n)
  (make-world
   (new-rectangle INITIAL-RECT1-X INITIAL-RECT1-Y
                  INITIAL-RECT1-VX INITIAL-RECT1-VY
                  0 0
                  FALSE)
   (new-rectangle INITIAL-RECT2-X INITIAL-RECT2-Y
                  INITIAL-RECT2-VX INITIAL-RECT2-VY
                  0 0
                  FALSE)
   TRUE))

;; TESTS :
(begin-for-test
  (check-equal?
   (initial-world 1)
   PAUSED-WORLD
   "Initial World should be (make-world (make-rect 200 100 -12 20 0 0 #false)
                                        (make-rect 200 200 23 -14 0 0 #false)
                                        #true)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle : NonNegInt NonNegInt Int Int Int Int Boolean -> Rect
;; GIVEN : 2 non-negative integers x and y, 2 integers vx and vy, the distance of
;;         x and y from mx and my at mouse button-down event and Boolean indicating
;;         whether the rectangle is selected or not
;; RETURNS : An unselected rectangle centered at (x,y), which will travel with
;;           velocity (vx, vy) 
;; EXAMPLE :
;; (new-rectangle 200 100 -12 20 0 0 FALSE) => RECT1

;; DESIGN STRATEGY : Use template for Rect
(define (new-rectangle x y vx vy mx-dist my-dist sel)
  (make-rect x y vx vy mx-dist my-dist sel))

;; TESTS :
(begin-for-test
  (check-equal?
   (new-rectangle 200 100 -12 20 0 0 FALSE)
   RECT1
   "Rectangle should be (make-rect 100 100 12 -20 0 0 #false)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; GIVEN : A world state
;; RETURNS: The world state that should follow the given world state
;;          after a tick
;; EXAMPLES :
;; (world-after-tick PAUSED-WORLD) => PAUSED-WORLD
;; (world-after-tick UNPAUSED-WORLD) => UNPAUSED-WORLD-AFTER-1-TICK
;; (world-after-tick WORLD1-SELECTED) => WORLD1-SELECTED-AFTER-1-TICK

;; DESIGN STRATEGY : Use template for World on ws
(define (world-after-tick ws)
  (if (world-paused? ws)
      ws
      (make-world
       (rect-after-tick (world-rect1 ws))
       (rect-after-tick (world-rect2 ws))
       (world-paused? ws))))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-tick PAUSED-WORLD)
   PAUSED-WORLD
   "World should be paused if world-paused? is #true")
  (check-equal?
   (world-after-tick UNPAUSED-WORLD)
   UNPAUSED-WORLD-AFTER-1-TICK
   "After 1 tick the world should be (make-world (make-rect 188 120 0 0 #false)
                                                 (make-rect 223 186 0 0 #false)
                                                 #false)")
  (check-equal?
   (world-after-tick WORLD1-WITH-RECT-PAST-CORNERS)
   WORLD1-WITH-RECT-BACK-INSIDE-CANVAS
   "After passing the corners the world should be
    (make-world (make-rect 31 26 12 20 0 0 FALSE))
                (make-rect 31 274 23 -14 0 0 FALSE)
                #false)")
  (check-equal?
   (world-after-tick WORLD2-WITH-RECT-OUTSIDE-SIDE-WALLS)
   WORLD2-WITH-RECT-BACK-INSIDE-CANVAS
   "After bouncing off the walls, the world should be
    (make-world (make-rect 369 180 -12 -20 0 0 #false)
                (make-rect 31 136 23 -14 0 0 #false)
                #false)")  
  (check-equal?
   (world-after-tick WORLD3-WITH-RECT-OUTSIDE-TOPBOTTOM-WALLS)
   WORLD3-WITH-RECT-INSIDE-CANVAS
   "After bouncing off the walls, the world should be
    (make-world (make-rect 62 26 12 20 0 0 #false)
                (make-rect 93 274 23 -14 0 0 #false)
                #false)")
  (check-equal?
   (world-after-tick WORLD1-SELECTED)
   WORLD1-SELECTED-AFTER-1-TICK
   "World with 1 rectangle selected will at next tick be
    (make-world (make-rect 200 120 -12 20 20 12 #true)
                (make-rect 223 186 23 -14 0 0 #false)
                #false)"))



;; Helper function for world-after-tick
;; rect-after-tick : Rect -> Rect
;; GIVEN : A rect
;; RETURNS : A rect based on whether the rectangle is selected or not and
;;           unselected rectangle is outside of canvas or not at the next tick
;; EXAMPLES :
;; (rect-after-tick RECT1) => RECT1-AFTER-1-TICK
;; (rect-after-tick RECT1-SELECTED-AFTER-1-TICK) => RECT1-SELECTED-AFTER-1-TICK

;; DESIGN STRATEGY : Divide into cases on rect-selected?
(define (rect-after-tick rt)
  (if (rect-selected? rt)
      rt
      (outside-of-corners? (rect-x rt) (rect-y rt)
                           (rect-vx rt) (rect-vy rt)
                           (rect-mx-dist rt) (rect-my-dist rt)
                           (rect-selected? rt))))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-tick RECT1)
   RECT1-AFTER-1-TICK
   "If the rectangle is not selected, it will return the rectangle at next tick")
  (check-equal?
   (rect-after-tick RECT1-SELECTED-AFTER-1-TICK)
   RECT1-SELECTED-AFTER-1-TICK
   "If the rectangle is selected, it will return the given rectangle"))



;; Helper function for rect-after-tick
;; outside-of-corners? : NonNegInt NonNegInt Int Int Int Int Boolean -> Rect
;; GIVEN : 2 non-negative integers x and y, 2 integers vx and vy, the distance of
;;         x and y from mx and my at mouse button-down event and Boolean indicating
;;         whether the rectangle is selected or not
;; RETURNS : A rect based on whether it will pass corners/walls at the
;;           next tick or not
;; EXAMPLES :
;; (outside-of-corners? 0 300 -23 14 0 0 FALSE) => RECT2-BACK-INSIDE-CANVAS
;; (outside-of-corners? 0 0 -12 -20 0 0 FALSE) => RECT1-BACK-INSIDE-CANVAS
;; (outside-of-corners? 200 100 -12 20 0 0 FALSE) => RECT1-AFTER-1-TICK

;; DESIGN STRATEGY : Divide into cases based on rectangle will go outside
;;                   which corner
(define (outside-of-corners? x y vx vy mx-dist my-dist sel) 
  (cond
    [(and (>= (+ x HALF-RECT-WIDTH vx) RIGHT-WALL-LIMIT)
          (<= (+ (- y HALF-RECT-HEIGHT) vy) TOP-WALL-LIMIT))
     (make-rect MAX-X-FOR-RECT MIN-Y-FOR-RECT (- vx) (- vy)
                mx-dist my-dist sel)]
    
    [(and (<= (+ (- x HALF-RECT-WIDTH) vx) LEFT-WALL-LIMIT)
          (>= (+ y HALF-RECT-HEIGHT vy) BOTTOM-WALL-LIMIT))
     (make-rect MIN-X-FOR-RECT MAX-Y-FOR-RECT (- vx) (- vy)
                mx-dist my-dist sel)]
    
    [(and (<= (+ (- x HALF-RECT-WIDTH) vx) LEFT-WALL-LIMIT)
          (<= (+ (- y HALF-RECT-HEIGHT) vy) TOP-WALL-LIMIT))
     (make-rect MIN-X-FOR-RECT MIN-Y-FOR-RECT (- vx) (- vy)
                mx-dist my-dist sel)]
    
    [(and (>= (+ x HALF-RECT-WIDTH vx) RIGHT-WALL-LIMIT)
          (>= (+ y HALF-RECT-HEIGHT vy) BOTTOM-WALL-LIMIT))
     (make-rect MAX-X-FOR-RECT MAX-Y-FOR-RECT (- vx) (- vy)
                mx-dist my-dist sel)]
    [else (outside-walls? x y vx vy mx-dist my-dist sel)]))
 

;; TESTS :
(begin-for-test
  (check-equal?
   (outside-of-corners? 0 300 -23 14 0 0 FALSE)
   RECT2-BACK-INSIDE-CANVAS
   "Since rectangle will pass the corner, rectangle at next tick will be
    inside canvas")
  (check-equal?
   (outside-of-corners? 0 0 -12 -20 0 0 FALSE)
   RECT1-BACK-INSIDE-CANVAS
   "Since rectangle will pass the corner, rectangle at next tick will be
    inside canvas")
  (check-equal?
   (outside-of-corners? 200 100 -12 20 0 0 FALSE)
   RECT1-AFTER-1-TICK
   "Since rectangle will not pass the corner, it will follow its path based
    its velocities")
  (check-equal?
   (outside-of-corners? 400 0 12 -20 0 0 FALSE)
   RECT7-BACK-INSIDE-CANVAS
   "Since rectangle will pass the corner, rectangle at next tick will be
    inside canvas")
  (check-equal?
   (outside-of-corners? 400 300 23 14 0 0 FALSE)
   RECT8-BACK-INSIDE-CANVAS
   "Since rectangle will pass the corner, rectangle at next tick will be
    inside canvas"))
 


;; Helper function for outside-of-corners?
;; outside-walls? : NonNegInt NonNegInt Int Int Int Int Boolean -> Rect
;; GIVEN : 2 non-negative integers x and y,  2 integers vx and vy, the distance of
;;         x and y from mx and my at mouse button-down event and Boolean indicating
;;         whether the rectangle is selected or not
;; RETURNS : A rectangle based on whether it will go past the walls at the next tick
;;           or not
;; EXAMPLES :
;; (outside-walls? 390 200 12 -20 0 0 FALSE) => RECT3-BACK-INSIDE-CANVAS
;; (outside-walls? -10 150 -23 -14 0 0 FALSE) => RECT4-BACK-INSIDE-CANVAS
;; (outside-walls? 200 100 -12 20 0 0 FALSE) => RECT1-AFTER-1-TICK

;; DESIGN STRATEGY : Divide into cases on whether rectangle will be outside the
;;                   4 walls at next tick or not
(define (outside-walls? x y vx vy mx-dist my-dist sel)
  (cond   
    [(> (+ x HALF-RECT-WIDTH vx)
        RIGHT-WALL-LIMIT)
     (make-rect MAX-X-FOR-RECT (+ y vy) (- vx) vy mx-dist my-dist sel)] 
    [(< (+ (- x HALF-RECT-WIDTH) vx )
        LEFT-WALL-LIMIT)
     (make-rect MIN-X-FOR-RECT (+ y vy) (- vx) vy mx-dist my-dist sel)]
    [(< (+ (- y HALF-RECT-HEIGHT) vy)
        TOP-WALL-LIMIT)
     (make-rect (+ x vx) MIN-Y-FOR-RECT vx (- vy) mx-dist my-dist sel)]
    [(> (+ y HALF-RECT-HEIGHT vy)
        BOTTOM-WALL-LIMIT) 
     (make-rect (+ x vx) MAX-Y-FOR-RECT vx (- vy) mx-dist my-dist sel)]
    [else (make-rect (+ x vx) (+ y vy) vx vy mx-dist my-dist sel)]))

;; TESTS :
(begin-for-test
  (check-equal?
   (outside-walls? 390 200 12 -20 0 0 FALSE)
   RECT3-BACK-INSIDE-CANVAS
   "Rectangle after bouncing from wall should be
   (make-rect 369 180 -12 -20 0 0 FALSE)")
  (check-equal?
   (outside-walls? -10 150 -23 -14 0 0 FALSE)
   RECT4-BACK-INSIDE-CANVAS
   "Rectangle after bouncing from wall should be
   (make-rect 31 136 23 -14 0 0 FALSE)")
  (check-equal?
   (outside-walls? 50 -10 12 -20 0 0 FALSE)
   RECT5-BACK-INSIDE-CANVAS
   "Rectangle after bouncing from wall should be
   (make-rect 62 26 12 20 0 0 FALSE)")
  (check-equal?
   (outside-walls? 70 310 23 14 0 0 FALSE)
   RECT6-BACK-INSIDE-CANVAS
   "Rectangle after bouncing from wall should be
   (make-rect 93 274 23 -14 0 0 FALSE)")
  (check-equal?
   (outside-walls? 200 100 -12 20 0 0 FALSE)
   RECT1-AFTER-1-TICK
   "Rectangle after bouncing from wall should be
   (make-rect 188 120 -12 20 0 0 FALSE)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN : A world state and key event
;; RETURNS: The WorldState that should follow the given worldstate after
;;          the given keyevent
;; EXAMPLES :
;; (world-after-key-event PAUSED-WORLD PAUSE-KEY-EVENT) => PAUSED-WORLD-TOGGLED
;; (world-after-key-event PAUSED-WORLD NONPAUSE-KEY-EVENT) => PAUSED-WORLD
;; (world-after-key-event WORLD1-SELECTED PAUSE-KEY-EVENT) => WORLD1-SELECTED-PAUSED

;; DESIGN STRATEGY : Divide into cases on KeyEvent ke
(define (world-after-key-event ws ke)
  (if(key=? ke " ")
     (world-paused-toggled ws)
     ws))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-key-event PAUSED-WORLD
                          PAUSE-KEY-EVENT)
   PAUSED-WORLD-TOGGLED
   "Paused world on pause key event toggles the world to unpaused state" )
  (check-equal?
   (world-after-key-event PAUSED-WORLD
                          NONPAUSE-KEY-EVENT)
   PAUSED-WORLD
   "Paused world on non-pause key event keeps the world in paused state only")
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD
                          NONPAUSE-KEY-EVENT)
   UNPAUSED-WORLD
   "Unpaused world on non-pause key event keeps the world in unpaused state only")
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD
                          PAUSE-KEY-EVENT)
   UNPAUSED-WORLD-TOGGLED
   "Unpaused world on pause key event toggles the world to paused state")
  (check-equal?
   (world-after-key-event WORLD1-SELECTED
                          PAUSE-KEY-EVENT)
   WORLD1-SELECTED-PAUSED
   "Even if the rectangle is selected, on pause key event, the entire simulation
    will be paused with rectangle selected only"))



;; Helper function for world-after-key-event
;; world-paused-toggled : WorldState -> WorldState
;; GIVEN : A world state
;; RETURNS : A world state after it is paused
;; EXAMPLES :
;; (world-paused-toggled UNPAUSED-WORLD) => UNPAUSED-WORLD-TOGGLED
;; (world-paused-toggled UNPAUSED-WORLD-TOGGLED) => UNPAUSED-WORLD

;; DESIGN STRATEGY : Use template for World on ws
(define (world-paused-toggled ws)
  (make-world
   (world-rect1 ws)
   (world-rect2 ws)
   (not (world-paused? ws))))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-paused-toggled UNPAUSED-WORLD)
   UNPAUSED-WORLD-TOGGLED
  "After pause key event on unpaused world, the world should be paused world")
  (check-equal?
   (world-paused-toggled UNPAUSED-WORLD-TOGGLED)
   UNPAUSED-WORLD
   "After pause key event on paused world, the world becomes unpaused"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples

;; Selected Rectangles
(define RECT1-SELECTED (make-rect 200 120 -12 20 20 12 TRUE))
(define WORLD1-SELECTED (make-world RECT1-SELECTED RECT2 FALSE))
(define WORLD1-SELECTED-PAUSED (make-world RECT1-SELECTED RECT2 TRUE))
(define WORLD1-SELECTED-AFTER-1-TICK (make-world RECT1-SELECTED RECT2-AFTER-1-TICK FALSE))

(define RECT1-BEFORE-BUTTON-DOWN (make-rect 200 100 -12 20 0 0 FALSE))
(define RECT1-AFTER-BUTTON-DOWN (make-rect 200 100 -12 20 10 10 TRUE))
(define WORLD1-BEFORE-BUTTON-DOWN (make-world RECT1-BEFORE-BUTTON-DOWN RECT2 FALSE))
(define WORLD1-AFTER-BUTTON-DOWN (make-world RECT1-AFTER-BUTTON-DOWN RECT2 FALSE))

(define RECT2-BEFORE-DRAG (make-rect 200 200 23 -14 10 10 TRUE))
(define RECT2-AFTER-DRAG (make-rect 225 230 23 -14 10 10 TRUE))

(define RECT2-BEFORE-BUTTON-UP (make-rect 225 230 23 -14 10 10 TRUE))
(define RECT2-AFTER-BUTTON-UP (make-rect 225 230 23 -14 10 10 false))

(define RECT1-AFTER-OTHER-EVENT (make-rect 200 100 -12 20 0 0 FALSE))

(define RECT1-SELECTED-AFTER-1-TICK (make-rect 188 120 -12 20 10 10 TRUE))


;; world-after-mouse-event : WorldState Int Int MouseEvent -> WorldState
;; GIVEN : A World, the x- and y-coordinates of a mouse event, and the
;;         mouse event
;; RETURNS : The world that should follow the given world after the given mouse
;;           event
;; EXAMPLE :
;; (world-after-mouse-event WORLD1-BEFORE-BUTTON-DOWN 210 110 BUTTON-DOWN-EVENT)
;;  => WORLD1-AFTER-BUTTON-DOWN

;; DESIGN STRATEGY : Use template for World on ws
(define (world-after-mouse-event ws mx my me)
  (make-world
    (rect-after-mouse-event (world-rect1 ws) mx my me)
    (rect-after-mouse-event (world-rect2 ws) mx my me)
    (world-paused? ws)))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-mouse-event WORLD1-BEFORE-BUTTON-DOWN 210 110 BUTTON-DOWN-EVENT)
   WORLD1-AFTER-BUTTON-DOWN
   "After there's button down on Rectangle 1, the world should be
    (make-world (make-rect 200 120 -12 20 10 10 #true)
                (make-rect 200 200 23 -14 0 0 #false)
                #false)"))



;; rect-after-mouse-event :  Rect Int Int MouseEvent -> Rect
;; GIVEN : A rectangle, the x- and y-coordinates of a mouse event, and the
;;         mouse event
;; RETURNS : The rectangle that should follow the given rectangle after
;;           the given mouse event
;; EXAMPLES :
;; (rect-after-mouse-event RECT1-BEFORE-BUTTON-DOWN 210 110 BUTTON-DOWN-EVENT)
;;   => RECT1-AFTER-BUTTON-DOWN
;; (rect-after-mouse-event RECT2-BEFORE-DRAG 235 240 DRAG-EVENT) => RECT2-AFTER-DRAG
;; (rect-after-mouse-event RECT1 210 110 OTHER-EVENT) => RECT1-AFTER-OTHER-EVENT

;; DESIGN STRATEGY : Divide into cases on MouseEvent me
(define (rect-after-mouse-event rt mx my me)
  (cond
    [(mouse=? me BUTTON-DOWN-EVENT)
     (rect-after-button-down rt mx my)]
    [(mouse=? me DRAG-EVENT)
     (rect-after-drag rt mx my)]
    [(mouse=? me BUTTON-UP-EVENT)
     (rect-after-button-up rt mx my)]
    [else rt]))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-mouse-event RECT1-BEFORE-BUTTON-DOWN 210 110 BUTTON-DOWN-EVENT)
   RECT1-AFTER-BUTTON-DOWN
   "After button down on a rectangle, the selected? field of the rectangle
    should be #true")
  (check-equal?
   (rect-after-mouse-event RECT2-BEFORE-DRAG 235 240 DRAG-EVENT)
   RECT2-AFTER-DRAG
   "After drag, the rectangle should have moved by given (mx, my)")
  (check-equal?
   (rect-after-mouse-event RECT2-BEFORE-BUTTON-UP 235 240 BUTTON-UP-EVENT)
   RECT2-AFTER-BUTTON-UP
   "After button up on a rectangle, the selected? field of the rectangle
    should be #false")
  (check-equal?
   (rect-after-mouse-event RECT1 210 110 OTHER-EVENT)
   RECT1-AFTER-OTHER-EVENT
   "On any other event, the rectangle should remain as before"))

 

;; Helper Function for rect-after-mouse-event
;; rect-after-button-down : Rect Int Int -> Rect
;; GIVEN : A rect and mouse coordinates mx and my
;; RETURNS : A rect after button-down event
;; EXAMPLES :
;; (rect-after-button-down RECT1-BEFORE-BUTTON-DOWN 210 110)
;;   => RECT1-AFTER-BUTTON-DOWN
;; (rect-after-button-down RECT1-BEFORE-BUTTON-DOWN 250 125)
;;   => RECT1-BEFORE-BUTTON-DOWN

;; DESIGN STRATEGY : Use template for Rect on rt
(define (rect-after-button-down rt mx my)
  (if (inside-rect? rt mx my)
      (make-rect (rect-x rt) (rect-y rt)
                 (rect-vx rt) (rect-vy rt)
                 (- mx (rect-x rt))
                 (- my (rect-y rt))
                 TRUE)
      rt))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-button-down RECT1-BEFORE-BUTTON-DOWN 210 110)
   RECT1-AFTER-BUTTON-DOWN
   "After button down on a rectangle, the selected? field of the rectangle
    should be #true")
  (check-equal?
   (rect-after-button-down RECT1-BEFORE-BUTTON-DOWN 250 125)
   RECT1-BEFORE-BUTTON-DOWN
   "If the mouse pointer is outside the rectangle, the rectangle won't
    be selected"))



;; Helper function for rect-after-button-down
;; inside-rect? : Rect Int Int -> Rect
;; GIVEN :  A rect and mouse coordinates mx and my
;; RETURNS : True iff the mouse is inside the rectangle
;; EXAMPLES :
;; (inside-rect? RECT1-BEFORE-BUTTON-DOWN 210 110) => TRUE
;; (inside-rect? RECT1-BEFORE-BUTTON-DOWN 240 150) => FALSE

;; DESIGN STRATEGY : Use template for Rect on rt
(define (inside-rect? rt mx my)
  (and (<= (- (rect-x rt) HALF-RECT-WIDTH)
           mx
           (+ (rect-x rt) HALF-RECT-WIDTH))
       (<= (- (rect-y rt) HALF-RECT-HEIGHT)
           my
           (+ (rect-y rt) HALF-RECT-HEIGHT))))

;; TESTS :
(begin-for-test
  (check-equal?
   (inside-rect? RECT1-BEFORE-BUTTON-DOWN 210 110)
   TRUE
   "Function returns #true if the mouse is inside the rectangle")
  (check-equal?
   (inside-rect? RECT1-BEFORE-BUTTON-DOWN 240 150)
   FALSE
   "Function returns #false if the mouse is outside the rectangle"))

 

;; Helper Function for rect-after-mouse-event
;; rect-after-drag : Rect Int Int -> Rect
;; GIVEN :  A rect and mouse coordinates mx and my
;; RETURNS : A rect after mouse drag event
;; EXAMPLES :
;; (rect-after-drag RECT2-BEFORE-DRAG 235 240) => RECT2-AFTER-DRAG
;; (rect-after-drag RECT1-BEFORE-BUTTON-DOWN 210 100) => RECT1-BEFORE-BUTTON-DOWN

;; DESIGN STRATEGY : Use template for Rect on rt
(define (rect-after-drag rt mx my)
  (if (and (rect-selected? rt) (rect-inside-canvas-at-drag? rt mx my))
      (make-rect (- mx (rect-mx-dist rt))
                 (- my (rect-my-dist rt))
                 (rect-vx rt)
                 (rect-vy rt)
                 (rect-mx-dist rt)
                 (rect-my-dist rt)
                 TRUE)
      rt))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-drag RECT2-BEFORE-DRAG 235 240)
   RECT2-AFTER-DRAG
   "After drag, the rectangle should have moved by given (mx, my)")
  (check-equal?
   (rect-after-drag RECT1-BEFORE-BUTTON-DOWN 210 100)
   RECT1-BEFORE-BUTTON-DOWN
   "Since the rectangle is not selected, the given rectangle will
    only be returned"))



;; Helper Function for rect-after-drag
;; rect-inside-canvas-at-drag? : Rect Int Int -> Boolean
;; GIVEN : A rect and mouse coordinates mx and my
;; RETURNS : True iff the rectangle during drag is inside the canvas
;; EXAMPLES :
;; (rect-inside-canvas-at-drag? RECT2-BEFORE-DRAG 235 240) => TRUE
;; (rect-inside-canvas-at-drag? RECT2-BEFORE-DRAG 410 240) => FALSE

;; DESIGN STRATEGY : Use template for Rect on rt
(define (rect-inside-canvas-at-drag? rt mx my)
  (and (< LEFT-WALL-LIMIT
          (- (- mx (rect-mx-dist rt)) HALF-RECT-WIDTH))
       (> RIGHT-WALL-LIMIT
          (+ (- mx (rect-mx-dist rt)) HALF-RECT-WIDTH))
       (< TOP-WALL-LIMIT
          (- (- my (rect-my-dist rt)) HALF-RECT-HEIGHT))
       (> BOTTOM-WALL-LIMIT
          (+ (- my (rect-my-dist rt)) HALF-RECT-HEIGHT))))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-inside-canvas-at-drag? RECT2-BEFORE-DRAG 235 240)
   TRUE
   "Function returns #true if during drag the rectangle doesn't go
    outside the canvas")
  (check-equal?
   (rect-inside-canvas-at-drag? RECT2-BEFORE-DRAG 410 240)
   FALSE
   "Function returns #false if during drag the rectangle goes
    outside the canvas"))



;; Helper Function for rect-after-mouse-event
;; rect-after-button-up : Rect Int Int -> Rect
;; GIVEN :  A rect and mouse coordinates mx and my
;; RETURNS : A rect after button-up event
;; EXAMPLES :
;; (rect-after-button-up RECT2-BEFORE-BUTTON-UP 235 240) => RECT2-AFTER-BUTTON-UP
;; (rect-after-button-up RECT2-AFTER-BUTTON-UP 235 240) => RECT2-AFTER-BUTTON-UP

;; DESIGN STRATEGY : Use template for Rect on rt
(define (rect-after-button-up rt mx my)
  (if (rect-selected? rt)
      (make-rect (rect-x rt) (rect-y rt)
                 (rect-vx rt) (rect-vy rt)
                 (rect-mx-dist rt) (rect-my-dist rt)
                 FALSE)
      rt))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-button-up RECT2-BEFORE-BUTTON-UP 235 240)
   RECT2-AFTER-BUTTON-UP
   "On button-up, if the given rectangle was selected, its selected? field
    will now be #false and rest fields will be same")
  (check-equal?
   (rect-after-button-up RECT2-AFTER-BUTTON-UP 235 240)
   RECT2-AFTER-BUTTON-UP
   "On button-up, if the rectangle wasn't selected, the given rectangle
    will be returned"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples for world-to-scene, place-rectangle and place-velocity-inside-rect
;;              red-rect-with-circle functions

(define TEXT2 (text (string-append OPEN-BRACKET
                                   (number->string INITIAL-RECT2-VX)
                                   COMMA
                                   (number->string INITIAL-RECT2-VY)
                                   CLOSE-BRACKET)
                    TEXT-HEIGHT
                    TEXT-COLOR-BLUE))
(define STRING-INSIDE-RECT2 (overlay TEXT2 BLUE-RECT))
(define SCENE1 (place-image STRING-INSIDE-RECT2
                            INITIAL-RECT2-X
                            INITIAL-RECT2-Y
                            EMPTY-CANVAS))

(define TEXT1 (text (string-append OPEN-BRACKET
                                   (number->string INITIAL-RECT1-VX)
                                   COMMA
                                   (number->string INITIAL-RECT1-VY)
                                   CLOSE-BRACKET)
                    TEXT-HEIGHT
                    TEXT-COLOR-BLUE))
(define STRING-INSIDE-RECT1 (overlay TEXT1 BLUE-RECT))
(define SCENE (place-image STRING-INSIDE-RECT1
                           INITIAL-RECT1-X
                           INITIAL-RECT1-Y
                           SCENE1))

(define TEXT3 (text (string-append OPEN-BRACKET
                                   (number->string INITIAL-RECT1-VX)
                                   COMMA
                                   (number->string INITIAL-RECT1-VY)
                                   CLOSE-BRACKET)
                    TEXT-HEIGHT
                    TEXT-COLOR-RED))
(define RED-RECT-WITH-CIRCLE (overlay/xy RED-RECT 35 30 RED-CIRCLE))
(define STRING-INSIDE-RECT1-SELECTED (overlay TEXT3 RED-RECT-WITH-CIRCLE))
(define SCENE3 (place-image STRING-INSIDE-RECT1-SELECTED
                            INITIAL-RECT1-X
                            INITIAL-RECT1-Y
                            SCENE1))

;; world-to-scene : WorldState -> Scene
;; GIVEN : A world state
;; RETURNS : A scene that potrays the world state
;; EXAMPLES :
;; (world-to-scene PAUSED-WORLD) => SCENE
;; (world-to-scene WORLD1-AFTER-BUTTON-DOWN) => SCENE3

;; DESIGN STRATEGY : Use template for World on ws
(define (world-to-scene ws)
  (place-rectangle
   (world-rect1 ws)
   (place-rectangle (world-rect2 ws)
                    EMPTY-CANVAS)))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-to-scene PAUSED-WORLD)
   SCENE
   "Parameters for world-to-scene are wrong")
  (check-equal?
   (world-to-scene WORLD1-AFTER-BUTTON-DOWN)
   SCENE3
   "Parameters for world-to-scene are wrong"))



;; Helper function for world-to-scene
;; place-rectangle : Rect Scene -> Scene
;; GIVEN : A rect and scene
;; RETURNS : A scene like the given one with rect painted on it
;; EXAMPLE :
;; (place-rectangle RECT1 SCENE1) => SCENE
;; (place-rectangle RECT1-AFTER-BUTTON-DOWN SCENE1) => SCENE3

;; DESIGN STRATEGY : Use template for Rect on rt
(define (place-rectangle rt scene)
  (place-image (place-velocity-inside-rect
                rt
                (rect-vx rt)
                (rect-vy rt)
                (rect-selected? rt))
               (rect-x rt)
               (rect-y rt)
               scene))

;; TESTS :
(begin-for-test
  (check-equal?
   (place-rectangle RECT1 SCENE1)
   SCENE
   "Parameters for place-rectangle are wrong")
  (check-equal?
   (place-rectangle RECT1-AFTER-BUTTON-DOWN SCENE1)
   SCENE3
   "Parameters for place-rectangle are wrong"))



;; Helper function for place-rectangle
;; place-velocity-inside-rect : Rect Integer Integer Boolean -> Scene
;; GIVEN : A rect, 2 integers vx and vy, which represent the velocities of rectangle,
;;         and a Boolean indicating whether the rectangle is selected or not
;; RETURNS : A scene with (vx, vy), and red circle painted on rectangle based on
;;           whether rectangle is selected or not
;; EXAMPLE :
;; (place-velocity-inside-rect RECT1 INITIAL-RECT1-VX INITIAL-RECT1-VY FALSE)
;;   => STRING-INSIDE-RECT1
;; (place-velocity-inside-rect RECT1-AFTER-BUTTON-DOWN
;;                             INITIAL-RECT1-VX INITIAL-RECT1-VY TRUE)
;;   => STRING-INSIDE-RECT1-SELECTED

;; DESIGN STRATEGY : Combine simpler functions
(define (place-velocity-inside-rect rt vx vy sel)
  (overlay
   (text
    (string-append OPEN-BRACKET
                   (number->string vx)
                   COMMA
                   (number->string vy)
                   CLOSE-BRACKET)
    TEXT-HEIGHT
    (if sel
        TEXT-COLOR-RED
        TEXT-COLOR-BLUE))
   (if sel
       (red-rect-with-circle rt)
       BLUE-RECT)))

;; TESTS :
(begin-for-test
  (check-equal?
   (place-velocity-inside-rect RECT1 INITIAL-RECT1-VX INITIAL-RECT1-VY FALSE)
   STRING-INSIDE-RECT1
   "Parameters for place-velocity-inside-rect are wrong")
  (check-equal?
   (place-velocity-inside-rect RECT1-AFTER-BUTTON-DOWN
                               INITIAL-RECT1-VX INITIAL-RECT1-VY TRUE)
   STRING-INSIDE-RECT1-SELECTED
   "Parameters for place-velocity-inside-rect are wrong"))



;; Helper function for place-velocity-inside-rect
;; red-rect-with-circle : Rect -> Scene
;; GIVEN : A rect
;; RETURNS : A red circle drawn on rectangle at (mx, my)
;; EXAMPLE :
;; (red-rect-with-circle RECT1-AFTER-BUTTON-DOWN) => RED-RECT-WITH-CIRCLE

;; DESIGN STRATEGY : Combine simpler functions
(define (red-rect-with-circle rt)
  (overlay/xy RED-RECT
           (- (+ HALF-RECT-WIDTH (rect-mx-dist rt))
              CIRCLE-RADIUS)
           (- (+ HALF-RECT-HEIGHT (rect-my-dist rt))
              CIRCLE-RADIUS)
           RED-CIRCLE))

;; TESTS :
(begin-for-test
  (check-equal?
   (red-rect-with-circle RECT1-AFTER-BUTTON-DOWN)
   RED-RECT-WITH-CIRCLE))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start with screensaver
;(screensaver 0.5)