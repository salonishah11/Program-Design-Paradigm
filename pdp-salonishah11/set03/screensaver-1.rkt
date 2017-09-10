;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; screensaver-1.rkt
;; Screensaver-1 is a program with two rectangles moving around the canvas.
;; The rectangles bounce smoothly on touching the sides of canvas. The rectangles
;; have a fixed velocity, and their directions change on touching the sides of canvas.
;; When a rectangle passes the corner, it bounces back with x- and y- velocities
;; reversed.



(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "03" "screensaver-1.rkt")

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-rect1 
 world-rect2 
 world-paused?
 new-rectangle
 rect-x 
 rect-y 
 rect-vx
 rect-vy)

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
(define RECT (rectangle RECT-WIDTH RECT-HEIGHT "outline" "blue"))

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
(define TEXT-COLOR "blue")

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

;; Booleans
(define TRUE #true)
(define FALSE #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; Structure Rect :
(define-struct rect (x y vx vy))

;; CONSTRUCTOR TEMPLATE :
;; A rect is (make-rect NonNegInt NonNegInt Integer Integer)

;; INTERPRETATION :
;; (make-rect NonNegInt NonNegInt Integer Integer) is a rect where
;; x and y are the coordinates of the center of rectangle
;; vx and vy are the velocities of rectangle in x and y direction
;; respectively

;; DESTRUCTOR TEMPLATE :
;; rect-fn : Rect -> ??
#;(define (rect-fn rt)
    (...
     (rect-x rt)
     (rect-y rt)
     (rect-vx rt)
     (rect-vy rt)
     ...))



;; Structure World :
(define-struct world (rect1 rect2 paused?))

;; CONSTRUCTOR TEMPLATE :
;; A world is (make-world Rect Rect Boolean)

;; INTERPRETATION :
;; (make-world Rect Rect Boolean) is a world where
;; rect1 and rect2 are rectangles of structure Rect
;; paused? is a boolean which represents whether the simulation is paused
;; or not

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

(define RECT1 (make-rect 200 100 -12 20))
(define RECT2 (make-rect 200 200 23 -14))
(define PAUSED-WORLD (make-world RECT1 RECT2 TRUE))
(define PAUSED-WORLD-TOGGLED (make-world RECT1 RECT2 FALSE))

(define UNPAUSED-WORLD (make-world RECT1 RECT2 FALSE))
(define UNPAUSED-WORLD-TOGGLED (make-world RECT1 RECT2 TRUE))
(define RECT1-AFTER-1-TICK (make-rect 188 120 -12 20))
(define RECT2-AFTER-1-TICK (make-rect 223 186 23 -14))
(define UNPAUSED-WORLD-AFTER-1-TICK (make-world RECT1-AFTER-1-TICK
                                                RECT2-AFTER-1-TICK
                                                FALSE))

(define RECT1-PAST-TOP-LEFT-CORNER (make-rect 0 0 -12 -20))
(define RECT2-PAST-BOTTOM-LEFT-CORNER (make-rect 0 300 -23 14))
(define WORLD1-WITH-RECT-PAST-CORNERS (make-world RECT1-PAST-TOP-LEFT-CORNER
                                                  RECT2-PAST-BOTTOM-LEFT-CORNER
                                                  FALSE))
(define RECT1-BACK-INSIDE-CANVAS (make-rect 31 26 12 20))
(define RECT2-BACK-INSIDE-CANVAS (make-rect 31 274 23 -14))
(define WORLD1-WITH-RECT-BACK-INSIDE-CANVAS (make-world RECT1-BACK-INSIDE-CANVAS
                                                        RECT2-BACK-INSIDE-CANVAS
                                                        FALSE))

(define RECT3-OUTSIDE-RIGHT-WALL (make-rect 390 200 12 -20))
(define RECT4-OUTSIDE-LEFT-WALL (make-rect -10 150 -23 -14))
(define WORLD2-WITH-RECT-OUTSIDE-SIDE-WALLS (make-world RECT3-OUTSIDE-RIGHT-WALL
                                                        RECT4-OUTSIDE-LEFT-WALL
                                                        FALSE))
(define RECT3-BACK-INSIDE-CANVAS (make-rect 369 180 -12 -20))
(define RECT4-BACK-INSIDE-CANVAS (make-rect 31 136 23 -14))
(define WORLD2-WITH-RECT-BACK-INSIDE-CANVAS (make-world RECT3-BACK-INSIDE-CANVAS
                                                        RECT4-BACK-INSIDE-CANVAS
                                                        FALSE))

(define RECT5-OUTSIDE-TOP-WALL (make-rect 50 -10 12 -20))
(define RECT6-OUTSIDE-BOTTOM-WALL (make-rect 70 310 23 14))
(define WORLD3-WITH-RECT-OUTSIDE-TOPBOTTOM-WALLS (make-world RECT5-OUTSIDE-TOP-WALL
                                                             RECT6-OUTSIDE-BOTTOM-WALL
                                                             FALSE))
(define RECT5-BACK-INSIDE-CANVAS (make-rect 62 26 12 20))
(define RECT6-BACK-INSIDE-CANVAS (make-rect 93 274 23 -14))
(define WORLD3-WITH-RECT-INSIDE-CANVAS (make-world RECT5-BACK-INSIDE-CANVAS
                                                   RECT6-BACK-INSIDE-CANVAS
                                                   FALSE))

(define RECT7-PAST-TOP-RIGHT-CORNER (make-rect 400 0 12 -20))
(define RECT8-PAST-BOTTOM-RIGHT-CORNER (make-rect 400 300 23 14))
(define RECT7-BACK-INSIDE-CANVAS (make-rect 369 26 -12 20))
(define RECT8-BACK-INSIDE-CANVAS (make-rect 369 274 -23 -14))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; screensaver : PosReal -> WorldState
;; GIVEN : The speed of the simulation, in seconds/tick
;; EFFECT : Runs the simulation, starting with the initial state as
;;         specified in the problem set
;; RETURNS : The final state of the world
;; EXAMPLES :
;; (screensaver 0.5) => (make-world (make-rect 200 100 -12 20)
;;                                  (make-rect 200 200 23 -14)
;;                                  #true)
;; (screensaver 0.5) => (make-world (make-rect 80 60 12 -20)
;;                                  (make-rect 108 116 -23 14)
;;                                  #false)

(define (screensaver rate)
  (big-bang (initial-world 1)
            (on-tick world-after-tick rate)
            (on-key world-after-key-event)
            (on-draw world-to-scene)))

;; TESTS :
#;(begin-for-test
    (check-equal?
     (screensaver 0.5)
     (make-world (make-rect 200 100 -12 20) (make-rect 200 200 23 -14) TRUE)
     "Final state on closing the simulation without running it should be
     (make-world (make-rect 200 100 -12 20)
                 (make-rect 200 200 23 -14))
                  #true)")
    (check-equal?
     (screensaver 0.5)
     (make-world (make-rect 80 60 12 -20) (make-rect 108 116 -23 14) FALSE)
     "Final state on closing the simulation after 15 ticks should be
     (make-world (make-rect 80 60 12 -20)
                 (make-rect 108 116 -23 14)
                 #false)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN : Any value (ignored)
;; RETURNS : The initial world specified in the problem set
;; EXAMPLES :
;; (initial-world 1) => PAUSED-WORLD

;; DESIGN STRATEGY : Use template for World 
(define (initial-world n)
  (make-world
   (new-rectangle INITIAL-RECT1-X INITIAL-RECT1-Y
                  INITIAL-RECT1-VX INITIAL-RECT1-VY)
   (new-rectangle INITIAL-RECT2-X INITIAL-RECT2-Y
                  INITIAL-RECT2-VX INITIAL-RECT2-VY)
   TRUE))

;; TESTS :
(begin-for-test
  (check-equal?
   (initial-world 1)
   PAUSED-WORLD
   "Initial World should be (make-world (make-rect 200 100 -12 20)
                            (make-rect 200 200 23 -14)
                            #true)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN : 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS : A rectangle centered at (x,y), which will travel with
;;           velocity (vx, vy)
;; EXAMPLES :
;; (new-rectangle 200 100 -12 20) => RECT1

;; DESIGN STRATEGY : Use template for Rect
(define (new-rectangle x y vx vy)
  (make-rect x y vx vy))

;; TESTS :
(begin-for-test
  (check-equal?
   (new-rectangle 200 100 -12 20)
   RECT1
   "Rectangle should be (make-rect 100 100 12 -20)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; GIVEN : A world state
;; RETURNS: The world state that should follow the given world state
;;          after a tick
;; EXAMPLES :
;; (world-after-tick PAUSED-WORLD) => PAUSED-WORLD
;; (world-after-tick UNPAUSED-WORLD) => UNPAUSED-WORLD-AFTER-1-TICK
;; (world-after-tick WORLD1-WITH-RECT-PAST-CORNERS) => WORLD1-WITH-RECT-BACK-INSIDE-CANVAS

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
   "After 1 tick the world should be (make-world (make-rect 188 120)
                                                 (make-rect 223 186)
                                                 #false)")
  (check-equal?
   (world-after-tick WORLD1-WITH-RECT-PAST-CORNERS)
   WORLD1-WITH-RECT-BACK-INSIDE-CANVAS
   "After passing the corners the world should be
    (make-world (make-rect 31 26 20 12)
                (make-rect 31 274 14 -23)
                #false)")
  (check-equal?
   (world-after-tick WORLD2-WITH-RECT-OUTSIDE-SIDE-WALLS)
   WORLD2-WITH-RECT-BACK-INSIDE-CANVAS
   "After bouncing off the walls, the world should be
    (make-world (make-rect 369 180 -12 -20)
                (make-rect 31 136 23 -14)
                #false)")  
  (check-equal?
   (world-after-tick WORLD3-WITH-RECT-OUTSIDE-TOPBOTTOM-WALLS)
   WORLD3-WITH-RECT-INSIDE-CANVAS
   "After bouncing off the walls, the world should be
    (make-world (make-rect 62 26 12 20)
                (make-rect 93 274 23 -14)
                #false)"))



;; Helper function for world-after-tick
;; rect-after-tick : Rect -> Rect
;; GIVEN : A rect
;; RETURNS : A rect based on the whether the rectangle will go outside the
;;           canvas on next tick or not
;; EXAMPLES :
;; (rect-after-tick RECT1-PAST-TOP-LEFT-CORNER) => RECT1-BACK-INSIDE-CANVAS
;; (rect-after-tick RECT5-OUTSIDE-TOP-WALL) => RECT5-BACK-INSIDE-CANVAS

;; DESIGN STRATEGY : Use template for Rect on rt
(define (rect-after-tick rt)
  (outside-of-corners? (rect-x rt) (rect-y rt)
                       (rect-vx rt) (rect-vy rt)))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-tick RECT1-PAST-TOP-LEFT-CORNER)
   RECT1-BACK-INSIDE-CANVAS
   "Rectangle back inside canvas will be (make-rect 31 26 12 20)")
  (check-equal?
   (rect-after-tick RECT5-OUTSIDE-TOP-WALL)
   RECT5-BACK-INSIDE-CANVAS
   "Rectangle back inside canvas will be (make-rect 62 26 12 20)"))



;; Helper function for rect-after-tick
;; outside-of-corners? : NonNegInt NonNegInt Int Int -> Rect
;; GIVEN : 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS :  A rect based on whether it will pass corners/walls at the
;;           next tick
;; EXAMPLES :
;; (outside-of-corners? 0 300 -23 14) => RECT2-BACK-INSIDE-CANVAS
;; (outside-of-corners? 0 0 -12 -20) => RECT1-BACK-INSIDE-CANVAS
;; (outside-of-corners? 200 100 -12 20) => RECT1-AFTER-1-TICK

;; DESIGN STRATEGY : Divide into cases based on rectangle will go outside
;;                   which corner
(define (outside-of-corners? x y vx vy) 
  (cond
    [(and (>= (+ x HALF-RECT-WIDTH vx) RIGHT-WALL-LIMIT)
          (<= (+ (- y HALF-RECT-HEIGHT) vy) TOP-WALL-LIMIT))
     (make-rect MAX-X-FOR-RECT MIN-Y-FOR-RECT (- vx) (- vy))]
    
    [(and (<= (+ (- x HALF-RECT-WIDTH) vx) LEFT-WALL-LIMIT)
          (>= (+ y HALF-RECT-HEIGHT vy) BOTTOM-WALL-LIMIT))
     (make-rect MIN-X-FOR-RECT MAX-Y-FOR-RECT (- vx) (- vy))]
    
    [(and (<= (+ (- x HALF-RECT-WIDTH) vx) LEFT-WALL-LIMIT)
          (<= (+ (- y HALF-RECT-HEIGHT) vy) TOP-WALL-LIMIT))
     (make-rect MIN-X-FOR-RECT MIN-Y-FOR-RECT (- vx) (- vy))]
    
    [(and (>= (+ x HALF-RECT-WIDTH vx) RIGHT-WALL-LIMIT)
          (>= (+ y HALF-RECT-HEIGHT vy) BOTTOM-WALL-LIMIT))
     (make-rect MAX-X-FOR-RECT MAX-Y-FOR-RECT (- vx) (- vy))]
    [else (outside-walls? x y vx vy)]))

;; TESTS :
(begin-for-test
  (check-equal?
   (outside-of-corners? 0 300 -23 14)
   RECT2-BACK-INSIDE-CANVAS
   "Since rectangle will pass the corner, rectangle at next tick will be
    inside canvas")
  (check-equal?
   (outside-of-corners? 0 0 -12 -20)
   RECT1-BACK-INSIDE-CANVAS
   "Since rectangle will pass the corner, rectangle at next tick will be
    inside canvas")
  (check-equal?
   (outside-of-corners? 200 100 -12 20)
   RECT1-AFTER-1-TICK
   "Since rectangle will not pass the corner, it will follow its path based
    its velocities")
  (check-equal?
   (outside-of-corners? 400 0 12 -20)
   RECT7-BACK-INSIDE-CANVAS
   "Since rectangle will pass the corner, rectangle at next tick will be
    inside canvas")
  (check-equal?
   (outside-of-corners? 400 300 23 14)
   RECT8-BACK-INSIDE-CANVAS
   "Since rectangle will pass the corner, rectangle at next tick will be
    inside canvas"))



;; Helper function for outside-of-corners?
;; outside-walls? : NonNegInt NonNegInt Integer Integer -> Rect
;; GIVEN : 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS : A rectangle at the next tick
;; EXAMPLES :
;; (outside-walls? 390 200 12 -20) => RECT3-BACK-INSIDE-CANVAS
;; (outside-walls? 200 100 -12 20) => RECT1-AFTER-1-TICK

;; DESIGN STRATEGY : Divide into cases on whether rectangle will be outside the
;;                   4 walls at next tick or not
(define (outside-walls? x y vx vy )
  (cond   
    [(> (+ x HALF-RECT-WIDTH vx)
        RIGHT-WALL-LIMIT)
     (make-rect MAX-X-FOR-RECT (+ y vy) (- vx) vy)]
    [(< (+ (- x HALF-RECT-WIDTH) vx )
        LEFT-WALL-LIMIT)
     (make-rect MIN-X-FOR-RECT (+ y vy) (- vx) vy)]
    [(< (+ (- y HALF-RECT-HEIGHT) vy)
        TOP-WALL-LIMIT)
     (make-rect (+ x vx) MIN-Y-FOR-RECT vx (- vy))]
    [(> (+ y HALF-RECT-HEIGHT vy)
        BOTTOM-WALL-LIMIT) 
     (make-rect (+ x vx) MAX-Y-FOR-RECT vx (- vy))]
    [else (make-rect (+ x vx) (+ y vy) vx vy)]))

;; TESTS :
(begin-for-test
  (check-equal?
   (outside-walls? 390 200 12 -20)
   RECT3-BACK-INSIDE-CANVAS
   "Rectangle after bouncing from wall should be (make-rect 369 180 -12 -20)")
  (check-equal?
   (outside-walls? -10 150 -23 -14)
   RECT4-BACK-INSIDE-CANVAS
   "Rectangle after bouncing from wall should be (make-rect 31 136 23 -14)")
  (check-equal?
   (outside-walls? 50 -10 12 -20)
   RECT5-BACK-INSIDE-CANVAS
   "Rectangle after bouncing from wall should be (make-rect 62 26 12 20)")
  (check-equal?
   (outside-walls? 70 310 23 14)
   RECT6-BACK-INSIDE-CANVAS
   "Rectangle after bouncing from wall should be (make-rect 93 274 23 -14)")
  (check-equal?
   (outside-walls? 200 100 -12 20)
   RECT1-AFTER-1-TICK
   "Rectangle after bouncing from wall should be (make-rect 188 120 -12 20)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN : A world state and key event
;; RETURNS: The WorldState that should follow the given worldstate
;;          after the given keyevent
;; EXAMPLES :
;; (world-after-key-event PAUSED-WORLD PAUSE-KEY-EVENT) => PAUSED-WORLD-TOGGLED
;; (world-after-key-event PAUSED-WORLD NONPAUSE-KEY-EVENT) => PAUSED-WORLD
;; (world-after-key-event UNPAUSED-WORLD NONPAUSE-KEY-EVENT) => UNPAUSED-WORLD

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
   "Unpaused world on pause key event toggles the world to paused state"))



;; Helper function for world-after-key-event
;; world-paused-toggled : WorldState -> WorldState
;; GIVEN : A world state
;; RETURNS : A world state after pause keyevent
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

;; Examples for world-to-scene, place-rectangle and place-velocity-inside-rect
;;              functions

(define TEXT2 (text (string-append OPEN-BRACKET
                                   (number->string INITIAL-RECT2-VX)
                                   COMMA
                                   (number->string INITIAL-RECT2-VY)
                                   CLOSE-BRACKET)
                    TEXT-HEIGHT
                    TEXT-COLOR))
(define STRING-INSIDE-RECT2 (overlay TEXT2 RECT))
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
                    TEXT-COLOR))
(define STRING-INSIDE-RECT1 (overlay TEXT1 RECT))
(define SCENE (place-image STRING-INSIDE-RECT1
                                         INITIAL-RECT1-X
                                         INITIAL-RECT1-Y
                                         SCENE1))


;; world-to-scene : WorldState -> Scene
;; GIVEN : A world state
;; RETURNS : A scene that potrays the given world state
;; EXAMPLE :
;; (world-to-scene PAUSED-WORLD) => SCENE

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
   "Parameters for world-to-scene are wrong"))



;; Helper function for world-to-scene
;; place-rectangle : Rect Scene -> Scene
;; GIVEN : A rect and scene
;; RETURNS : A scene like the given one with rect painted on it
;; EXAMPLE :
;; (place-rectangle RECT1 SCENE1) => SCENE


;; DESIGN STRATEGY : Use template for Rect on rt
(define (place-rectangle rt scene)
  (place-image (place-velocity-inside-rect (rect-vx rt)
                                           (rect-vy rt))
               (rect-x rt)
               (rect-y rt)
               scene))

;; TESTS :
(begin-for-test
  (check-equal?
   (place-rectangle RECT1 SCENE1)
   SCENE
   "Parameters for place-rectangle are wrong"))



;; Helper function for place-rectangle
;; place-velocity-inside-rect : Integer Integer -> Scene
;; GIVEN : 2 integers vx and vy, which represent the velocities of rectangle
;; RETURNS : A scene with (vx, vy) painted on it
;; EXAMPLE :
;; (place-velocity-inside-rect INITIAL-RECT1-VX INITIAL-RECT1-VY) => STRING-INSIDE-RECT1

;; DESIGN STRATEGY : Combine simpler functions
(define (place-velocity-inside-rect vx vy)
  (overlay (text (string-append
                  OPEN-BRACKET
                  (number->string vx)
                  COMMA
                  (number->string vy)
                  CLOSE-BRACKET)
                 TEXT-HEIGHT
                 TEXT-COLOR)
           RECT))

;; TESTS :
(begin-for-test
  (check-equal?
   (place-velocity-inside-rect INITIAL-RECT1-VX INITIAL-RECT1-VY)
   STRING-INSIDE-RECT1
   "Parameters for place-velocity-inside-rect are wrong"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start with screensaver
;(screensaver 0.5)