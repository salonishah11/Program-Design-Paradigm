;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver-4
;; In screensaver-4, a pen feature is added.
;; When a rectangle is selected, the "d" key drops a pen down.
;; When the pen is down, the rectangle records on the screen a dot marking
;; its center at each tick. The dot is displayed as a solid black circle of radius 1.
;; When a rectangle is selected, the "u" key lifts the pen up.
;; When the pen is up, the rectangle does not leave a track on the screen.
;; No marks are made during a drag.

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "04" "screensaver-4.rkt")

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-rects
 world-paused?
 rect-x 
 rect-y 
 rect-vx
 rect-vy
 rect-selected?
 rect-pen-down?
 world-after-mouse-event
 rect-after-mouse-event
 new-rectangle
 rect-after-key-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS :

;; Dimensions of canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
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

;; Dot required for trail
(define DOT (circle 1 "solid" "black"))

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
(define NEW-RECT-KEY-EVENT "n")
(define UP-ARROW-KEY-EVENT "up")
(define DOWN-ARROW-KEY-EVENT "down")
(define LEFT-ARROW-KEY-EVENT "left")
(define RIGHT-ARROW-KEY-EVENT "right")
(define PEN-DOWN-KEY-EVENT "d")
(define PEN-UP-KEY-EVENT "u")
(define NONPAUSE-KEY-EVENT "a")

;; Mouse events
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")
(define OTHER-EVENT "enter")

;; Booleans
(define TRUE #true)
(define FALSE #false)

;; Velocity increase/decrease factor
(define VELOCITY-FACTOR 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; Structure Point :
(define-struct point (x y))

;; CONSTRUCTOR TEMPLATE :
;; A point is (make-point NonNegInt NonNegInt)

;; INTERPRETATION :
;; (make-point NonNegInt NonNegInt) is a point where
;; x : is the x-ccordinate of the point
;; y : is the y-ccordinate of the point

;; DESTRUCTOR TEMPLATE :
;; point-fn : Point -> ??
#;(define (point-fn pt)
    (...
     (point-x pt)
     (point-y pt)
     ...))



;; Structure ListOfPoints (LOP) :
;; A ListOfPoints is one of
;; -- empty
;; -- (cons Point LOP)

;; INTERPRETATION :
;; empty : a sequence of Points with no elements
;; (cons Point LOP) : a sequence of Point's whose first
;;                    element is Point and whose other elements
;;                    are represented by LOP

;; DECONSTRUCTOR TEMPLATE :
;; lop-fn : LOP -> ??
#;(define (lop-fn lop)
    (cond
      [(empty? lop) ...]
      [else (...
             (point-fn (first lop))
             (lop-fn (rest lop)) ...)]))



;; Structure Rect :
(define-struct rect (x y vx vy mx-dist my-dist selected? pen-down? points))

;; CONSTRUCTOR TEMPLATE :
;; A rect is (make-rect NonNegInt NonNegInt Int Int Int Int Boolean Boolean LOP)

;; INTERPRETATION :
;; (make-rect NonNegInt NonNegInt Integer Integer Int Int Boolean Boolean LOP)
;; is a rect where
;; x, y : are the coordinates of the center of rectangle
;; vx, vy : are the velocities of rectangle in x and y direction respectively
;; mx-dist : is the distance from mx(mouse x-coordinate) to x at button-down event
;; my-dist : is the distance from my(mouse y-coordinate) to y at button-down event
;; selected? : is a Boolean indicating whether the rectangle is selected or not
;; pen-down? : is a Boolean indicating whether the pen is down on selected rectangle
;;             or not
;; points : are ListOfPoints of all the center points that the rectangle was at
;;          during pen down event

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
     (rect-pen-down? rt)
     (rect-points rt)
     ...))



;; Structure ListOfRectangles (LOR) :
;; A  ListOfRectangles (LOR) is either :
;; -- empty
;; -- (cons Rect LOR)

;; INTERPRETATION :
;; empty : a sequence of Rectangles with no elements
;; (cons Rect LOR) : a sequence of Rectangle's whose first
;;                   element is Rect and whose other elements
;;                   are represented by LOR

;; DECONSTRUCTOR TEMPLATE :
;; lor-fn : LOR -> ??
#;(define (lor-fn lor)
    (cond
      [(empty? lor) ...]
      [else (...
             (rect-fn (first lor))
             (lor-fn (rest lor)) ...)]))



;; Structure World :
(define-struct world (rects paused?))

;; CONSTRUCTOR TEMPLATE :
;; A world is (make-world LOR Boolean)

;; INTERPRETATION :
;; (make-world LOR LOP Boolean) is a world where
;; rects : is a ListOfRectangles of structure Rect
;; paused? : is a boolean which represents whether the simulation is paused
;;         or not

;; DESTRUCTOR TEMPLATE :
;; world-fn : World -> ??
#;(define (world-fn ws)
    (...
     (world-rects ws)
     (world-paused? ws)
     ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example for initial world

(define INITIAL-WORLD (make-world empty TRUE))

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
;; (initial-world 1) => INITIAL-WORLD

;; DESIGN STRATEGY : Use template for World 
(define (initial-world n)
  (make-world
   empty TRUE))

;; TESTS :
(begin-for-test
  (check-equal?
   (initial-world 1)
   INITIAL-WORLD
   "Initial world should be (make-world empty TRUE)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples for world-after-tick

(define LOR1 (cons
              (make-rect 200 100 -10 10 0 0 FALSE FALSE empty)
              (cons
               (make-rect 300 200 4 -6  10 10 TRUE FALSE empty)
               empty)))
(define PAUSED-WORLD (make-world LOR1 TRUE))
(define UNPAUSED-WORLD (make-world LOR1 FALSE))

(define LOR1-AFTER-NEW-RECT-ADDED (cons
                                   (make-rect 200 150 0 0 0 0 FALSE FALSE empty)
                                   (cons
                                    (make-rect 200 100 -10 10 0 0 FALSE FALSE empty)
                                    (cons
                                     (make-rect 300 200 4 -6  10 10 TRUE FALSE empty)
                                     empty))))
(define UNPAUSED-WORLD-AFTER-NEW-RECT-ADDED (make-world LOR1-AFTER-NEW-RECT-ADDED
                                                        FALSE))

(define LOR1-AFTER-1-TICK (cons
                           (make-rect 190 110 -10 10 0 0 FALSE FALSE empty)
                           (cons
                           (make-rect 300 200 4 -6 10 10 TRUE FALSE empty)
                           empty)))
(define WORLD-AFTER-1-TICK (make-world LOR1-AFTER-1-TICK FALSE))


;; Unselected Rectangles
(define RECT1 (make-rect 200 100 -12 20 0 0 FALSE TRUE empty))
(define RECT1-AFTER-1-TICK (make-rect 188 120 -12 20 0 0 FALSE TRUE
                                      (cons (make-point 200 100) empty)))

(define RECT2 (make-rect 200 200 8 -14 0 0 FALSE FALSE empty))
(define RECT2-AFTER-1-TICK (make-rect 208 186 8 -14 0 0 FALSE FALSE empty))

(define RECT1-PAST-TOP-LEFT-CORNER (make-rect 0 0 -12 -20 0 0 FALSE FALSE empty))
(define RECT1-BACK-INSIDE-CANVAS (make-rect 31 26 12 20 0 0 FALSE FALSE empty))

(define RECT2-PAST-BOTTOM-LEFT-CORNER (make-rect 0 300 -8 14 0 0 FALSE FALSE empty))
(define RECT2-BACK-INSIDE-CANVAS (make-rect 31 274 8 -14 0 0 FALSE FALSE empty))

(define RECT3-OUTSIDE-RIGHT-WALL (make-rect 390 200 12 -20 0 0 FALSE FALSE empty))
(define RECT3-BACK-INSIDE-CANVAS (make-rect 369 180 -12 -20 0 0 FALSE FALSE empty))

(define RECT4-OUTSIDE-LEFT-WALL (make-rect -10 150 -8 -14 0 0 FALSE FALSE empty))
(define RECT4-BACK-INSIDE-CANVAS (make-rect 31 136 8 -14 0 0 FALSE FALSE empty))

(define RECT5-OUTSIDE-TOP-WALL (make-rect 50 -10 12 -20 0 0 FALSE FALSE empty))
(define RECT5-BACK-INSIDE-CANVAS (make-rect 62 26 12 20 0 0 FALSE FALSE empty))

(define RECT6-OUTSIDE-BOTTOM-WALL (make-rect 70 310 8 14 0 0 FALSE FALSE empty))
(define RECT6-BACK-INSIDE-CANVAS (make-rect 78 274 8 -14 0 0 FALSE FALSE empty))

(define RECT7-PAST-TOP-RIGHT-CORNER (make-rect 400 0 12 -20 0 0 FALSE FALSE empty))
(define RECT7-BACK-INSIDE-CANVAS (make-rect 369 26 -12 20 0 0 FALSE FALSE empty))

(define RECT8-PAST-BOTTOM-RIGHT-CORNER (make-rect 400 300 8 14 0 0 FALSE FALSE empty))
(define RECT8-BACK-INSIDE-CANVAS (make-rect 369 274 -8 -14 0 0 FALSE FALSE empty))

;; Selected Rectangles
(define RECT1-SELECTED (make-rect 200 120 -12 20 20 12 TRUE FALSE empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; GIVEN : A world state
;; RETURNS: The world state that should follow the given world state
;;          after a tick
;; EXAMPLES :
;; (world-after-tick PAUSED-WORLD) => PAUSED-WORLD
;; (world-after-tick UNPAUSED-WORLD) => WORLD-AFTER-1-TICK

;; DESIGN STRATEGY : Use template for World on ws
(define (world-after-tick ws)
  (if (world-paused? ws)
      ws
      (make-world
       (list-of-rects-after-tick (world-rects ws))
       (world-paused? ws))))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-tick PAUSED-WORLD)
   PAUSED-WORLD
   "On passing a paused world to world-after-tick, the same world is
    returned")
  (check-equal?
   (world-after-tick UNPAUSED-WORLD)
   WORLD-AFTER-1-TICK
   "On passing an unpaused world to world-after-tick, the world at the 
    next tick is returned"))



;; list-of-rects-after-tick : LOR -> LOR
;; GIVEN : A ListOfRectangles
;; RETURNS : A ListOfRectangles at the next tick
;; EXAMPLE :
;; (list-of-rects-after-tick LOR1) => LOR1-AFTER-1-TICK

;; DESIGN STRATEGY : Use template for ListOfRectangles on lor
(define (list-of-rects-after-tick lor)
  (cond
    [(empty? lor) empty]
    [else
     (cons (rect-after-tick (first lor))
           (list-of-rects-after-tick (rest lor)))]))

;; TESTs :
(begin-for-test
  (check-equal?
   (list-of-rects-after-tick LOR1)
   LOR1-AFTER-1-TICK
   "If the LOR is not empty, rectangles at the next tick is
    retuned in the list"))



;; Helper function for list-of-rects-after-tick
;; rect-after-tick : Rect -> Rect
;; GIVEN : A rect
;; RETURNS : A rect based on whether the rectangle is selected or not and
;;           unselected rectangle is outside of canvas or not at the next tick
;; EXAMPLES :
;; (rect-after-tick RECT1) => RECT1-AFTER-1-TICK
;; (rect-after-tick RECT1-SELECTED) => RECT1-SELECTED

;; DESIGN STRATEGY : Use of template for Rect on rt
(define (rect-after-tick rt)
  (if (rect-selected? rt)
      rt
      (x-y-at-next-tick (rect-x rt) (rect-y rt)
                        (rect-vx rt) (rect-vy rt)
                        (rect-mx-dist rt) (rect-my-dist rt)
                        (rect-selected? rt)
                        (rect-pen-down? rt)
                        (rect-points rt))))

;; TESTS ;
(begin-for-test
  (check-equal?
   (rect-after-tick RECT1)
   RECT1-AFTER-1-TICK
   "If the rectangle is not selected, its state at the next tick
    is returned")
  (check-equal?
   (rect-after-tick RECT1-SELECTED)
   RECT1-SELECTED
   "If the rectangle is selected, same rectangle state is returned"))



;; Helper function for rect-after-tick
;; x-y-at-next-tick : NonNegInt NonNegInt Int Int Int Int Boolean Boolean LOP -> Rect
;; GIVEN : 2 non-negative integers x and y, 2 integers vx and vy, the distance of
;;         x and y from mx and my at mouse button-down event, Boolean indicating
;;         whether the rectangle is selected or not, Boolean indicating whether
;;         pen-down event on rectangle is turned on or not and LOP
;; RETURNS : A rect based on whether it will pass corners/walls at the
;;           next tick or not
;; EXAMPLES :
;; (x-y-at-next-tick  200 100 -12 20 0 0 FALSE TRUE empty) => RECT1-AFTER-1-TICK
;; (x-y-at-next-tick 200 200 8 -14 0 0 FALSE FALSE empty) => RECT2-AFTER-1-TICK

;; DESIGN STRATEGY : Divide into cases based on whether the pen is down on the
;;                   rectangle or not
(define (x-y-at-next-tick x y vx vy mx my sel pd lop)
  (if pd
      (x-at-next-tick x y vx vy
                      mx my sel pd (updated-lop x y lop))
      (x-at-next-tick x y vx vy
                      mx my sel pd lop)))

;; TESTS :
(begin-for-test
  (check-equal?
   (x-y-at-next-tick  200 100 -12 20 0 0 FALSE TRUE empty)
   RECT1-AFTER-1-TICK
   "At the next tick, rectangle will follow its path as per velocity,
    and since its pen-down? is TRUE, the current x & y will be added
    to its ListofPoints")
  (check-equal?
   (x-y-at-next-tick 200 200 8 -14 0 0 FALSE FALSE empty)
   RECT2-AFTER-1-TICK
   "At the next tick, rectangle will follow its path as per velocity,
    and since its pen-down? is FALSE, the ListofPoints will not be
    updated"))



;; Helper function for x-y-at-next-tick
;; updated-lop : NonNegInt NonNegInt ListOfPoints -> ListOfPoints
;; GIVEN : 2 non-negative integers x and y and ListOfPoints
;; RETURNS : An updated ListOfPoints, with given (x, y) added to the list
;; EXAMPLE :
;; (updated-lop 200 100 empty) => (cons (make-point 200 100) empty)

;; DESIGN STRATEGY : Use of template for Point
(define (updated-lop x y lop)
  (cons (make-point x y)
        lop))

;; TESTS :
(begin-for-test
  (check-equal?
   (updated-lop 200 100 empty)
   (cons (make-point 200 100) empty)
   "The given x and y will be added to the given ListOfPoints"))



;; Helper function for x-y-at-next-tick
;; x-at-next-tick : NonNegInt NonNegInt Int Int Int Int Boolean Boolean LOP -> Rect
;; GIVEN : 2 non-negative integers x and y, 2 integers vx and vy, the distance of
;;         x and y from mx and my at mouse button-down event, Boolean indicating
;;         whether the rectangle is selected or not, Boolean indicating whether
;;         pen-down event on rectangle is turned on or not and LOP
;; RETURNS : A rect, at the next tick
;; EXAMPLES :
;; (x-y-at-next-tick 400 0 12 -20 0 0 FALSE FALSE empty) => RECT7-BACK-INSIDE-CANVAS
;; (x-y-at-next-tick 0 0 -12 -20 0 0 FALSE FALSE empty) => RECT1-BACK-INSIDE-CANVAS
;; (x-y-at-next-tick 200 100 -12 20 0 0 FALSE TRUE empty) => RECT1-AFTER-1-TICK

;; DESIGN STRATEGY : Divide into cases based on whether x-coordinate of rectangle will
;;                   go outside which corner/wall
(define (x-at-next-tick x y vx vy mx my sel pd lop)
   (cond
    [(x-more-than-max-limit? x vx)
     (y-at-next-tick MAX-X-FOR-RECT y
                     (- vx) vy mx my sel pd lop)]
    [(x-less-than-min-limit? x vx)
     (y-at-next-tick MIN-X-FOR-RECT y
                     (- vx) vy mx my sel pd lop)]
    [else (y-at-next-tick (+ x vx) y
                          vx vy mx my sel pd lop)]))

;; TESTS :
(begin-for-test
  (check-equal?
   (x-y-at-next-tick 400 0 12 -20 0 0 FALSE FALSE empty)
   RECT7-BACK-INSIDE-CANVAS
   "At next tick x-coordinate should be back inside canvas")
  (check-equal?
   (x-y-at-next-tick 0 0 -12 -20 0 0 FALSE FALSE empty)
   RECT1-BACK-INSIDE-CANVAS
   "At next tick x-coordinate should be back inside canvas")
  (check-equal?
   (x-y-at-next-tick 200 100 -12 20 0 0 FALSE TRUE empty)
   RECT1-AFTER-1-TICK
   "At next tick x-coordinate will be as per its velocity vx"))



;; Helper for x-at-next-tick
;; x-more-than-max-limit? : NonNegInt Int -> Boolean
;; GIVEN : x coordinate and velocity vx
;; RETURNS : True iff x at next tick will cross the max
;;           limit for x
;; EXAMPLE :
;; (x-more-than-max-limit? 400 12) => TRUE

;; DESIGN STRATEGY : Combine simpler functions
(define (x-more-than-max-limit? x vx)
  (> (+ x vx) MAX-X-FOR-RECT))

;; TESTS :
(begin-for-test
  (check-equal?
   (x-more-than-max-limit? 400 12)
   TRUE
  "Since x+vx is more than 400, #true is returned"))



;; Helper for x-at-next-tick
;; x-less-than-min-limit? : NonNegInt Int -> Boolean
;; GIVEN : x coordinate and velocity vx
;; RETURNS : True iff x at next tick will cross the min
;;           limit for x
;; EXAMPLE :
;; (x-less-than-min-limit? 0 -12) => TRUE

;; DESIGN STRATEGY : Combine simpler functions
(define (x-less-than-min-limit? x vx)
  (< (+ x vx) MIN-X-FOR-RECT))

;; TESTS :
(begin-for-test
  (check-equal?
   (x-less-than-min-limit? 0 -12)
   TRUE
   "Since x+vx is less than 0, #true is returned"))



;; Helper function for rect-after-tick
;; y-at-next-tick : NonNegInt NonNegInt Int Int Int Int Boolean Boolean -> Rect
;; GIVEN : 2 non-negative integers x and y, 2 integers vx and vy, the distance of
;;         x and y from mx and my at mouse button-down event, Boolean indicating
;;         whether the rectangle is selected or not and Boolean indicating whether
;;         pen-down event on rectangle is turned on or not
;; RETURNS : A rect based on whether it's y-coordinate will pass corners/walls at the
;;           next tick or not
;; EXAMPLES :
;; (y-at-next-tick 31 0 12 -20 0 0 FALSE FALSE empty) => RECT1-BACK-INSIDE-CANVAS
;; (y-at-next-tick 31 300 8 14 0 0 FALSE FALSE empty) => RECT2-BACK-INSIDE-CANVAS
;; (y-at-next-tick 188 100 -12 20 0 0 FALSE TRUE (cons (make-point 200 100) empty))
;;   => RECT1-AFTER-1-TICK

;; DESIGN STRATEGY : Divide into cases based on whether y-coordinate of rectangle will
;;                   go outside which corner/wall
(define (y-at-next-tick x y vx vy mx my sel pd lop)
  (cond
    [(y-less-than-min-limit? y vy)
     (make-rect x MIN-Y-FOR-RECT
                vx (- vy) mx my sel pd lop)]
    [(y-more-than-max-limit? y vy)
     (make-rect x MAX-Y-FOR-RECT
                vx (- vy) mx my sel pd lop)]
    [else (make-rect x (+ y vy)
                     vx vy mx my sel pd lop)]))

;; TESTS :
(begin-for-test
  (check-equal?
   (y-at-next-tick 31 0 12 -20 0 0 FALSE FALSE empty)
   RECT1-BACK-INSIDE-CANVAS
   "At next tick y-coordinate should be back inside canvas")
  (check-equal?
   (y-at-next-tick 31 300 8 14 0 0 FALSE FALSE empty)
   RECT2-BACK-INSIDE-CANVAS
   "At next tick y-coordinate should be back inside canvas")
  (check-equal?
   (y-at-next-tick 188 100 -12 20 0 0 FALSE TRUE
                   (cons (make-point 200 100) empty))
   RECT1-AFTER-1-TICK
   "At next tick y-coordinate will be as per its velocity vx"))



;; Helper for y-at-next-tick
;; y-less-than-min-limit? : NonNegInt Int -> Boolean
;; GIVEN : x coordinate and velocity vx
;; RETURNS : True iff y at next tick will cross the max
;;           limit for y
;; EXAMPLE :
;; (y-less-than-min-limit? 0 -14) => TRUE

;; DESIGN STRATEGY : Combine simpler functions
(define (y-less-than-min-limit? y vy)
  (< (+ y vy) MIN-Y-FOR-RECT))

;; TESTS :
(begin-for-test
  (check-equal?
   (y-less-than-min-limit? 0 -14)
   TRUE
   "Since y+vy is less than 0, #true is returned"))



;; Helper for y-at-next-tick
;; y-more-than-max-limit? : NonNegInt Int -> Boolean
;; GIVEN : x coordinate and velocity vx
;; RETURNS : True iff y at next tick will cross the max
;;           limit for y
;; EXAMPLE :
;; (y-more-than-max-limit? 300 14) => TRUE

;; DESIGN STRATEGY : Combine simpler functions
(define (y-more-than-max-limit? y vy)
  (> (+ y vy) MAX-Y-FOR-RECT))

;; TESTS :
(begin-for-test
  (check-equal?
   (y-more-than-max-limit? 300 14)
   TRUE
   "Since y+vy is more than 300, #true is returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples for Key Events

(define LOR1-AFTER-UP-ARROW-KEY-EVENT
  (cons
   (make-rect 200 100 -10 10 0 0 FALSE FALSE empty)
   (cons
    (make-rect 300 200 4 -8  10 10 TRUE FALSE empty)
    empty)))

(define LOR1-AFTER-DOWN-ARROW-KEY-EVENT
  (cons
   (make-rect 200 100 -10 10 0 0 FALSE FALSE empty)
   (cons
    (make-rect 300 200 4 -4  10 10 TRUE FALSE empty)
    empty)))

(define LOR1-AFTER-RIGHT-ARROW-KEY-EVENT
  (cons
   (make-rect 200 100 -10 10 0 0 FALSE FALSE empty)
   (cons
    (make-rect 300 200 6 -4  10 10 TRUE FALSE empty)
    empty)))

(define WORLD-AFTER-UP-ARROW-KEY-EVENT
  (make-world LOR1-AFTER-UP-ARROW-KEY-EVENT
              FALSE))

(define RECT1-AFTER-UP-ARROW-KEY-EVENT
  (make-rect 200 120 -12 18 20 12 TRUE FALSE empty))

(define RECT1-AFTER-DOWN-ARROW-KEY-EVENT
  (make-rect 200 120 -12 22 20 12 TRUE FALSE empty))

(define RECT1-AFTER-LEFT-ARROW-KEY-EVENT
  (make-rect 200 120 -14 20 20 12 TRUE FALSE empty))

(define RECT1-AFTER-RIGHT-ARROW-KEY-EVENT
  (make-rect 200 120 -10 20 20 12 TRUE FALSE empty))

(define RECT1-AFTER-PEN-DOWN-KEY-EVENT
  (make-rect 200 120 -12 20 20 12 TRUE TRUE empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN : A world state and key event
;; RETURNS: The WorldState that should follow the given worldstate after
;;          the given keyevent
;; EXAMPLES :
;; (world-after-key-event UNPAUSED-WORLD PAUSE-KEY-EVENT) => PAUSED-WORLD
;; (world-after-key-event UNPAUSED-WORLD NEW-RECT-KEY-EVENT) =>
;;   UNPAUSED-WORLD-AFTER-NEW-RECT-ADDED
;; (world-after-key-event UNPAUSED-WORLD UP-ARROW-KEY-EVENT) =>
;;   WORLD-AFTER-UP-ARROW-KEY-EVENT

;; DESIGN STRATEGY : Divide into cases on KeyEvent ke
(define (world-after-key-event ws ke)
  (cond
    [(string=? ke PAUSE-KEY-EVENT)
     (world-after-pause-key-event ws)]
    [(string=? ke NEW-RECT-KEY-EVENT)
     (world-after-new-rect-key-event ws)]
    [else (world-after-arrow-pen-key-event ws ke)]))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD PAUSE-KEY-EVENT)
   PAUSED-WORLD
   "Unpaused world after pause key event gets paused")
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD NEW-RECT-KEY-EVENT)
   UNPAUSED-WORLD-AFTER-NEW-RECT-ADDED
   "On 'n' key event, a new rectangle is added to the ListOfRectangles,
    and the new  ListOfRectangles is returned in world-rects")
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD UP-ARROW-KEY-EVENT)
   WORLD-AFTER-UP-ARROW-KEY-EVENT
   "On up-arrow key event, the updated ListOfRectangles is passed
    to world-rects"))



;; Helper function for world-after-key-event
;; world-after-pause-key-event : WorldState -> WorldState
;; GIVEN : A world state
;; RETURNS : A world state after it is world-paused? state is toggled
;; EXAMPLE :
;; (world-after-pause-key-event UNPAUSED-WORLD) => PAUSED-WORLD

;; DESIGN STRATEGY : Use template for World on ws
(define (world-after-pause-key-event ws)
  (make-world
   (world-rects ws)
   (not (world-paused? ws))))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-pause-key-event UNPAUSED-WORLD)
   PAUSED-WORLD
   "Unpaused world after pause key event gets paused"))



;; Helper function for world-after-key-event
;; world-after-new-rect-key-event : WorldState -> WorldState
;; GIVEN : A world state
;; RETURNS : A world state with new rectangle added in the ListOfRectangles 
;; EXAMPLE :
;; (world-after-new-rect-key-event UNPAUSED-WORLD) =>
;;   UNPAUSED-WORLD-AFTER-NEW-RECT-ADDED

;; DESIGN STRATEGY : Use of template for World on ws
(define (world-after-new-rect-key-event ws)
  (make-world
   (cons (new-rectangle HALF-CANVAS-WIDTH
                        HALF-CANVAS-HEIGHT
                        0 0)
         (world-rects ws))
   (world-paused? ws)))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-new-rect-key-event UNPAUSED-WORLD)
    UNPAUSED-WORLD-AFTER-NEW-RECT-ADDED
    "On 'n' key event, a new rectangle is added to the ListOfRectangles"))



;; new-rectangle : NonNegInt NonNegInt Int Int -> Rect
;; GIVEN : 2 non-negative integers x and y, 2 integers vx and vy
;; RETURNS : An unselected rectangle centered at (x,y), which will travel with
;;           velocity (vx, vy) 
;; EXAMPLE :
;; (new-rectangle 200 150 0 0) => (make-rect 200 150 0 0 0 0 FALSE FALSE empty)

;; DESIGN STRATEGY : Use template for Rect
(define (new-rectangle x y vx vy)
  (make-rect x y
             vx vy
             0 0
             FALSE
             FALSE
             empty))

;; TESTS :
(begin-for-test
  (check-equal?
   (new-rectangle 200 150 0 0)
   (make-rect 200 150 0 0 0 0 FALSE FALSE empty)
   "New rectangle creates a new rect with given x & y coordinates, vx & vy
    velocities, 0 and 0 mx and my distances and boolean #false"))



;; Helper function for world-after-key-event
;; world-after-arrow-pen-key-event : WorldState KeyEvent -> WorldState
;; GIVEN : A world state and an key event
;; WHERE : Key event is one of up,down, left, right, "d" or "u"
;; RETURNS : A world state after the given key event
;; EXAMPLE :
;; (world-after-arrow-pen-key-event UNPAUSED-WORLD UP-ARROW-KEY-EVENT) =>
;;    WORLD-AFTER-UP-ARROW-KEY-EVENT

;; DESIGN STRATEGY : Use template for World on ws
(define (world-after-arrow-pen-key-event ws ke)
  (make-world
   (list-of-rects-after-key-event (world-rects ws) ke)
   (world-paused? ws)))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-arrow-pen-key-event UNPAUSED-WORLD UP-ARROW-KEY-EVENT)
    WORLD-AFTER-UP-ARROW-KEY-EVENT
    "The world after up-arrow key event is returned"))



;; Helper function for world-after-arrow-pen-key-event
;; list-of-rects-after-key-event : ListOfRectangles KeyEvent -> ListOfRectangles
;; GIVEN : A ListOfRectangles and an key event
;; RETURNS : A ListOfRectangles after the given key event performed on the
;;           selected rectangles
;; EXAMPLE :
;; (list-of-rects-after-key-event LOR1 DOWN-ARROW-KEY-EVENT) =>
;;   LOR1-AFTER-DOWN-ARROW-KEY-EVENT

;; DESIGN STRATEGY : Use of template for ListOfRectangles on lor
(define (list-of-rects-after-key-event lor ke)
  (cond
    [(empty? lor) empty]
    [else
     (cons (rect-after-key-event (first lor) ke)
           (list-of-rects-after-key-event (rest lor) ke))]))

;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-rects-after-key-event LOR1 DOWN-ARROW-KEY-EVENT)
   LOR1-AFTER-DOWN-ARROW-KEY-EVENT
   "Updated ListOfRectangles is returned after down-arrow key event
    on selected rectangles"))



;; rect-after-key-event : Rect KeyEvent -> Rect
;; GIVEN : A rect and an key event
;; RETURNS : A rect with the given key event performed on it
;; EXAMPLES :
;; (rect-after-key-event RECT1-SELECTED UP-ARROW-KEY-EVENT) =>
;;   RECT1-AFTER-UP-ARROW-KEY-EVENT
;; (rect-after-key-event RECT1-SELECTED DOWN-ARROW-KEY-EVENT) =>
;;   RECT1-AFTER-DOWN-ARROW-KEY-EVENT

;; DESIGN STRATEGY : Divide into cases on the type of arrow and pen key
;;                   events on selected rectangles
(define (rect-after-key-event rt ke)
  (if (rect-selected? rt)
      (cond
        [(string=? ke UP-ARROW-KEY-EVENT)
         (rect-after-decrease-in-y-velocity rt)]
        [(string=? ke DOWN-ARROW-KEY-EVENT)
         (rect-after-increase-in-y-velocity rt)]
        [(string=? ke LEFT-ARROW-KEY-EVENT)
         (rect-after-decrease-in-x-velocity rt)]
        [(string=? ke RIGHT-ARROW-KEY-EVENT)
         (rect-after-increase-in-x-velocity rt)]
        [(string=? ke PEN-DOWN-KEY-EVENT)
         (rect-after-pen-down-key-event rt)]
        [(string=? ke PEN-UP-KEY-EVENT)
         (rect-after-pen-up-key-event rt)]
        [else rt])
      rt))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-key-event RECT1-SELECTED UP-ARROW-KEY-EVENT)
   RECT1-AFTER-UP-ARROW-KEY-EVENT
   "Velocity of the rectangle decreases in y-direction on up-arrow
    key event")
  (check-equal?
   (rect-after-key-event RECT1-SELECTED DOWN-ARROW-KEY-EVENT)
   RECT1-AFTER-DOWN-ARROW-KEY-EVENT
   "Velocity of the rectangle increases in y-direction on down-arrow
    key event")
  (check-equal?
   (rect-after-key-event RECT1-SELECTED LEFT-ARROW-KEY-EVENT)
   RECT1-AFTER-LEFT-ARROW-KEY-EVENT
   "Velocity of the rectangle decreases in x-direction on left-arrow
    key event")
  (check-equal?
   (rect-after-key-event RECT1-SELECTED RIGHT-ARROW-KEY-EVENT)
   RECT1-AFTER-RIGHT-ARROW-KEY-EVENT
   "Velocity of the rectangle increases in x-direction on right-arrow
    key event")
  (check-equal?
   (rect-after-key-event RECT1-SELECTED NONPAUSE-KEY-EVENT)
   RECT1-SELECTED
   "Rectangle is unaffected on any other key event")
  (check-equal?
   (rect-after-key-event RECT1-SELECTED PEN-DOWN-KEY-EVENT)
   RECT1-AFTER-PEN-DOWN-KEY-EVENT
   "Rectangle on pen-down key event, pen-down? field of rectangle
    is changed to #true")
  (check-equal?
   (rect-after-key-event RECT1-AFTER-PEN-DOWN-KEY-EVENT
                         PEN-UP-KEY-EVENT)
   RECT1-SELECTED
    "Rectangle on pen-up key event, pen-down? field of rectangle
    is changed to #false")
  (check-equal?
   (rect-after-key-event RECT1 RIGHT-ARROW-KEY-EVENT)
   RECT1
   "If rectangle is not selected, its given state is returned"))



;; Helper function for rect-after-key-event
;; rect-after-decrease-in-y-velocity : Rect -> Rect
;; GIVEN : A rect
;; WHERE : The rectangle is selected
;; RETURNS : A rect, after its velocity has been decreased in y-direction
;; EXAMPLE :
;; (rect-after-decrease-in-y-velocity  RECT1-SELECTED) => 
;;   RECT1-AFTER-UP-ARROW-KEY-EVENT

;; DESIGN STRATEGY : Use of template for Rect on rt
(define (rect-after-decrease-in-y-velocity rt)
  (make-rect (rect-x rt)
             (rect-y rt)
             (rect-vx rt)
             (- (rect-vy rt) VELOCITY-FACTOR)
             (rect-mx-dist rt)
             (rect-my-dist rt)
             (rect-selected? rt)
             (rect-pen-down? rt)
             (rect-points rt)))

;; TESTS :
(begin-for-test
  (rect-after-decrease-in-y-velocity  RECT1-SELECTED)
   RECT1-AFTER-UP-ARROW-KEY-EVENT
   "The y-velocity of the rectangle will be decreased by 2 piels/tick")



;; Helper function for rect-after-key-event
;; rect-after-increase-in-y-velocity : Rect -> Rect
;; GIVEN : A rect
;; WHERE : The rectangle is selected
;; RETURNS : A rect, after its velocity has been increased in y-direction
;; EXAMPLE :
;; (rect-after-increase-in-y-velocity  RECT1-SELECTED) =>
;;   RECT1-AFTER-DOWN-ARROW-KEY-EVENT

;; DESIGN STRATEGY : Use of template for Rect on rt
(define (rect-after-increase-in-y-velocity rt)
  (make-rect (rect-x rt)
             (rect-y rt)
             (rect-vx rt)
             (+ (rect-vy rt) VELOCITY-FACTOR)
             (rect-mx-dist rt)
             (rect-my-dist rt)
             (rect-selected? rt)
             (rect-pen-down? rt)
             (rect-points rt)))

;; TESTS :
(begin-for-test
  (rect-after-increase-in-y-velocity  RECT1-SELECTED)
   RECT1-AFTER-DOWN-ARROW-KEY-EVENT
   "The y-velocity of the rectangle will be increased by 2 piels/tick")



;; Helper function for rect-after-key-event
;; rect-after-decrease-in-x-velocity : Rect -> Rect
;; GIVEN : A rect
;; WHERE : The rectangle is selected
;; RETURNS : A rect, after its velocity has been decreased in x-direction
;; EXAMPLE :
;; (rect-after-decrease-in-x-velocity  RECT1-SELECTED) =>
;;   RECT1-AFTER-LEFT-ARROW-KEY-EVENT

;; DESIGN STRATEGY : Use of template for Rect on rt
(define (rect-after-decrease-in-x-velocity rt)
  (make-rect (rect-x rt)
             (rect-y rt)
             (- (rect-vx rt) VELOCITY-FACTOR)
             (rect-vy rt)
             (rect-mx-dist rt)
             (rect-my-dist rt)
             (rect-selected? rt)
             (rect-pen-down? rt)
             (rect-points rt)))

;; TESTS :
(begin-for-test
  (rect-after-decrease-in-x-velocity  RECT1-SELECTED)
   RECT1-AFTER-LEFT-ARROW-KEY-EVENT
   "The x-velocity of the rectangle will be decreased by 2 piels/tick")



;; Helper function for rect-after-key-event
;; rect-after-increase-in-x-velocity : Rect -> Rect
;; GIVEN : A rect
;; WHERE : The rectangle is selected
;; RETURNS : A rect, after its velocity has been increased in x-direction
;; EXAMPLE :
;; (rect-after-increase-in-x-velocity  RECT1-SELECTED) =>
;;   RECT1-AFTER-RIGHT-ARROW-KEY-EVENT

;; DESIGN STRATEGY : Use of template for Rect on rt
(define (rect-after-increase-in-x-velocity rt)
  (make-rect (rect-x rt)
             (rect-y rt)
             (+ (rect-vx rt) VELOCITY-FACTOR)
             (rect-vy rt)
             (rect-mx-dist rt)
             (rect-my-dist rt)
             (rect-selected? rt)
             (rect-pen-down? rt)
             (rect-points rt)))

;; TESTS :
(begin-for-test
  (rect-after-increase-in-x-velocity  RECT1-SELECTED)
   RECT1-AFTER-RIGHT-ARROW-KEY-EVENT
   "The x-velocity of the rectangle will be increased by 2 piels/tick")



;; Helper function for rect-after-key-event
;; rect-after-pen-down-key-event : Rect -> Rect
;; GIVEN : A rect
;; WHERE : The rectangle is selected
;; RETURNS : A rect, after pen down event on it
;; EXAMPLE :
;; (rect-after-pen-down-key-event RECT1-SELECTED) =>
;;   RECT1-AFTER-PEN-DOWN-KEY-EVENT

;; DESIGN STRATEGY : Use of template for Rect on rt
(define (rect-after-pen-down-key-event rt)
  (make-rect (rect-x rt)
             (rect-y rt)
             (rect-vx rt)
             (rect-vy rt)
             (rect-mx-dist rt)
             (rect-my-dist rt)
             (rect-selected? rt)
             TRUE
             (rect-points rt)))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-pen-down-key-event RECT1-SELECTED)
   RECT1-AFTER-PEN-DOWN-KEY-EVENT
   "On pen-down event on selected rectangle, the pen-down? field
    is changed to #true"))



;; Helper function for rect-after-key-event
;; rect-after-pen-up-key-event : Rect -> Rect
;; GIVEN : A rect
;; WHERE : The rectangle is selected
;; RETURNS : A rect, after pen up event on it
;; EXAMPLE :
;; (rect-after-pen-up-key-event RECT1-AFTER-PEN-DOWN-KEY-EVENT) =>
;;   RECT1-SELECTED

;; DESIGN STRATEGY : Use of template for Rect on rt
(define (rect-after-pen-up-key-event rt)
  (make-rect (rect-x rt)
             (rect-y rt)
             (rect-vx rt)
             (rect-vy rt)
             (rect-mx-dist rt)
             (rect-my-dist rt)
             (rect-selected? rt)
             FALSE
             (rect-points rt)))

;; TESTS :
(begin-for-test
  (check-equal?
   (rect-after-pen-up-key-event RECT1-AFTER-PEN-DOWN-KEY-EVENT)
   RECT1-SELECTED
   "On pen-up event on selected rectangle, the pen-down? field
    is changed to #false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples for Mouse Events

(define LOR1-AFTER-BUTTON-DOWN (cons
                                (make-rect 200 100 -10 10 10 10 TRUE FALSE empty)
                                (cons
                                 (make-rect 300 200 4 -6  10 10 TRUE FALSE empty)
                                 empty)))
                                 
(define WORLD-AFTER-BUTTON-DOWN 
  (make-world LOR1-AFTER-BUTTON-DOWN FALSE))

;; Selected Rectangles
(define RECT1-BEFORE-BUTTON-DOWN (make-rect 200 100 -12 20 0 0 FALSE FALSE empty))
(define RECT1-AFTER-BUTTON-DOWN (make-rect 200 100 -12 20 10 10 TRUE FALSE empty))

(define RECT2-BEFORE-DRAG (make-rect 200 200 23 -14 10 10 TRUE FALSE empty))
(define RECT2-AFTER-DRAG (make-rect 225 230 23 -14 10 10 TRUE FALSE empty))

(define RECT2-BEFORE-BUTTON-UP (make-rect 225 230 23 -14 10 10 TRUE FALSE empty))
(define RECT2-AFTER-BUTTON-UP (make-rect 225 230 23 -14 10 10 FALSE FALSE empty))

;(define RECT1-AFTER-OTHER-EVENT (make-rect 200 100 -12 20 0 0 FALSE FALSE empty))

(define RECT1-SELECTED-AFTER-1-TICK (make-rect 188 120 -12 20 10 10 TRUE FALSE empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : WorldState Int Int MouseEvent -> WorldState
;; GIVEN : A World, the x- and y-coordinates of a mouse event, and the
;;         mouse event
;; RETURNS : The world that should follow the given world after the given mouse
;;           event
;; EXAMPLE :
;; (world-after-mouse-event UNPAUSED-WORLD 210 110 BUTTON-DOWN-EVENT) => 
;;   WORLD-AFTER-BUTTON-DOWN

;; DESIGN STRATEGY : Use template for World on ws
(define (world-after-mouse-event ws mx my me)
  (make-world
    (list-of-rects-after-mouse-event (world-rects ws) mx my me)
    (world-paused? ws)))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-mouse-event UNPAUSED-WORLD 210 110 BUTTON-DOWN-EVENT)
   WORLD-AFTER-BUTTON-DOWN
   "After button-down, the selected rectangle in ListOfRectangles will
    be updated"))



;; Helper function for world-after-mouse-event
;; list-of-rects-after-mouse-event : ListOfRectangles  Int Int MouseEvent ->
;;                                   ListOfRectangles
;; GIVEN : A ListOfRectangles, mouse coordinates mx and my and a mouse event
;; RETURNS : A ListOfRectangles, after the given mouse event has been performed
;;           on the rectangles
;; EXAMPLE :
;; (list-of-rects-after-mouse-event LOR1 210 110 BUTTON-DOWN-EVENT) =>
;;   LOR1-AFTER-BUTTON-DOWN

;; DESIGN STRATEGY : Use of template ListOfRectangles on lor
(define (list-of-rects-after-mouse-event lor mx my me)
  (cond
    [(empty? lor) empty]
    [else
     (cons (rect-after-mouse-event (first lor) mx my me)
           (list-of-rects-after-mouse-event (rest lor) mx my me))]))

;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-rects-after-mouse-event LOR1 210 110 BUTTON-DOWN-EVENT)
   LOR1-AFTER-BUTTON-DOWN)
  "After button-down on selected rectangles, updated ListOfRectangles
   is returned")



;; rect-after-mouse-event : Rect Int Int MouseEvent -> Rect
;; GIVEN : A rect, mouse coordinates mx and my, and mouse event
;; RETURNS : A rect with the mouse event performed on it
;; EXAMPLES :
;; (rect-after-mouse-event RECT1-BEFORE-BUTTON-DOWN 210 110 BUTTON-DOWN-EVENT)
;;   => RECT1-AFTER-BUTTON-DOWN
;; (rect-after-mouse-event RECT2-BEFORE-DRAG 235 240 DRAG-EVENT) =>
;;   RECT2-AFTER-DRAG

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
   (rect-after-mouse-event RECT1 235 240 OTHER-EVENT)
   RECT1
   "On any other event, the rectangle should remain as before"))



;; Helper Function for rect-after-mouse-event
;; rect-after-button-down : Rect Int Int -> Rect
;; GIVEN : A rect and mouse coordinates mx and my
;; RETURNS : A rect after button-down event
;; EXAMPLES :
;; (rect-after-button-down RECT1-BEFORE-BUTTON-DOWN 210 110) =>
;;   RECT1-AFTER-BUTTON-DOWN
;; (rect-after-button-down RECT1-BEFORE-BUTTON-DOWN 250 125) =>
;;   RECT1-BEFORE-BUTTON-DOWN

;; DESIGN STRATEGY : Use template for Rect on rt
(define (rect-after-button-down rt mx my)
  (if (inside-rect? rt mx my)
      (make-rect (rect-x rt) (rect-y rt)
                 (rect-vx rt) (rect-vy rt)
                 (- mx (rect-x rt))
                 (- my (rect-y rt))
                 TRUE
                 (rect-pen-down? rt)
                 (rect-points rt))
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
;; (rect-after-drag RECT1-BEFORE-BUTTON-DOWN 210 100) =>
;;   RECT1-BEFORE-BUTTON-DOWN

;; DESIGN STRATEGY : Use template for Rect on rt
(define (rect-after-drag rt mx my)
  (if (and (rect-selected? rt)
           (rect-inside-canvas-at-drag? rt mx my))
      (make-rect (- mx (rect-mx-dist rt))
                 (- my (rect-my-dist rt))
                 (rect-vx rt)
                 (rect-vy rt)
                 (rect-mx-dist rt)
                 (rect-my-dist rt)
                 TRUE
                 (rect-pen-down? rt)
                 (rect-points rt))
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
;; RETURNS : True iff the rectangle, during drag is inside the canvas
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
;; (rect-after-button-up RECT2-BEFORE-BUTTON-UP 235 240) =>
;;   RECT2-AFTER-BUTTON-UP
;; (rect-after-button-up RECT2-AFTER-BUTTON-UP 235 240) =>
;;   RECT2-AFTER-BUTTON-UP

;; DESIGN STRATEGY : Use template for Rect on rt
(define (rect-after-button-up rt mx my)
  (if (rect-selected? rt)
      (make-rect (rect-x rt) (rect-y rt)
                 (rect-vx rt) (rect-vy rt)
                 (rect-mx-dist rt) (rect-my-dist rt)
                 FALSE
                 (rect-pen-down? rt)
                 (rect-points rt))
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

;; Examples for world-to-scene

(define TEXT2 (text (string-append OPEN-BRACKET
                                   (number->string -12)
                                   COMMA
                                   (number->string 20)
                                   CLOSE-BRACKET)
                    TEXT-HEIGHT
                    TEXT-COLOR-BLUE))
(define STRING-INSIDE-RECT2 (overlay TEXT2 BLUE-RECT))
(define SCENE1 (place-image STRING-INSIDE-RECT2
                            200
                            200
                            EMPTY-CANVAS))

(define TEXT1 (text (string-append OPEN-BRACKET
                                   (number->string -12)
                                   COMMA
                                   (number->string 20)
                                   CLOSE-BRACKET)
                    TEXT-HEIGHT
                    TEXT-COLOR-BLUE))
(define STRING-INSIDE-RECT1 (overlay TEXT1 BLUE-RECT))
(define SCENE (place-image STRING-INSIDE-RECT1
                           200
                           100
                           SCENE1))

(define TEXT3 (text (string-append OPEN-BRACKET
                                   (number->string -12)
                                   COMMA
                                   (number->string 20)
                                   CLOSE-BRACKET)
                    TEXT-HEIGHT
                    TEXT-COLOR-RED))
(define RED-RECT-WITH-CIRCLE (overlay/xy RED-RECT 35 30 RED-CIRCLE))
(define STRING-INSIDE-RECT1-SELECTED (overlay TEXT3 RED-RECT-WITH-CIRCLE))
(define SCENE3 (place-image STRING-INSIDE-RECT1-SELECTED
                            200
                            100
                            SCENE1))

(define LOR4 (cons
              (make-rect 200 100 -12 20 10 10 TRUE FALSE empty) empty))
(define WORLD4 (make-world LOR4 FALSE))
(define TEXT4 (text (string-append OPEN-BRACKET
                                   (number->string -12)
                                   COMMA
                                   (number->string 20)
                                   CLOSE-BRACKET)
                    TEXT-HEIGHT
                    TEXT-COLOR-RED))
(define STRING-INSIDE-LOR4 (overlay TEXT4 RED-RECT-WITH-CIRCLE))
(define SCENE4 (place-image STRING-INSIDE-RECT1-SELECTED
                            200
                            100
                            EMPTY-CANVAS))

(define EMPTY-LOR empty)

(define EMPTY-LOP empty)

(define LOP1 (cons (make-point 200 100) empty))

(define SCENE-WITH-DOT-TRAIL
  (place-image DOT 200 100 SCENE1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : WorldState -> Scene
;; GIVEN : A world state
;; RETURNS : A scene that potrays the world state
;; EXAMPLE :
;; (world-to-scene WORLD4) => SCENE4

;; DESIGN STRATEGY : Use template for World on ws
(define (world-to-scene ws)
  (place-list-of-rects
   (world-rects ws)))

;; TESTS :
(begin-for-test
  (check-equal?
   (world-to-scene WORLD4)
   SCENE4
  "Output Scene as per input world state"))



;; Helper function for world-to-scene
;; place-list-of-rects : ListOfRectangles -> Scene
;; GIVEN : A ListOfRectangles
;; RETURNS : A scene with all the rectangles in ListOfRectangles painted on it
;; EXAMPLES :
;; (place-list-of-rects EMPTY-LOR) => EMPTY-CANVAS
;; (place-list-of-rects LOR4) => SCENE4

;; DESIGN STRATEGY : Use of template for ListOfRectangles on lor
(define (place-list-of-rects lor)
  (cond
    [(empty? lor) EMPTY-CANVAS]
    [else
     (place-rectangle (first lor)
           (place-list-of-rects (rest lor)))]))

;; TESTS :
(begin-for-test
  (check-equal?
   (place-list-of-rects EMPTY-LOR)
   EMPTY-CANVAS
   "Empty ListOfRectangles creates empty canvas")
  (check-equal?
   (place-list-of-rects LOR4)
   SCENE4
   "Output Scene as per input ListOfRectangles"))



;; Helper function for place-list-of-rects
;; place-rectangle : Rect Scene -> Scene
;; GIVEN : A rect and scene
;; RETURNS : A scene like the given one with rectangle painted on it
;; EXAMPLES :
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
               (scene-with-dot-trail (rect-points rt) scene)))

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
;; scene-with-dot-trail : ListOfRectangles Scene -> Scene
;; GIVEN : ListOfRectangles and a scene
;; RETURNS : A scene with the dot trail of rectangle painted on it
;; EXAMPLES :
;; (scene-with-dot-trail EMPTY-LOP SCENE1) => SCENE1
;; (scene-with-dot-trail LOP1 SCENE1) => SCENE-WITH-DOT-TRAIL

;; DESIGN STRATEGY : Use of template for ListOfPoints on lop
(define (scene-with-dot-trail lop scene)
  (cond
    [(empty? lop) scene]
    [else
     (place-image DOT
                  (point-x (first lop))
                  (point-y (first lop))
                  (scene-with-dot-trail (rest lop) scene))]))

;; TESTS :
(begin-for-test
  (check-equal?
   (scene-with-dot-trail EMPTY-LOP SCENE1)
   SCENE1
   "If the LOP is empty, given scene is returned")
  (check-equal?
   (scene-with-dot-trail LOP1 SCENE1)
   SCENE-WITH-DOT-TRAIL
   "If the LOP is not empty, given points are painted as
    dots on the given scene"))



;; Helper function for place-rectangle
;; place-velocity-inside-rect : Rect Integer Integer Boolean -> Scene
;; GIVEN : A rect, 2 integers vx and vy, which represent the velocities of rectangle,
;;         and a Boolean indicating whether the rectangle is selected or not
;; RETURNS : A scene with (vx, vy), and red circle painted on rectangle based on
;;           whether rectangle is selected or not
;; EXAMPLES :
;; (place-velocity-inside-rect RECT1 -12 20 FALSE) =>
;;   STRING-INSIDE-RECT1
;; (place-velocity-inside-rect RECT1-AFTER-BUTTON-DOWN -12 20 TRUE) =>
;;   STRING-INSIDE-RECT1-SELECTED

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
   (place-velocity-inside-rect RECT1 -12 20 FALSE)
   STRING-INSIDE-RECT1
   "Parameters for place-velocity-inside-rect are wrong")
  (check-equal?
   (place-velocity-inside-rect RECT1-AFTER-BUTTON-DOWN
                               -12 20 TRUE)
   STRING-INSIDE-RECT1-SELECTED
   "Parameters for place-velocity-inside-rect are wrong"))



;; Helper function for place-velocity-inside-rect
;; red-rect-with-circle : Rect -> Scene
;; GIVEN : A rect
;; RETURNS : A red circle drawn on rectangle at (mx, my)
;; EXAMPLE :
;; (red-rect-with-circle RECT1-AFTER-BUTTON-DOWN) =>
;;   RED-RECT-WITH-CIRCLE

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
   RED-RECT-WITH-CIRCLE
   "Selected rectangle is displayed with a red circle whose center is
    given by mouse coordinates mx & my"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Start with Screensaver
;(screensaver 0.5)