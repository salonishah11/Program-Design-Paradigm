;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; trees.rkt
;;; This program implements a system of a graphical interface for trees.
;;; It allows you to create and manipulate trees on a canvas.

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(check-location "06" "trees.rkt")

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

;;; Dimensions of Canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;;; Canvas
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;; Properties of Trees
(define RADIUS 10)
(define NODE-COLOR "green")
(define OFF-TO-SIBLING (* 3 RADIUS))
(define OFF-TO-PARENT (* 3 RADIUS))

;;; Trees
(define UNSEL-NODE (circle RADIUS "outline" NODE-COLOR))
(define SEL-NODE (circle RADIUS "solid" NODE-COLOR))

;;; Attributes of line
(define LINE-COLOR "blue")

;;; Keys used in KeyEvent
(define ADD-ROOT "t")
(define ADD-SON "n")
(define DELETE-NODE "d")
(define DELETE-NODES-FROM-LEFT "l")
(define P-EVENT "p")

;;; Mouse events
(define BUTTON-DOWN "button-down")
(define DRAG "drag")
(define BUTTON-UP "button-up")
(define MOVE "move")

;;; Default co-ordinates of Tree
(define DEF-X HALF-CANVAS-WIDTH)
(define DEF-Y RADIUS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS:

;;; Structure Tree : 
;;; A Tree is a Node
(define-struct node (center mx-dist my-dist sel? sons))

;;; Constructor Template :
;;; A Node is (make-node Posn Int Int Boolean ListOfTrees)

;;; INTERPRETATION :
;;; (make-node Posn Int Int Boolean Tree) is a Node where :
;;; center : is the center coordinates of the node
;;; mx-dist : distance of the x-coordinate of center of node to the
;;;           x-coordinate of mouse on button-down
;;; my-dist : distance of the y-coordinate of center of node to the
;;;           y-coordinate of mouse on button-down
;;; sel? : true iff the node is selected
;;; sons : are the sons of the given Node, which can be empty or ListOfTrees

;;; DESTRUCTOR TEMPLATE :
;;; node-fn : Node -> ??
#;(define (node-fn n)
    (... (node-center n)
         (node-mx-dist n)
         (node-my-dist n)
         (node-sel? n)
         (lot-fn (node-sons)) ...))



;;; Structure ListOfTrees (LOT) :
;;; A ListOfTrees is either :
;;; -- empty
;;; -- (cons Tree ListOfTrees)

;;; INTERPRETATION :
;;; empty : a sequence of Trees with no elements
;;; (cons Trees ListOfTrees) : a sequence of Trees whose first element is a Tree
;;;                           and whose other elements are represented by ListOfTrees

;;; DESTRUCTOR TEMPLATE :
;;; lot-fn : LOT -> ??
#;(define (lot-fn lot)
    (cond
      [(empty? lot) ...]
      [else
       (... (node-fn (first lot))
            (lot-fn (rest lot)) ...)]))



;;; Structure World :
(define-struct world (lot))

;;; CONSTRUCTOR TEMPLATE :
;;; A World is (make-world ListOfTrees)

;;; INTERPRETATION :
;;; (make-world ListOfTrees) is :
;;; ListOfTrees : is the list of Trees which are present in the World

;;; DESTRUCTOR TEMPLATE :
;;; world-fn : World -> ??
#;(define (world-fn lot)
    (... (lot-fn (world-lot)) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; run: Any -> World
;;; GIVEN: Any value
;;; EFFECT: Runs a copy of an initial world
;;; RETURNS: The final state of the world.  The given value is ignored.

(define (run n)
  (big-bang (initial-world n)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; initial-world: Any -> World
;;; GIVEN: Any value
;;; RETURNS: An initial world. The given value is ignored
;;; EXAMPLES:
;;; (make-world empty) = (make-world '())

;;; DESIGN STRATEGY: Use template for World
(define (initial-world any)
  (make-world empty))

;;; TESTS :
(begin-for-test
  (check-equal?
   (initial-world empty)
   (make-world '())
   "intial world with no trees in it"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants for tests

(define SUBTREE1 (make-node (make-posn HALF-CANVAS-WIDTH (+ OFF-TO-PARENT RADIUS))
                            0 0
                            false
                            empty))

(define SUBTREE2-SON (make-node (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                                           (+ OFF-TO-PARENT OFF-TO-PARENT RADIUS))
                                0 0
                                false
                                empty))

(define SUBTREE2 (make-node (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                                       (+ OFF-TO-PARENT RADIUS))
                            0 0
                            false
                            (list SUBTREE2-SON)))


(define TREES1 (make-node (make-posn HALF-CANVAS-WIDTH RADIUS)                                     
                          0 0
                          false
                          (list SUBTREE1
                                SUBTREE2)))

(define WORLD1 (make-world (list TREES1)))


(define SUBTREE2-SON-AFTER-BUTTON-DOWN
  (make-node (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                        (+ OFF-TO-PARENT OFF-TO-PARENT RADIUS))
             (- OFF-TO-SIBLING 1)
             (- OFF-TO-PARENT 1)
             false
             empty))

(define SUBTREE2-AFTER-BUTTON-DOWN
  (make-node (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                        (+ OFF-TO-PARENT RADIUS))
             (- OFF-TO-SIBLING 1)
             -1
             false
             (list SUBTREE2-SON-AFTER-BUTTON-DOWN)))


(define SUBTREE1-SELECTED (make-node (make-posn HALF-CANVAS-WIDTH
                                                (+ OFF-TO-PARENT RADIUS))
                                     -1 -1
                                     true
                                     empty))

(define TREES1-AFTER-MOUSE-DOWN (make-node (make-posn HALF-CANVAS-WIDTH RADIUS)                                     
                                           (- 1)
                                           (- (+ OFF-TO-PARENT 1))
                                           false
                                           (list SUBTREE1-SELECTED
                                                 SUBTREE2-AFTER-BUTTON-DOWN)))

(define WORLD1-AFTER-MOUSE-DOWN (make-world (list TREES1-AFTER-MOUSE-DOWN)))


(define SUBTREE1-AFTER-DRAG (make-node (make-posn (+ HALF-CANVAS-WIDTH 1)
                                                  (+ OFF-TO-PARENT RADIUS 1))
                                       -1 -1
                                       true
                                       empty))

(define SUBTREE1-AFTER-BUTTON-UP (make-node (make-posn (+ HALF-CANVAS-WIDTH 1)
                                                       (+ OFF-TO-PARENT RADIUS 1))
                                            0 0
                                            false
                                            empty))

(define SUBTREE2-SON-AFTER-BUTTON-DOWN-ON-SUBTREE2
  (make-node (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                        (+ OFF-TO-PARENT OFF-TO-PARENT RADIUS))
             0
             OFF-TO-PARENT
             false
             empty))

(define SUBTREE2-SELECTED
  (make-node (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                        (+ OFF-TO-PARENT RADIUS))
             0 0
             true
             (list SUBTREE2-SON-AFTER-BUTTON-DOWN-ON-SUBTREE2)))

(define SUBTREE2-SON-AFTER-DRAG
  (make-node (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING 1)
                        (+ OFF-TO-PARENT OFF-TO-PARENT RADIUS 1))
             0
             OFF-TO-PARENT
             false
             empty))

(define SUBTREE2-AFTER-DRAG
  (make-node (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING 1)
                        (+ OFF-TO-PARENT RADIUS 1))
             0 0
             true
             (list SUBTREE2-SON-AFTER-DRAG)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General function
;;; tree-after-mouse-event : Tree Integer Integer Boolean
;;;                          (ListOfTree NonNegInt NonNegInt -> ListOfTrees)
;;;                          NonNegInt NoneNegInt
;;; GIVEN : A Tree, mx-dist and my-dist, sel?, a function name which takes ListOfTrees,
;;;         mouse coordinates mx and my and returns ListOfTrees, and mouse coordinates
;;;         mx and my
;;; RETURNS : A Tree after mouse event and fn-name performed on it

;;; DESIGN STRATEGY : Use template for Tree on tr
(define (tree-after-mouse-event tr mx-dist my-dist sel? fn-name mx my)
  (make-node (node-center tr)
             mx-dist
             my-dist
             sel?
             (fn-name (node-sons tr) mx my)))



;;; General function
;;; lot-after-mouse-event-general : (Tree NonNegInt NonNegInt -> Tree) ListOfTrees
;;;                                 NonNegInt NonNegInt -> ListOfTrees
;;; GIVEN : A function name which takes Tree, mouse coordinates mx and my as arguments
;;;         and returns Tree, ListOfTrees, mx and my
;;; RETURNS : A ListOfTrees, with the given function performed on each tree in the list

;;; DESIGN STRATEGY : Use HOF map on lot
(define (lot-after-mouse-event-general tree-fn-name lot mx my)
  (map
   ;;; Tree -> Tree
   ;;; GIVEN: A Tree
   ;;; RETURNS: An updated Tree after the mouse event
   (lambda (tr) (tree-fn-name tr mx my))
   lot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-after-mouse-event : World NonNegInt NonNegInt MouseEvent -> World
;;; GIVEN : A World, mouse coordinates mx and my, and a MouseEvent
;;; RETURNS : The state of the world as it should be following the given
;;;          mouse event at that location.
;;; EXAMPLE :
;;; (world-after-mouse-event WORLD1 (+ HALF-CANVAS-WIDTH 1)
;;;                                 (+ RADIUS OFF-TO-PARENT 1)
;;;                                 BUTTON-DOWN) => WORLD1-AFTER-MOUSE-DOWN

;;; DESIGN STRATEGY : Use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world (lot-after-mouse-event (world-lot w) mx my mev)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-mouse-event WORLD1 (+ HALF-CANVAS-WIDTH 1)
                            (+ RADIUS OFF-TO-PARENT 1)
                            BUTTON-DOWN)
   WORLD1-AFTER-MOUSE-DOWN
   "After button-down, the ListOfTrees will be updated"))



;;; Helper function for world-after-mouse-event
;;; lot-after-mouse-event : ListOfTrees NonNegInt NonNegInt MouseEvent-> ListOfTrees
;;; GIVEN : A ListOfTrees, mouse coordinates mx and my and a MouseEvent
;;; RETURNS : An updated ListOfTrees after the mouse event
;;; EXAMPLES :
;;; (lot-after-mouse-event (list TREES1) (+ HALF-CANVAS-WIDTH 1)
;;;                        (+ RADIUS OFF-TO-PARENT 1) BUTTON-DOWN)
;;;   => (list TREES1-AFTER-MOUSE-DOWN)
;;; (lot-after-mouse-event (list SUBTREE1-SELECTED) (+ HALF-CANVAS-WIDTH 2)
;;;                        (+ RADIUS OFF-TO-PARENT 2) DRAG)
;;;   => (list SUBTREE1-AFTER-DRAG)

;;; DESIGN STRATEGY : Divide into cases based on MouseEvent
(define (lot-after-mouse-event lot mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (lot-after-mouse-down lot mx my)]
    [(mouse=? mev DRAG) (lot-after-drag lot mx my)]
    [(mouse=? mev BUTTON-UP) (lot-after-mouse-up lot mx my)]
    [else lot]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (lot-after-mouse-event (list TREES1)
                          (+ HALF-CANVAS-WIDTH 1)
                          (+ RADIUS OFF-TO-PARENT 1)
                          BUTTON-DOWN)
   (list TREES1-AFTER-MOUSE-DOWN)
   "After button-down, the updated ListOfTrees with the node selected has
    its sel? field updated to true")
  (check-equal?
   (lot-after-mouse-event (list SUBTREE1-SELECTED)
                          (+ HALF-CANVAS-WIDTH 2)
                          (+ RADIUS OFF-TO-PARENT 2)
                          DRAG)
   (list SUBTREE1-AFTER-DRAG)
   "After drag, the updated ListOfTrees with the selected tree and its subtree
    after drag is returned")
  (check-equal?
   (lot-after-mouse-event (list SUBTREE1-AFTER-DRAG)
                          (+ HALF-CANVAS-WIDTH 6)
                          (+ RADIUS OFF-TO-PARENT 6)
                          BUTTON-UP)
   (list SUBTREE1-AFTER-BUTTON-UP)
   "After button-up, the updated ListOfTrees with the selected node having sel?
    updated to false is returned")
  (check-equal?
   (lot-after-mouse-event (list TREES1)
                          (+ HALF-CANVAS-WIDTH 1)
                          (+ RADIUS OFF-TO-PARENT 1)
                          MOVE)
   (list TREES1)
   "On other mouse events, same ListOfTrees is returned"))



;;; Helper function for lot-after-mouse-event
;;; lot-after-mouse-down : ListOfTree NonNegInt NonNegInt -> ListOfTrees
;;; GIVEN : A ListOfTree, x and y coordinates of mouse event
;;; RETURNS : A ListOfTree after button-down event performed on it
;;; EXAMPLE :
;;; (lot-after-mouse-down (list TREES1) (+ HALF-CANVAS-WIDTH 1)
;;;                       (+ RADIUS OFF-TO-PARENT 1))
;;;   => (list TREES1-AFTER-MOUSE-DOWN)

;;; DESIGN STRATEGY : Use of General function lot-after-mouse-event-general
(define (lot-after-mouse-down lot mx my)
  (lot-after-mouse-event-general tree-after-mouse-down
                                 lot
                                 mx my))

;;; TESTS :
(begin-for-test
  (check-equal?
   (lot-after-mouse-down (list TREES1)
                         (+ HALF-CANVAS-WIDTH 1)
                         (+ RADIUS OFF-TO-PARENT 1))
   (list TREES1-AFTER-MOUSE-DOWN)
   "After button-down, the updated ListOfTrees with the node selected has
    its sel? field updated to true"))



;;; Helper function for lot-after-mouse-down
;;; tree-after-mouse-down : Tree NonNegInt NonNegInt -> Tree
;;; GIVEN : A Tree and x-y coordinates of mouse
;;; RETURNS : An updated instance of Tree after button-down event
;;; EXAMPLE :
;;; (tree-after-mouse-down TREES1 (+ HALF-CANVAS-WIDTH 1)
;;;                               (+ RADIUS OFF-TO-PARENT 1))
;;;   => TREES1-AFTER-MOUSE-DOWN

;;; DESIGN STRATEGY : Use General function tree-after-mouse-event
(define (tree-after-mouse-down tr mx my)
  (tree-after-mouse-event tr
                          (- (posn-x (node-center tr)) mx)
                          (- (posn-y (node-center tr)) my)
                          (inside-node? (node-center tr) mx my)
                          lot-after-mouse-down mx my))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-after-mouse-down TREES1
                          (+ HALF-CANVAS-WIDTH 1)
                          (+ RADIUS OFF-TO-PARENT 1))
   TREES1-AFTER-MOUSE-DOWN
   "After button-down, the given Tree and its subtree are updated"))



;;; Helper function for tree-after-mouse-down
;;; inside-node? : Posn NonNegInt NonNegInt -> Boolean
;;; GIVEN : Center of the Tree, x and y coordinates of mouse
;;; RETURNS : True, iff mouse is inside the Tree else false
;;; EXAMPLE :
;;; (inside-node? (make-posn HALF-CANVAS-WIDTH RADIUS)
;;;               (+ HALF-CANVAS-WIDTH 1) (+ RADIUS 1))
;;;   => true

;;; DESIGN STRATEGY : Combine simpler functions
(define (inside-node? cen mx my)
  (<= (sqrt (+ (sqr (- mx (posn-x cen)))
               (sqr (- my (posn-y cen)))))
      RADIUS))

;;; TESTS :
(begin-for-test
  (check-equal?
   (inside-node? (make-posn HALF-CANVAS-WIDTH RADIUS)
                 (+ HALF-CANVAS-WIDTH 1)
                 (+ RADIUS 1))
   true
   "Since the given mx and my are inside the node, true is returned"))



;;; Helper function for lot-after-mouse-event
;;; lot-after-mouse-up : ListOfTrees NonNegInt NonNegInt -> ListOfTrees
;;; GIVEN : A ListOfTrees, x and y coordinates mouse
;;; RETURNS : An updated ListOfTrees after button-up event
;;; EXAMPLE :
;;; (lot-after-mouse-up (list SUBTREE1-AFTER-DRAG) (+ HALF-CANVAS-WIDTH 6)
;;;                     (+ RADIUS OFF-TO-PARENT 6)) => (list SUBTREE1-AFTER-BUTTON-UP)

;;; DESIGN STRATEGY : Use of General function lot-after-mouse-event-general
(define (lot-after-mouse-up lot mx my)
  (lot-after-mouse-event-general tree-after-mouse-up
                                 lot
                                 mx my))

;;; TESTS :
(begin-for-test
  (check-equal?
   (lot-after-mouse-up (list SUBTREE1-AFTER-DRAG)
                       (+ HALF-CANVAS-WIDTH 6)
                       (+ RADIUS OFF-TO-PARENT 6))
   (list SUBTREE1-AFTER-BUTTON-UP)
   "After button-up, the ListOfTrees is updated with the selected tree updated
    to unselected"))



;;; Helper function for lot-after-mouse-up
;;; tree-after-mouse-up : Tree NonNegInt NonNegInt -> Tree
;;; GIVEN : A Tree and x and y coordinates of mouse
;;; RETURNS : An updated Tree after mouse up event
;;; EXAMPLE :
;;; (tree-after-mouse-up SUBTREE1-AFTER-DRAG (+ HALF-CANVAS-WIDTH 6)
;;;                      (+ RADIUS OFF-TO-PARENT 6)) => SUBTREE1-AFTER-BUTTON-UP

;;; DESIGN STRATEGY : Use General function tree-after-mouse-event
(define (tree-after-mouse-up tr mx my)
  (tree-after-mouse-event tr
                          0 0
                          false
                          lot-after-mouse-up
                          mx my))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-after-mouse-up SUBTREE1-AFTER-DRAG
                        (+ HALF-CANVAS-WIDTH 6)
                        (+ RADIUS OFF-TO-PARENT 6))
   SUBTREE1-AFTER-BUTTON-UP
   "After button-up, the given Tree and its sons, any of which were selected
    is unselected"))



;;; Helper function for lot-after-mouse-event
;;; lot-after-drag: ListOfTrees NonNegInt NonNegInt -> ListOfTrees
;;; GIVEN : A ListOfTrees, mouse coordinates mx and my
;;; RETURNS : An updated ListOfTrees after drag event performed on the selected
;;;           Tree in the list
;;; EXAMPLE :
;;; (lot-after-drag (list SUBTREE1-SELECTED) (+ HALF-CANVAS-WIDTH 2)
;;;                 (+ RADIUS OFF-TO-PARENT 2)) => (list SUBTREE1-AFTER-DRAG)

;;; DESIGN STRATEGY : Use of General function lot-after-mouse-event-general
(define (lot-after-drag lot mx my)
  (lot-after-mouse-event-general tree-after-drag
                                 lot
                                 mx my))

;;; TESTS :
(begin-for-test
  (check-equal?
   (lot-after-drag (list SUBTREE1-SELECTED)
                   (+ HALF-CANVAS-WIDTH 2)
                   (+ RADIUS OFF-TO-PARENT 2))
   (list SUBTREE1-AFTER-DRAG)
   "After drag, the updated ListOfTrees with the selected tree and its subtree
    after drag is returned"))



;;; Helper function for lot-after-drag
;;; tree-after-drag : Tree NonNegInt NonNegInt -> Tree
;;; GIVEN : A Tree and x and y coordinate of mouse
;;; RETURNS : An updated Tree after drag event performed on selected Tree
;;; EXAMPLES :
;;; (tree-after-drag SUBTREE1-SELECTED (+ HALF-CANVAS-WIDTH 2)
;;;                  (+ RADIUS OFF-TO-PARENT 2)) => SUBTREE1-AFTER-DRAG
;;; (tree-after-drag SUBTREE2 (+ HALF-CANVAS-WIDTH 2)
;;;                  (+ RADIUS OFF-TO-PARENT 2)) => SUBTREE2

;;; DESIGN STRATEGY : Use template for Tree on tr
(define (tree-after-drag tr mx my)
  (if (node-to-selected? tr)
      (drag-tree tr mx my)
      (tree-after-mouse-event tr
                              (node-mx-dist tr)
                              (node-my-dist tr)
                              (node-sel? tr)
                              lot-after-drag mx my)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-after-drag SUBTREE1-SELECTED
                    (+ HALF-CANVAS-WIDTH 2)
                    (+ RADIUS OFF-TO-PARENT 2))
   SUBTREE1-AFTER-DRAG
   "After drag, the Tree and its sons, if selected, after drag are returned")
  (check-equal?
   (tree-after-drag SUBTREE2
                    (+ HALF-CANVAS-WIDTH 2)
                    (+ RADIUS OFF-TO-PARENT 2))
   SUBTREE2
   "Since the Tree nor its sons are selected, same Tree is returned"))



;;; Helper function for tree-after-drag
;;; drag-tree : Tree NonNegInt NonNegInt -> Tree
;;; GIVEN : A Tree and x and y coordinates of mouse
;;; RETURNS : An updated Tree and it's subtree after drag event performed on it
;;; EXAMPLES :
;;; (drag-tree SUBTREE1-SELECTED (+ HALF-CANVAS-WIDTH 2)
;;;            (+ RADIUS OFF-TO-PARENT 2)) => SUBTREE1-AFTER-DRAG
;;; (drag-tree SUBTREE2-SELECTED (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING 1)
;;;           (+ OFF-TO-PARENT RADIUS 1)) => SUBTREE2-AFTER-DRAG

;;; DESIGN STRATEGY : Use template for Tree on tr
(define (drag-tree tr mx my)
  (make-node (make-posn (+ (node-mx-dist tr) mx) (+ (node-my-dist tr) my))
             (node-mx-dist tr)
             (node-my-dist tr)
             (node-sel? tr)
             (drag-subtree (node-sons tr) mx my)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (drag-tree SUBTREE1-SELECTED
              (+ HALF-CANVAS-WIDTH 2)
              (+ RADIUS OFF-TO-PARENT 2))
   SUBTREE1-AFTER-DRAG
   "After drag, the Tree and its sons, if selected, after drag are returned")
  (check-equal?
   (drag-tree SUBTREE2-SELECTED
              (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING 1)
              (+ OFF-TO-PARENT RADIUS 1))
   SUBTREE2-AFTER-DRAG
   "After drag, the Tree and its sons, if selected, after drag are returned"))



;;; Helper function for drag-tree
;;; drag-subtree : ListOfTrees NonNegInt NonNegInt -> ListOfTrees
;;; GIVEN : A ListOfTrees, mx and my
;;; RETURNS : An updated ListOfTrees after drag event
;;; EXAMPLE :
;;; (drag-subtree (list SUBTREE2-SON-AFTER-BUTTON-DOWN-ON-SUBTREE2)
;;;               (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING 1)
;;;               (+ OFF-TO-PARENT RADIUS 1))
;;;   => (list SUBTREE2-SON-AFTER-DRAG)

;;; DESIGN STRATEGY : Use of General function lot-after-mouse-event-general
(define (drag-subtree lot mx my)
  (lot-after-mouse-event-general drag-tree
                                 lot
                                 mx my))

;;; TESTS :
(begin-for-test
  (check-equal?
   (drag-subtree (list SUBTREE2-SON-AFTER-BUTTON-DOWN-ON-SUBTREE2)
                 (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING 1)
                 (+ OFF-TO-PARENT RADIUS 1))
   (list SUBTREE2-SON-AFTER-DRAG)
   "The given subtree after drag event is returned"))                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants for tests

(define DEF-ROOT-NODE (make-node (make-posn DEF-X DEF-Y) 0 0 false empty))

(define SELECTED-ROOT (make-node (make-posn DEF-X DEF-Y) 0 0 true empty))

(define ROOT-NODE-IN-LEFT (make-node (make-posn (- DEF-X 5) DEF-Y) 0 0 false empty))

(define SON1 (make-node (make-posn HALF-CANVAS-WIDTH (+ (* 2 OFF-TO-PARENT) RADIUS))
                        (- 1) (- OFF-TO-PARENT 1) false empty))

(define SON2 (make-node (make-posn DEF-X (+ OFF-TO-PARENT DEF-Y))
                        0 OFF-TO-PARENT false empty))

(define ROOT-SON2 (make-node (make-posn DEF-X DEF-Y) 0 0 true (list SON2)))

(define SUBTREE1-SEL-SON1 (make-node (make-posn DEF-X (+ OFF-TO-PARENT DEF-Y))
                                     -1 -1
                                     true
                                     (list SON1)))

(define TREE1-AFTER-ADD-SON (make-node (make-posn DEF-X DEF-Y)                                     
                                       (- 1)
                                       (- (+ OFF-TO-PARENT 1))
                                       false
                                       (list SUBTREE1-SEL-SON1
                                             SUBTREE2-AFTER-BUTTON-DOWN)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General function
;;; tree-after-key-event : Tree (ListOfTrees -> ListOfTrees) -> Tree
;;; GIVEN : A Tree, function which takes ListOfTrees as input and returns ListOfTrees
;;; RETURNS : An updated Tree after the given function is performed on given
;;;           Tree's sons

;;; DESIGN STRATEGY : Use template for Tree on n
(define (tree-after-key-event n fn-name)
  (make-node (node-center n)
             (node-mx-dist n)
             (node-my-dist n)
             (node-sel? n)
             (fn-name (node-sons n))))



;;; General function
;;; tree-delete-fold : (Tree -> Boolean) (Tree -> Tree) ListOfTrees -> ListOfTrees
;;; GIVEN : Condition, function name and ListOfTrees
;;; RETURNS : A ListOfTrees with the given function performed on the list based
;;;           on the condition

;;; DESIGN STRATEGY : Use HOF map and filter on lot
(define (tree-delete-fold condition fn-name-for-tree lot)
  (map fn-name-for-tree
       (filter
        ;;; Tree -> Boolean
        ;;; GIVEN : A Tree
        ;;; RETURNS : True if the codition is not satisfied else false
        (lambda (tr) (not (condition tr))) lot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-after-key-event :  World KeyEvent -> World
;;; GIVEN: A World and a key event
;;; RETURNS: The state of the World following the given key event
;;; EXAMPLES :
;;; (world-after-key-event (initial-world 4) ADD-ROOT)  
;;;   => (make-world (list DEF-ROOT-NODE))

;;; DESIGN STRATEGY : Divide into cases on KeyEvent ke
(define (world-after-key-event w ke)
  (cond
    [(key=? ke ADD-ROOT)
     (world-after-add-root-event w)]
    [else
     (make-world
      (lot-after-key-event (world-lot w) ke))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (world-after-key-event (initial-world 4) ADD-ROOT)
   (make-world (list DEF-ROOT-NODE))
   "Adds root node at the default position")
  (check-equal?
   (world-after-key-event (initial-world 4) P-EVENT)
   (make-world empty)
   "Invalid KeyEvent - World stays intact"))



;;; Helper function for world-after-key-event
;;; world-after-add-root-event : World -> World
;;; GIVEN : A World
;;; RETURNS : A World, after a new root Node added to it
;;; EXAMPLES :
;;; (world-after-add-root-event (initial-world 1)) =>
;;;            (make-world (list DEF-ROOT-NODE))

;;; DESIGN STRATEGY : Use template for World on w
(define (world-after-add-root-event w)
  (make-world
   (cons (make-node (make-posn DEF-X DEF-Y)
                    0 0
                    false
                    empty)
         (world-lot w))))

;;; TESTS :
(begin-for-test
  (world-after-add-root-event (initial-world 1))
  (make-world (list DEF-ROOT-NODE))
  "Adds root node at the default position")



;;; Helper function for world-after-key-event
;;; lot-after-key-event : ListOfTrees KeyEvent -> ListOfTrees
;;; GIVEN : A ListOfTrees and key event
;;; RETURNS : A ListOfTrees with given key event performed on it
;;; EXAMPLES :
;;; (lot-after-key-event (list TREES1-AFTER-MOUSE-DOWN) ADD-SON) =>
;;;            (list TREE1-AFTER-ADD-SON)

;;; DESIGN STRATEGY : Divide into cases on KeyEvent ke
(define (lot-after-key-event lot ke)
  (cond
    [(key=? ke ADD-SON) (lot-after-add-son-event lot)]
    [(key=? ke DELETE-NODE) (lot-after-delete-node-event lot)]
    [(key=? ke DELETE-NODES-FROM-LEFT) (lot-after-nodes-deleted-from-left lot)]
    [else lot]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (lot-after-key-event (list TREES1-AFTER-MOUSE-DOWN) ADD-SON)
   (list TREE1-AFTER-ADD-SON)
   "a son gets added")
  (check-equal?
   (lot-after-key-event (list SELECTED-ROOT) DELETE-NODE)
   empty
   "root gets deleted")
  (check-equal?
   (lot-after-key-event (list ROOT-NODE-IN-LEFT) DELETE-NODES-FROM-LEFT)
   empty
   "nodes on left gets deleted"))



;;; Helper function for lot-after-key-event
;;; lot-after-add-son-event : ListOfTrees -> ListOfTrees
;;; GIVEN : A ListOfTrees
;;; RETURNS : A ListOfTrees with add new son key event performed on the selected Tree
;;; EXAMPLES :
;;; (lot-after-add-son-event (list TREES1-AFTER-MOUSE-DOWN)) =>
;;;            (list TREE1-AFTER-ADD-SON)

;;; DESIGN STRATEGY : Use HOF map on lot
(define (lot-after-add-son-event lot)
  (map tree-after-add-son-event lot))

;;; TESTS :
(begin-for-test
  (check-equal?
   (lot-after-add-son-event (list TREES1-AFTER-MOUSE-DOWN))
   (list TREE1-AFTER-ADD-SON)
   "a son gets added"))



;;; Helper function for lot-after-add-son-event
;;; tree-after-add-son-event : Tree -> Tree
;;; GIVEN : A Tree 
;;; RETURNS : A Tree, with a new son added to the selected Tree
;;; EXAMPLES :
;;; (tree-after-add-son-event TREES1-AFTER-MOUSE-DOWN) =>
;;;            TREE1-AFTER-ADD-SON

;;; DESIGN STRATEGY : Divide into cases on node-to-selected?
(define (tree-after-add-son-event tr)
  (if (node-to-selected? tr)
      (add-new-son-to-tree tr)
      (tree-after-key-event tr lot-after-add-son-event)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-after-add-son-event TREES1-AFTER-MOUSE-DOWN)
   TREE1-AFTER-ADD-SON
   "a son gets added"))



;;; Helper function for tree-after-add-son-event
;;; add-new-son-to-tree : Tree -> Tree
;;; GIVEN : A Tree
;;; RETURNS : An updated Tree after a new son has been added to its ListOfTrees
;;; EXAMPLES :
;;; (add-new-son-to-tree SELECTED-ROOT) => ROOT-SON2

;;; DESIGN STRATEGY : Use template for Tree on n
(define (add-new-son-to-tree n)
  (make-node
   (node-center n)
   (node-mx-dist n)
   (node-my-dist n)
   (node-sel? n)
   (add-new-son (node-center n) (node-mx-dist n)
                (node-my-dist n) (node-sons n))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (add-new-son-to-tree SELECTED-ROOT)
   ROOT-SON2
   "a new son added to root"))



;;; Helper function for add-new-son-to-tree
;;; add-new-son : Posn Integer Integer ListOfTrees -> ListOfTrees
;;; GIVEN : Center of parent Tree, mx-dist and my-dist and ListOfTrees
;;; RETURNS : A ListOfTrees, with a new son (Tree) added ot the list
;;; EXAMPLES :
;;; (add-new-son (make-posn DEF-X DEF-Y) 0 0 empty) => SON2

;;; DESIGN STRATEGY : Combine simpler functions
(define (add-new-son n-center mx-dist my-dist lot)
  (cons (make-node (make-posn
                    (calculate-x-offset (posn-x n-center) lot)
                    (+ (posn-y n-center) OFF-TO-PARENT))
                   (mx-dist-of-son n-center mx-dist lot)
                   (+ my-dist OFF-TO-PARENT)
                   false
                   empty)
        lot))

;;; TESTS :
(begin-for-test
  (check-equal?
   (add-new-son (make-posn DEF-X DEF-Y) 0 0 empty)
   (list SON2)
   "returns a new node which is son to parent"))



;;; Helper function for add-new-son
;;; calculate-x-offset : NonNegInt ListOfTrees -> NonNegInt
;;; GIVEN : A NonNegInt, representing the x-coordinate of the center of
;;;         parent Tree and ListOfTrees
;;; RETURNS : A NonNegInt, representing the x-coordinate of the center
;;;           of the new son (Tree)
;;; EXAMPLES :
;;; (calculate-x-offset DEF-X (list SON2)) = 280

;;; DESIGN STRATEGY : Divide into cases on (empty? lot)
(define (calculate-x-offset x lot)
  (if (empty? lot)
      x
      (+ OFF-TO-SIBLING (max-x-from-sons lot))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (calculate-x-offset DEF-X empty)
   DEF-X
   "new x-coordinate of son")
  (check-equal?
   (calculate-x-offset DEF-X (list SON2))
   (+ DEF-X OFF-TO-SIBLING)
   "new x-coordinate f son beside sibling"))



;;; Helper function for calculate-x-offset
;;; max-x : ListOfTrees -> NonNegInt
;;; GIVEN : A ListOfTrees
;;; RETURNS : A NonNegInt, representing the maximum value of x-coordinate
;;;           of the centers of the Trees in the list
;;; EXAMPLES :
;;; (max-x-from-sons (list SON1 SON2)) = 250

;;; DESIGN STRATEGY : Use HOF foldr on lot
(define (max-x-from-sons lot)
  (foldr
   ;;; Tree NonNegInt -> NonNegInt
   ;;; GIVEN : A Tree and NonNegInt representing the maximum
   ;;;         value of x-coordinate of center from rest of list
   ;;; RETURNS : A NonNegInt representing the maximum value of x
   (lambda (tr max-x-from-rest)
     (max (posn-x (node-center tr))
          max-x-from-rest))
   0
   lot))

;;; TESTS :
(begin-for-test
  (check-equal?
   (max-x-from-sons (list SON1 SON2))
   DEF-X
   "returns x-cordinate of the rightmost son"))



;;; Helper function for add-new-son
;;; mx-dist-of-son : Posn Integer ListOfTrees -> Integer
;;; GIVEN : A Posn, mx-dist and ListOfTrees
;;; RETURNS : An Integer representing the mx-dist for son
;;; EXAMPLES :
;;; (mx-dist-of-son (make-posn DEF-X DEF-Y) 5 (list SON2)) =>
;;;   (+ 5 OFF-TO-SIBLING)

;;; DESIGN STRATEGY : Combine simpler functions
(define (mx-dist-of-son n-center mx-dist lot)
  (+ mx-dist (- (calculate-x-offset (posn-x n-center)
                                    lot)
                (posn-x n-center))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (mx-dist-of-son (make-posn DEF-X DEF-Y) 5 (list SON2))
   (+ 5 OFF-TO-SIBLING)
   "returns the offseet for the son from mouse co-ordinate"))



;;; Helper function for lot-after-key-event
;;; lot-after-delete-node-event : ListOfTrees -> ListOfTrees
;;; GIVEN : A ListOfTrees
;;; RETURNS : A ListOfTrees with the selected Tree deleted from the list
;;; EXAMPLES :
;;; (lot-after-delete-node-event (list SELECTED-ROOT)) => empty

;;; DESIGN STRATEGY : Use General function tree-delete-fold
(define (lot-after-delete-node-event lot)
  (tree-delete-fold node-to-selected?
                    tree-after-delete-event
                    lot))

;;; TESTS :
(begin-for-test
  (check-equal?
   (lot-after-delete-node-event (list SELECTED-ROOT))
   empty
   "root gets deleted"))



;;; Helper function lot-after-delete-node-event
;;; tree-after-delete-event : Tree -> Tree
;;; GIVEN : A Tree
;;; RETURNS : A Tree, with selected sub-tree deleted from the given Tree
;;; EXAMPLES :
;;; (tree-after-delete-event SELECTED-ROOT) => SELECTED-ROOT

;;; DESIGN STRATEGY : Use General function tree-after-key-event
(define (tree-after-delete-event tr)
  (tree-after-key-event tr lot-after-delete-node-event))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-after-delete-event SELECTED-ROOT)
   SELECTED-ROOT
   "root gets deleted"))



;;; Helper function for lot-after-key-event
;;; lot-after-nodes-deleted-from-left : ListOfTrees -> ListOfTrees
;;; GIVEN : A ListOfTrees 
;;; RETURNS : A ListOfTrees, with all the Trees and their sub-trees
;;;           lying on the left-half of the canvas deleted from list
;;; EXAMPLES :
;;; (lot-after-nodes-deleted-from-left (list ROOT-NODE-IN-LEFT DEF-ROOT-NODE)) =>
;;;    (list DEF-ROOT-NODE)

;;; DESIGN STRATEGY : Use General function tree-delete-fold
(define (lot-after-nodes-deleted-from-left lot)
  (tree-delete-fold tree-in-left-half?
                    tree-after-nodes-deleted-from-left
                    lot))

;;; TESTS :
(begin-for-test
  (check-equal?
   (lot-after-nodes-deleted-from-left (list ROOT-NODE-IN-LEFT DEF-ROOT-NODE))
   (list DEF-ROOT-NODE)
   "all nodes from  left side of canvas gets deleted"))



;;; Helper function for lot-after-nodes-deleted-from-left
;;; tree-in-left-half? : Tree -> Boolean
;;; GIVEN : A Tree
;;; RETURNS : True, iff the x-ccordinate of the center of Tree lies in the
;;;           left-half of canvas
;;; EXAMPLES :
;;; (tree-in-left-half? ROOT-NODE-IN-LEFT) => true

;;; DESIGN STRATEGY : Use template for Tree on tr
(define (tree-in-left-half? tr)
  (< (posn-x (node-center tr))
     HALF-CANVAS-WIDTH))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-in-left-half? ROOT-NODE-IN-LEFT)
   true
   "ROOT-NODE-IN-LEFT is in the left side of the node"))



;;; Helper function for lot-after-nodes-deleted-from-left
;;; tree-after-nodes-deleted-from-left : Tree -> Tree
;;; GIVEN : A Tree 
;;; RETURNS : A Tree, with all its son(s) and their sub-trees
;;;           lying on the left-half of the canvas deleted from list
;;; EXAMPLES :

;;; DESIGN STRATEGY : Use General function tree-after-key-event
(define (tree-after-nodes-deleted-from-left tr)
  (tree-after-key-event tr
                        lot-after-nodes-deleted-from-left))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-after-nodes-deleted-from-left ROOT-NODE-IN-LEFT)
   ROOT-NODE-IN-LEFT
   "The Trees lying on left-half of canvas is deleted"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants for tests

(define SUBTREE2-POSN (make-posn (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                                 (+ OFF-TO-PARENT RADIUS)))

(define SUBTREE2-SON-IMAGE
  (scene+line (place-image UNSEL-NODE
                           (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                           (+ OFF-TO-PARENT OFF-TO-PARENT RADIUS)
                           EMPTY-CANVAS)
              (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
              (+ OFF-TO-PARENT RADIUS)
              (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
              (+ OFF-TO-PARENT OFF-TO-PARENT RADIUS)
              LINE-COLOR))

(define SUBTREE2-IMAGE
  (scene+line (place-image UNSEL-NODE
                           (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
                           (+ OFF-TO-PARENT RADIUS)
                           SUBTREE2-SON-IMAGE)
              HALF-CANVAS-WIDTH
              RADIUS
              (+ HALF-CANVAS-WIDTH OFF-TO-SIBLING)
              (+ OFF-TO-PARENT RADIUS)
              LINE-COLOR))

(define SUBTREE1-IMAGE
  (scene+line (place-image SEL-NODE
                           HALF-CANVAS-WIDTH
                           (+ OFF-TO-PARENT RADIUS)
                           SUBTREE2-IMAGE)
              HALF-CANVAS-WIDTH
              RADIUS
              HALF-CANVAS-WIDTH
              (+ OFF-TO-PARENT RADIUS)
              LINE-COLOR))

(define TREES1-IMAGE
  (place-image UNSEL-NODE
               HALF-CANVAS-WIDTH
               RADIUS
               SUBTREE1-IMAGE))

(define WORLD1-IMAGE TREES1-IMAGE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-to-scene : World -> Scene
;;; GIVEN : An instance of World
;;; RETURNS : A Scene that portrays the given World
;;; EXAMPLES :
;;; (world-to-scene WORLD1-AFTER-MOUSE-DOWN) => WORLD1-IMAGE

;;; DESIGN STRATEGY: Use HOF foldr
(define (world-to-scene w)
  (foldr
   display-tree
   EMPTY-CANVAS
   (world-lot w)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (world-to-scene WORLD1-AFTER-MOUSE-DOWN)
   WORLD1-IMAGE
   "The image of the given World is displayed"))



;;; Helper function for world-to-scene
;;; display-tree : Tree Scene -> Scene
;;; GIVEN : An instance of Tree and a Scene
;;; RETURNS : A Scene with the Tree displayed on it
;;; EXAMPLES :
;;; (display-tree TREES1-AFTER-MOUSE-DOWN EMPTY-CANVAS) => TREES1-IMAGE

;;; DESIGN STRATEGY: Use template for Tree on tr
(define (display-tree tr scene)
  (place-image (if (node-to-selected? tr) SEL-NODE UNSEL-NODE)
               (posn-x (node-center tr))
               (posn-y (node-center tr))
               (display-sons (node-center tr) (node-sons tr) scene)))         

;;; TESTS :
(begin-for-test
  (check-equal?
   (display-tree TREES1-AFTER-MOUSE-DOWN EMPTY-CANVAS)
   TREES1-IMAGE
   "The image of given Tree is painted on given scene"))



;;; Helper function for display-tree
;;; display-sons : Posn ListOfTrees Scene -> Scene
;;; GIVEN : Coordinates of center, ListOfTrees and a scene
;;; RETURNS : A scene with sons and lines added
;;; EXAMPLES :
;;; (display-sons SUBTREE2-POSN
;;;               (list SUBTREE2-SON-AFTER-BUTTON-DOWN)
;;;               EMPTY-CANVAS)
;;;  => SUBTREE2-SON-IMAGE

;;; DESIGN STRATEGY : Use HOF foldr on scn and lot
(define (display-sons parent lot scene)
  (foldr
   ;;; Tree Scene -> Scene
   ;;; GIVEN: A Tree and a Scene
   ;;; RETURNS: A Scene with Tree and lines added
   (lambda (tr r) (scene+line (display-tree tr r)
                             (posn-x parent)
                             (posn-y parent)
                             (posn-x (node-center tr))
                             (posn-y (node-center tr))
                             LINE-COLOR))
   scene
   lot))

;;; TESTS :
(begin-for-test
  (check-equal?
   (display-sons SUBTREE2-POSN
                 (list SUBTREE2-SON-AFTER-BUTTON-DOWN)
                 EMPTY-CANVAS)
   SUBTREE2-SON-IMAGE
   "The given ListOfTrees is painted on the given scene"))                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; node-to-selected? : Tree -> Boolean
;;; GIVEN : A Tree 
;;; RETURNS : True iff the given Tree is selected
;;; EXAMPLE :
;;; (node-to-selected? SUBTREE2-SELECTED) => true

;;; DESIGN STRATEGY: Use template for Tree on tr
(define (node-to-selected? tr)
  (node-sel? tr))

;;; TESTS :
(begin-for-test
  (check-equal?
   (node-to-selected? SUBTREE2-SELECTED)
   true
   "Returns true since the Tree is selected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-to-trees : World -> ListOfTree
;;; GIVEN : A World
;;; RETURNS : A list of all the Trees in the given world.
;;; EXAMPLES :
;;; (world-to-trees WORLD1) => (list TREES1)

;;; DESIGN STRATEGY: Use template for World on w
(define (world-to-trees w)
  (world-lot w))

;;; TESTS :
(begin-for-test
  (check-equal?
   (world-to-trees WORLD1)
   (list TREES1)
   "It returns the ListOfTrees present in the given World"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tree-to-root : Tree -> Tree
;;; GIVEN : A tree
;;; RETURNS : The Tree at the root of the Tree
;;; EXAMPLES :
;;; (tree-to-root SUBTREE1) => SUBTREE1

;;; DESIGN STRATEGY : Return the given input
(define (tree-to-root tr) 
  tr)

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-to-root SUBTREE1)
   SUBTREE1
   "The given Tree is returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tree-to-sons : Tree -> ListOfTrees
;;; GIVEN : A tree
;;; RETURNS : The Tree at the root of the Tree, or its sons.
;;; EXAMPLES :
;;; (tree-to-sons TREES1) => (list SUBTREE1 SUBTREE2)

;;; DESIGN STRATEGY: Use template for Tree on tr
(define (tree-to-sons tr)
  (node-sons tr))

;;; TESTS :
(begin-for-test
  (check-equal?
   (tree-to-sons TREES1)
   (list SUBTREE1
         SUBTREE2)
   "The sons of the given Tree are returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; node-to-center : Tree -> Posn
;;; GIVEN : A Tree
;;; RETURNS : The center of the given Tree as it is to be displayed on the
;;;           scene
;;; EXAMPLES:
;;; (node-to-center TREES1) => (make-posn HALF-CANVAS-WIDTH RADIUS)

;;; DESIGN STRATEGY: Use template for Tree on tr
(define (node-to-center tr)
  (node-center tr))

;;; TESTS :
(begin-for-test
  (check-equal?
   (node-to-center TREES1)
   (make-posn HALF-CANVAS-WIDTH
              RADIUS)
   "The center of the given Tree is returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
