;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; robot.rkt
;;; On an infinite the chessboard, we have a robot and some blocks.
;;; The robot occupies a single square on the chessboard, as does each of the
;;; blocks. The robot can move any number of squares in any diagonal direction,
;;; but it can never move to or through a square occupied by a block.
;;; In this way, its behavior is like that of a bishop in chess.

(require rackunit)
(require "extras.rkt")
(check-location "08" "robot.rkt")

(provide
 path
 eval-plan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS :

;;; Position :
;;; A Position is a (list Integer Integer)

;;; INTERPRETATION :
;;; (list x y) represents the position (x,y)

;;; DESTRUCTOR TEMPLATE :
;;; pos-fn : Position -> ??
#;(define (pos-fn pos)
    (...
     (... (first pos))
     (... (second pos))))



;;; ListOfPosition (LOP) :
;;; A ListOfPosition is either one of :
;;; -- empty
;;; -- (cons Position ListOfPosition)

;;; INTERPRETATION :
;;; empty                          : a sequence of Position with no elements
;;; (cons Position ListOfPosition) : a sequence of Position whose first element is a
;;;                                  Position and whose other elements are
;;;                                  represented by ListOfPosition

;;; DESTRUCTOR TEMPLATE :
;;; lop-fn : ListOfPosition -> ??
#;(define (lop-fn lop)
    (cond
      [(empty? lop) ...]
      [else
       (... (pos-fn (first lop)) 
            (lop-fn (rest lop)))]))



;;; NEListOfPosition (NELOP) :
;;; A NEListOfPosition is one of
;;; -- (cons Position empty)
;;; -- (cons Position NEListOfPosition)

;;; INTERPRETATION :
;;; (cons Position empty)            : a sequence of ListOfPosition with 1 element
;;; (cons Position NEListOfPosition) : a sequence of ListOfPosition whose
;;;                                    first element is a ListOfPosition
;;;                                    and whose other elements are represented
;;;                                    by NELIstOfPosition

;;; DESTRUCTOR TEMPLATE :
;;; nelop-fn : NELOP -> ??
#;(define (nelop-fn nelop)
    (cond
      [(empty? (rest nelop)) ...]
      [else
       (... (lop-fn (first nelop)) 
            (nelop-fn (rest nelop)))]))



;;; ListOfListOfPosition (LLOP) :
;;; A ListOfListOfPosition is either one of :
;;; -- empty
;;; -- (cons ListOfPosition ListOfListOfPosition)

;;; INTERPRETATION :
;;; empty                                      : a sequence of ListOfPosition with no
;;;                                              elements
;;; (cons ListOfPosition ListOfListOfPosition) : a sequence of ListOfPosition whose
;;;                                              first element is a ListOfPosition
;;;                                              and whose other elements are
;;;                                              represented by ListOfListOfPosition

;;; DESTRUCTOR TEMPLATE :
;;; llop-fn : ListOfListOfPosition -> ??
#;(define (llop-fn llop)
    (cond
      [(empty? llop) ...]
      [else
       (... (lop-fn (first llop)) 
            (llop-fn (rest llop)))]))



;;; Move :
;;; A Move is a (list Direction PosInt)

;;; INTERPRETATION :
;;; Direction represents the direction of move
;;; PosInt represents the number of steps in the indicated direction

;;; DESTRUCTOR TEMPLATE :
;;; move-fn : Move -> ??
#;(define (move-fn mov)
    (...
     (dir-fn (first mov))
     (... (second move))))



;;; ListOfMove (LOM) :
;;; A ListOfMove is either one of :
;;; -- empty
;;; -- (cons Move ListOfMove)

;;; INTERPRETATION :
;;; empty                  : a sequence of Move with no elements
;;; (cons Move ListOfMove) : a sequence of Move whose first element is a Move and
;;;                          whose other elements are represented by ListOfMove

;;; DESTRUCTOR TEMPLATE :
;;; lom-fn : ListOfMove -> ??
#;(define (lom-fn lom)
    (cond
      [(empty? lom) ...]
      [else
       (... (move-fn (first lom)) 
            (lom-fn (rest lom)))]))



;;; Direction :
;;; A Direction is one of
;;; -- "ne"
;;; -- "se"
;;; -- "sw"
;;; -- "nw"

;;; INTERPRETATION :
;;; -- "ne" : represents the north-east direction
;;; -- "se" : represents the south-east direction
;;; -- "sw" : represents the south-west direction
;;; -- "nw" : represents the north-west direction

;;; DESTRUCTOR TEMPLATE :
;;; dir-fn : Direction -> ??
#;(define (dir-fn dr)
    (cond
      [(string=? "ne" dr) ...]
      [(string=? "se" dr) ...]
      [(string=? "sw" dr) ...]
      [(string=? "nw" dr) ...]))



;;; ListOfDirection (LOD) :
;;; A ListOfDirection is either one of :
;;; -- empty
;;; -- (cons Direction ListOfDirection)

;;; INTERPRETATION :
;;; empty                            : a sequence of Direction with no elements
;;; (cons Direction ListOfDirection) : a sequence of Direction whose first element
;;;                                    is a Direction and whose other elements are
;;;                                    represented by ListOfDirection

;;; DESTRUCTOR TEMPLATE :
;;; lod-fn : ListOfDirection -> ??
#;(define (lod-fn lod)
    (cond
      [(empty? lod) ...]
      [else
       (... (dir-fn (first lod)) 
            (lod-fn (rest lod)))]))



;;; Plan :
;;; A Plan is a ListOfMove (LOM)
;;; WHERE: the list does not contain two consecutive moves in the same
;;; direction.

;;; INTERPRETATION :
;;; The moves are to be executed from the first in the list to
;;; the last in the list.



;;; A MaybeX is one of
;;; -- false
;;; -- X

;;; DESTRUCTOR TEMPLATE :
;;; maybex-fn : MaybeX -> ??
#;(define (maybex-fn mx)
    (cond
      [(false? mx) ...]
      [else (... mx)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants

(define DIR-LIST-STARTING-WITH-NW (list "nw" "sw" "se" "ne"))
(define DIR-LIST-STARTING-WITH-SW (list "sw" "se" "ne" "nw"))
(define DIR-LIST-STARTING-WITH-SE (list "se" "ne" "nw" "sw"))
(define DIR-LIST-STARTING-WITH-NE (list "ne" "nw" "sw" "se"))

(define INC-DEC-FACTOR 1)

(define COUNT-START 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants for tests

(define DIR-NE "ne")
(define DIR-SE "se")
(define DIR-SW "sw")
(define DIR-NW "nw")

(define MOVE-NE (list "ne" 2))
(define MOVE-SE (list "se" 2))
(define MOVE-SW (list "sw" 2))
(define MOVE-NW (list "nw" 2))


(define BLOCK-LIST1
  '((0 3)(2 3)(4 3)
    (0 5)     (4 5)
    (0 7)(2 7)(4 7)))

(define BLOCK-LIST2
  '((0 3)(4 3)
    (0 5)(4 5)
    (0 7)(4 7)
    (0 9)(4 9)
    (0 11)(4 11)))

(define BLOCK-LIST3
  (list (list 3 0)
        (list 3 2)
        (list 3 6)
        (list 5 0)
        (list 5 4)))

(define START1 (list 2 5))

(define TARGET1 (list 4 9))
(define TARGET2 (list 3 4))
(define TARGET3 (list 0 3))
(define TARGET4 (list -1 6))

(define TARGET1-NOT-REACHABLE (list 4 6))
(define TARGET2-NOT-REACHABLE (list 1 3))

(define LEGAL-SUCCESSORS-OF-START1
  (list
   (list (list 3 6) (list 2 5))
   (list (list 1 6) (list 2 5))
   (list (list 1 4) (list 2 5))
   (list (list 3 4) (list 2 5))))

(define LIST-OF-NEXT-POS-FROM-START1
  (list
   (list 3 6)
   (list 1 6)
   (list 1 4)
   (list 3 4)))

(define LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1
  (list
   (list 4 9)
   (list 3 8)
   (list 2 9)
   (list 1 8)
   (list 0 9)
   (list -1 8)
   (list -2 7)
   (list -1 6)
   (list -2 5)
   (list -1 4)
   (list 0 3)
   (list 1 4)
   (list 2 5)))

(define POS-AFTER-MOVE-NE (list 4 7))
(define MOVE-START1-NE-BY-2 (list (list 3 6) (list 4 7)))
(define MOVE-START1-SE-BY-2 (list (list 3 4) (list 4 3)))
(define POS-AFTER-MOVE-SW (list 0 3))
(define MOVE-START1-SW-BY-2 (list (list 1 4) (list 0 3)))
(define MOVE-START1-NW-BY-2 (list (list 1 6) (list 0 7)))

(define LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1
  (list
   (list "sw" 1)
   (list "sw" 1)
   (list "nw" 1)
   (list "nw" 1)
   (list "ne" 1)
   (list "nw" 1)
   (list "ne" 1)
   (list "ne" 1)
   (list "se" 1)
   (list "ne" 1)
   (list "se" 1)
   (list "ne" 1)))

(define PLAN-FOR-START1-TARGET1-NOT-REACHABLE-EMPTY
  false)

(define PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1
  (list
   (list "sw" 2)
   (list "nw" 2)
   (list "ne" 1)
   (list "nw" 1)
   (list "ne" 2)
   (list "se" 1)
   (list "ne" 1)
   (list "se" 1)
   (list "ne" 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; path : Position Position ListOfPosition -> MaybePlan
;;; GIVEN : 1. the starting position of the robot,
;;;         2. the target position that robot is supposed to reach
;;;         3. A list of the blocks on the board
;;; RETURNS : A plan that, when executed, will take the robot from
;;;           the starting position to the target position without passing over any
;;;           of the blocks, or false if no such sequence of moves exists.
;;; EXAMPLES :
;;; (path START1 START1 empty) => empty
;;; (path START1 TARGET1-NOT-REACHABLE empty) => false
;;; (path START1 TARGET1 (rest BLOCK-LIST1))
;;;  => PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1

;;; DESIGN STRATEGY : Divide into cases based on start and target
(define (path start target lob)
  (cond
    [(reached-target? start target) empty]
    [(path-not-possible? start target) false]
    [else
     (generate-plan (generate-list-of-pos (list start) target lob))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (path START1 START1 empty)
   empty
   "It returns empty if start and target are same positions")
  (check-equal?
   (path START1 TARGET1-NOT-REACHABLE empty)
   false
   "It returns false if path doesn't exist")
  (check-equal?
   (path START1 TARGET1 (rest BLOCK-LIST1))
   PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1
   "It returns the plan from start to target"))



;;; Helper function for path
;;; reached-target? : Position Position -> Boolean
;;; GIVEN : 2 Position, where pos represents the last position reached, and
;;;         target is the final position the robot needs to reach
;;; RETURNS : True, iff pos and target are equal
;;; EXAMPLES :
;;; (reached-target? START1 START1) => true
;;; (reached-target? START1 TARGET1) => false

;;; DESIGN STRATEGY : Use template for Position on pos
(define (reached-target? pos target)
  (and (= (first pos) (first target))
       (= (second pos) (second target))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (reached-target? START1 START1)
   true
   "Since pos and target are same, target is reached")
  (check-equal?
   (reached-target? START1 TARGET1)
   false
   "Since pos and target are not same, target is not reached"))



;;; Helper function for path
;;; path-not-possible? : Position Position -> Boolean
;;; GIVEN : 2 Positions, representing the starting and target position
;;;         of robot
;;; RETURNS : True, iff there is no path possible from starting to
;;;           to target position
;;; EXAMPLES :
;;; (path-not-possible? START1 TARGET1) => false
;;; (path-not-possible? START1 TARGET1-NOT-REACHABLE) => true
;;; (path-not-possible? START1 TARGET2-NOT-REACHABLE) => true

;;; DESIGN STRATEGY : Use template for Position on src and tar
(define (path-not-possible? src tar)
  (not (or (and (even? (abs (- (first src) (first tar))))
                (even? (abs (- (second src) (second tar)))))
           (and (odd? (abs (- (first src) (first tar))))
                (odd? (abs (- (second src) (second tar))))))))

;;; TESTS :
(begin-for-test
 (check-equal?
  (path-not-possible? START1 TARGET1)
  false
  "Since path from start to target is possible, false is returned")
 (check-equal?
  (path-not-possible? START1 TARGET1-NOT-REACHABLE)
  true
  "Since path from start to target is not possible, true is returned")
 (check-equal?
  (path-not-possible? START1 TARGET2-NOT-REACHABLE)
  true
  "Since path from start to target is not possible, true is returned"))



;;; Helper function for path
;;; generate-plan : MaybeListOfPosition -> MaybePlan   
;;; GIVEN : A MaybeListOfPosition
;;; RETURNS : Maybeplan, based on values of MaybeListOfPosition
;;; EXAMPLES :
;;; (generate-plan PLAN-FOR-START1-TARGET1-NOT-REACHABLE-EMPTY) => false
;;; (generate-plan LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1)
;;;   => PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1

;;; DESIGN STRATEGY : Divide into cases based on whether path is
;;;                   false or not
(define (generate-plan path)
  (if (false? path)
      false
      (compress-count (list-of-pos-to-moves (reverse path))
                      COUNT-START)))

;;; TESTS :
(begin-for-test
  (check-equal?
   (generate-plan PLAN-FOR-START1-TARGET1-NOT-REACHABLE-EMPTY)
   false
   "Since the path is not possible, false is returned")
  (check-equal?
   (generate-plan LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1)
   PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1
   "Since the path is possible, list of moves is returned"))



;;; Helper function for generate-plan
;;; compress-count : ListOfMove PosInt -> Plan
;;; GIVEN : A sub-list sublom of lom, and PosInt count-so-far
;;; WHERE : count-so-far is the number of consecutive same directions
;;;         before sublom
;;; RETURNS : A list of moves, with all the consecutive same directions
;;;           wrapped together as a single move
;;; EXAMPLES :
;;; (compress-count LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1 COUNT-START)
;;;   => PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1

;;; DESIGN STRATEGY : Use template for ListOfMove on sublom
(define (compress-count sublom count-so-far)
  (cond
    [(empty? sublom) empty]
    [else
     (if (next-dir-same-as-curr? (first sublom) (rest sublom))
         (compress-count (rest sublom) (add1 count-so-far))
         (cons (list (first (first sublom)) count-so-far)
               (compress-count (rest sublom) COUNT-START)))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (compress-count LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1
                   COUNT-START)
   PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1
   "The list of moves, with consecutives directions wrapped in
    a single move is returned"))



;;; Helper function for compress-count
;;; next-dir-same-as-curr? : Move ListOfMoves -> Boolean
;;; GIVEN : A move and list of moves
;;; RETURNS : True, iff the list of moves is non-empty and the direction
;;;           of given move is same as the direction of first move in the
;;;           list of moves
;;; EXAMPLES :
;;; (next-dir-same-as-curr? (first LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1)
;;;                         (rest LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1))
;;;   => true
;;; (next-dir-same-as-curr? (first LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1)
;;;                         empty)
;;;   => false

;;; DESIGN STRATEGY :Use template for Move on move
(define (next-dir-same-as-curr? move lom)
  (and (not (empty? lom))
       (string=? (first move) (first (first lom)))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (next-dir-same-as-curr? (first LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1)
                           (rest LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1))
   true
   "Since the direction of given move and the first move is same,
    true is returned")
  (check-equal?
   (next-dir-same-as-curr? (first LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1)
                           empty)
   false
   "Since the given lom is empty, false is returned"))
     


;;; Helper function for compress-count
;;; list-of-pos-to-moves : ListOfPosition -> ListOfMove
;;; GIVEN : A list of position lop, which is the reversed list of
;;;         position
;;; RETURNS : A list of moves, with each direction having step 1
;;; EXAMPLES :
;;; (list-of-pos-to-moves (reverse LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1))
;;;   => LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1

;;; DESIGN STRATEGY : Use template for NEListOfPosition on lop
(define (list-of-pos-to-moves lop)
  (cond
    [(empty? (rest lop)) empty]
    [else
     (cons (list (determine-dir (first lop)
                                (first (rest lop)))
                 COUNT-START)
           (list-of-pos-to-moves (rest lop)))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-pos-to-moves (reverse LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1))
   LOP-TO-MOVES-WITH-EACH-DIR-HAVING-CT-1
   "The given list of position is converted to list of moves having
    steps as 1"))



;;; Helper function for list-of-pos-to-moves
;;; determine-dir : Position Position -> Direction
;;; GIVEN : 2 Position, representing the current and next Position
;;; RETURNS : The direction of the move from current to the next position
;;; EXAMPLES :
;;; (determine-dir START1 TARGET1) => DIR-NE
;;; (determine-dir START1 TARGET2) => DIR-SE
;;; (determine-dir START1 TARGET3) => DIR-SW
;;; (determine-dir START1 TARGET4) => DIR-NW

;;; DESIGN STRATEGY : Use template for Position on curr and next
(define (determine-dir curr next)
  (first (optimal-dir-list (- (first curr) (first next))
                           (- (second curr) (second next)))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (determine-dir START1 TARGET1)
   DIR-NE
   "Since the target is in north-east direction from start, ne is returned")
  (check-equal?
   (determine-dir START1 TARGET2)
   DIR-SE
   "Since the target is in south-east direction from start, se is returned")
  (check-equal?
   (determine-dir START1 TARGET3)
   DIR-SW
   "Since the target is in south-west direction from start, sw is returned")
  (check-equal?
   (determine-dir START1 TARGET4)
   DIR-NW
   "Since the target is in north-west direction from start, nw is returned"))



;;; Helper function for determine-dir and list-of-next-pos-from-curr
;;; optimal-dir-list : Integer Integer -> ListOfDirection
;;; GIVEN : 2 Integer, where
;;;         -- x represents the difference x coordinates of current and target
;;;            positions of robot
;;;         -- y represents the difference y coordinates of current and target
;;;            positions of robot
;;; RETURNS : A list of direction, where first element in list represents
;;;           the direction in which target is from current position of
;;;           robot, with rest directions added in anti-clockwise manner
;;;           after first
;;; EXAMPLES :
;;; (optimal-dir-list 2 -1) => DIR-LIST-STARTING-WITH-NW
;;; (optimal-dir-list 2 2) => DIR-LIST-STARTING-WITH-SW
;;; (optimal-dir-list -2 1) => DIR-LIST-STARTING-WITH-SE
;;; (optimal-dir-list -2 -1) => DIR-LIST-STARTING-WITH-NE

;;; DESIGN STRATEGY : Divide into cases based on value of x and y
(define (optimal-dir-list x y)
  (cond
    [(and (> x 0) (or (= y 0) (< y 0)))
     DIR-LIST-STARTING-WITH-NW]
    [(and (> y 0) (or (= x 0) (> x 0)))
     DIR-LIST-STARTING-WITH-SW]
    [(and (< x 0) (or (= y 0) (> y 0)))
     DIR-LIST-STARTING-WITH-SE]
    [(and (< y 0) (or (= x 0) (< x 0)))
     DIR-LIST-STARTING-WITH-NE]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (optimal-dir-list 2 -1)
   DIR-LIST-STARTING-WITH-NW
   "list with north-west direction as first element and rest in anti-clockwise
    direction is returned")
  (check-equal?
   (optimal-dir-list 2 2)
   DIR-LIST-STARTING-WITH-SW
   "list with south-west direction as first element and rest in anti-clockwise
    direction is returned")
  (check-equal?
   (optimal-dir-list -2 1)
   DIR-LIST-STARTING-WITH-SE
   "list with south-east direction as first element and rest in anti-clockwise
    direction is returned")
  (check-equal?
   (optimal-dir-list -2 -1)
   DIR-LIST-STARTING-WITH-NE
   "list with north-east direction as first element and rest in anti-clockwise
    direction is returned"))



;;; Helper function for generate-plan
;;; generate-list-of-pos : ListOfPostion Position ListOfPosition
;;;                        -> MaybeListOfPosition
;;; GIVEN : A legal list of position, target and the list of blocks
;;; RETURNS : An extension of the given list with next possible position added
;;;           to the list if it exists, else false
;;; EXAMPLES :
;;; (generate-list-of-pos (list START1) TARGET1 (rest BLOCK-LIST1))
;;;   => LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1
;;; TERMINATION ARGUMENT :
;;; - when the current path has reached the target or
;;; - there are no legal successors from the current path

;;; DESIGN STRATEGY : Recur on each legal successor of list-pos-so-far 
(define (generate-list-of-pos list-pos-so-far target lob)
  (cond
    [(reached-target? (first list-pos-so-far) target)
     list-pos-so-far]
    [else
     (first-successor
      ;;; ListOfPosition -> MaybeListOfPosition
      ;;; GIVEN : A ListOfPosition
      ;;; RETURNS : A ListOfPosition if there is any legal possible path from
      ;;;           first position in given list of position to target,
      ;;;           else false
      (lambda (list-of-pos) (generate-list-of-pos list-of-pos target lob))
      (legal-successors list-pos-so-far target lob))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (generate-list-of-pos (list START1) TARGET1 (rest BLOCK-LIST1))
   LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1
   "The list of all positions from start to target, in reverse order is returned"))



;;; Helper function for generate-list-of-pos
;;; first-successor : (ListOfPostion -> MaybeListOfPosition) ListofListOfPosition
;;;                    -> MaybeListOfPosition
;;; GIVEN : A function, which takes list-pos-so-far, as argument and returns
;;;         a MaybeListOfPosition, and list of position from whose successors
;;;         are to be determined
;;; RETURNS : A MaybeListOfPosition, which is false if there are no legal successors
;;;           from the first position in list or list of positions if there exists 
;;;           any such successors
;;; EXAMPLES :
;;; (first-successor (lambda (x) (generate-list-of-pos x TARGET1 rest BLOCK-LIST1)))
;;;                   LEGAL-SUCCESSORS-OF-START1)
;;;   => LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1

;;; DESIGN STRATEGY : Use template for ListOfPosition on lop
(define (first-successor fn-name lop)
  (cond
    [(empty? lop) false]
    [else
     (local ((define maybe-list-pos (fn-name (first lop))))
       (if (not (false? maybe-list-pos))
           maybe-list-pos
           (first-successor fn-name (rest lop))))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (first-successor (lambda (x) (generate-list-of-pos x TARGET1
                                                      (rest BLOCK-LIST1)))
                    LEGAL-SUCCESSORS-OF-START1)
   LOP-FOR-START1-TARGET1-REST-BLOCK-LIST1
   "The first-successor of the given list is found"))



;;; Helper function for generate-list-of-pos
;;; legal-successors : ListOfPosition Position ListOfPosition -> ListOfListOfPosition
;;; GIVEN : A list-pos-so-far representing all the positions the robot has
;;;         travelled so far from the start, the target position and list of
;;;         blocks
;;; RETURNS : A list of legal successors from the first position in list-pos-so-far,
;;;           if there exists any
;;; EXAMPLES :
;;; (legal-successors (list START1) TARGET1 (rest BLOCK-LIST1)) 
;;;   => LEGAL-SUCCESSORS-OF-START1

;;; DESIGN STRATEGY : Use HOF map on list of next legal positions from first position
;;;                   of list-pos-so-far and filter on list of all possible positions
;;;                   from first position of list-pos-so-far 
(define (legal-successors list-pos-so-far target lob)
  (map
   ;;; Position -> ListOfPosition
   ;;; GIVEN : A Position
   ;;; RETURNS : A ListOfPosition with the given Position
   ;;;           appended to the list-pos-so-far
   (lambda (pos) (cons pos list-pos-so-far))
   (filter
    ;;; Position -> Boolean
    ;;; GIVEN : A Position
    ;;; RETURNS : True, iff the position is not a memeber of
    ;;;           list of blocks and is not already present
    ;;;           in list-pos-so-far
    (lambda (pos) (legal-to-add-pos? pos lob list-pos-so-far))
    (list-of-next-pos-from-curr (first list-pos-so-far) target))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (legal-successors (list START1) TARGET1 (rest BLOCK-LIST1))
   LEGAL-SUCCESSORS-OF-START1
   "It returns all the legal successors of the current position if they exist"))



;;; Helper function for legal-successors
;;; list-of-next-pos-from-curr : Position Position -> ListOfPosition
;;; GIVEN : 2 Position, representing the current and target position
;;; RETURNS : A list of positions, representing the positions the robot
;;;           can move from given one
;;; WHERE : the positions are ordered based on the order of direction,
;;;         with the first position in list being nearest to target, and
;;;         rest following it in anti-clockwise manner 
;;; EXAMPLES :
;;; (list-of-next-pos-from-curr START1 TARGET1) => LIST-OF-NEXT-POS-FROM-START1

;;; DESIGN STRATEGY : Use HOP map on list of directions
(define (list-of-next-pos-from-curr src tar)
  (map
   ;;; Direction -> Position
   ;;; GIVEN : A Direction
   ;;; RETURNS : A Position after src has moved in the given direction
   ;;;           by 1 step
   (lambda (dir) (first (list-pos-after-1-move src (list dir COUNT-START))))
   (optimal-dir-list (- (first src) (first tar))
                     (- (second src) (second tar)))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-next-pos-from-curr START1 TARGET1)
   LIST-OF-NEXT-POS-FROM-START1
   "All the possible positions from source to target is returned"))



;;; Helper function for legal-successors
;;; legal-to-add-pos? : Position ListOfBlock ListOfPosition -> Boolean
;;; GIVEN : Position pos, list of blocks and list of position list-pos-so-far
;;; RETURNS : True, iff the given pos
;;;           (a) doesn't pass over block
;;;           (b) is not already a member of the list-pos-so-far
;;; EXAMPLES :
;;; (legal-to-add-pos? START1 (rest BLOCK-LIST1) (list START1)) => false
;;; (legal-to-add-pos? TARGET3 BLOCK-LIST1 (list START1)) => false
;;; (legal-to-add-pos? TARGET2 (rest BLOCK-LIST1) (list START1)) => true

;;; DESIGN STRATEGY : Combine simpler functions
(define (legal-to-add-pos? pos lob list-pos-so-far)
  (not (or (passes-over-block? (list pos) lob)
           (member? pos list-pos-so-far))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (legal-to-add-pos? START1 (rest BLOCK-LIST1) (list START1))
   false
   "Since the given pos is already present in the list-pos-so-far, false
    is returned")
  (check-equal?
   (legal-to-add-pos? TARGET3 BLOCK-LIST1 (list START1))
   false
   "Since the given pos is member of list of blocks, false is returned")
  (check-equal?
   (legal-to-add-pos? TARGET2 (rest BLOCK-LIST1) (list START1))
   true
   "Since the given pos is neither a member of list of blocks, nor already
    present in list-pos-so-far false is returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General function

;;; list-of-pos-after-dir : Integer Integer ((Integer Integer ...+) -> Integer)
;;;                         ((Integer Integer ...+) -> Integer) NonNegInt
;;;                         -> ListOfPosition
;;; GIVEN : 2 Integers, representing the x and y coordinate of current
;;;         position of robot, 2 functions, to be performed on x and values
;;;         respectively and a NonNegInt indicating the number of steps
;;; RETURNS : A list of positions, containing all the positions the robot has
;;;           moved in specified direction for the given steps
;;; HALTING MEASURE : ct
;;; TERMINATION ARGUEMENT :
;;; - ct is the steps of direction in a move, which is a PosInt by the contract
;;; - at each recursive call, ct-1 is a non-negative integer, and it is strictly
;;;   less than the value of ct. So ct decreases at each recursive call

;;; DESIGN STRATEGY : Divide into cases on ct
(define (list-of-pos-after-dir x y x-op y-op ct)
  (cond
    [(= ct 0) empty]
    [else
     (local
       ((define new-x (x-op x INC-DEC-FACTOR))
        (define new-y (y-op y INC-DEC-FACTOR))
        (define new-ct (- ct INC-DEC-FACTOR)))
        (cons (list new-x new-y)
              (list-of-pos-after-dir new-x
                                     new-y
                                     x-op y-op
                                     new-ct)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; eval-plan : Position ListOfPosition Plan ->  MaybePosition
;;; GIVEN : 1. the starting position of the robot,
;;;         2. A list of the blocks on the board
;;;         3. A plan for the robot's motion
;;; RETURNS : The position of the robot at the end of executing the plan, or false
;;;           if  the plan sends the robot to or  through any block
;;; EXAMPLES :
;;; (eval-plan START1 (rest BLOCK-LIST1) PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1)
;;;   => TARGET1
;;; (eval-plan START1 BLOCK-LIST1 PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1)
;;;   => false

;;; DESIGN STRATEGY : Use template for Plan on plan
(define (eval-plan pos lob plan)
  (cond
    [(empty? plan) pos]
    [else
     (if (passes-over-block? (list-pos-after-1-move pos (first plan))
                             lob)
         false
         (eval-plan (next-pos-after-1-move pos (first plan))
                    lob
                    (rest plan)))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (eval-plan START1 (rest BLOCK-LIST1) PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1)
   TARGET1
   "The position after executing all the steps is returned")
  (check-equal?
   (eval-plan START1 BLOCK-LIST1 PLAN-FOR-START1-TARGET1-REST-BLOCK-LIST1)
   false
   "Since the plan passes over blocks, false is returned"))



;;; Helper function for eval-plan
;;; list-pos-after-1-move : Position Move -> ListOfPosition
;;; GIVEN : A Position pos and a move
;;; RETURNS : A list of position, with the given move performed on
;;;           the given position, where the list consists of all
;;;           positions the robot will be from pos, after moving in given
;;;           direction for specified number of steps
;;; EXAMPLES :
;;; (list-pos-after-1-move START1 MOVE-NE) => MOVE-START1-NE-BY-2
;;; (list-pos-after-1-move START1 MOVE-SE) => MOVE-START1-SE-BY-2
;;; (list-pos-after-1-move START1 MOVE-SW) => MOVE-START1-SW-BY-2
;;; (list-pos-after-1-move START1 MOVE-NW) => MOVE-START1-NW-BY-2

;;; DESIGN STRATEGY : Use template for Direction on (first move)
(define (list-pos-after-1-move pos move)
  (cond
    [(string=? "ne" (first move))
     (list-of-pos-after-ne (first pos) (second pos) (second move))]
    [(string=? "se" (first move))
     (list-of-pos-after-se (first pos) (second pos) (second move))]
    [(string=? "sw" (first move))
     (list-of-pos-after-sw (first pos) (second pos) (second move))]
    [(string=? "nw" (first move))
     (list-of-pos-after-nw (first pos) (second pos) (second move))]))

;;; TESTS :
(begin-for-test
  (check-equal?
   (list-pos-after-1-move START1 MOVE-NE)
   MOVE-START1-NE-BY-2
   "It will return a list will all the positions the given pos will be at
    on executing the given move")
  (check-equal?
   (list-pos-after-1-move START1 MOVE-SE)
   MOVE-START1-SE-BY-2
   "It will return a list will all the positions the given pos will be at
    on executing the given move")
  (check-equal?
   (list-pos-after-1-move START1 MOVE-SW)
   MOVE-START1-SW-BY-2
   "It will return a list will all the positions the given pos will be at
    on executing the given move")
  (check-equal?
   (list-pos-after-1-move START1 MOVE-NW)
   MOVE-START1-NW-BY-2
   "It will return a list will all the positions the given pos will be at
    on executing the given move"))



;;; Helper function for eval-plan
;;; passes-over-block? : ListOfPosition ListOfPosition -> Boolean
;;; GIVEN : A list of positions list-of-pos and list of blocks
;;; RETURNS : True, if pos is equal to any element in the given
;;;           list of blocks
;;; EXAMPLES :
;;; (passes-over-block? LIST-OF-NEXT-POS-FROM-START1 BLOCK-LIST1) => false
;;; (passes-over-block? LIST-OF-NEXT-POS-FROM-START1 BLOCK-LIST3) => true

;;; DESIGN STRATEGY : Use HOF ormap on lob
(define (passes-over-block? list-of-pos lob)
  (ormap
   ;;; Position -> Boolean
   ;;; GIVEN : A Position
   ;;; RETURNS : True, iff the given position is a member
   ;;;           of list of blocks
   (lambda (pos) (member? pos lob))
   list-of-pos))

;;; TESTS :
(begin-for-test
  (check-equal?
   (passes-over-block? LIST-OF-NEXT-POS-FROM-START1 BLOCK-LIST1)
   false
   "Since the positions in the given list are not member of lob, false
    is returned")
  (check-equal?
   (passes-over-block? LIST-OF-NEXT-POS-FROM-START1 BLOCK-LIST3)
   true
   "Since one positions in the given list is a member of lob, true
    is returned"))



;;; Helper function for eval-plan
;;; next-pos-after-1-move : Position Move
;;; GIVEN : A Position and a Move
;;; RETURNS : The next position after given one, after executing the
;;;           given move on it
;;; EXAMPLES :
;;; (next-pos-after-1-move START1 MOVE-NE) => POS-AFTER-MOVE-NE
;;; (next-pos-after-1-move START1 MOVE-SW) => POS-AFTER-MOVE-SW

;;; DESIGN STRATEGY : Combine simpler functions
(define (next-pos-after-1-move pos move)
  (first (reverse (list-pos-after-1-move pos move))))

;;; TESTS :
(begin-for-test
  (check-equal?
   (next-pos-after-1-move START1 MOVE-NE)
   POS-AFTER-MOVE-NE
   "The position pos will be after executing the given move is returned")
  (check-equal?
   (next-pos-after-1-move START1 MOVE-SW)
   POS-AFTER-MOVE-SW
   "The position pos will be after executing the given move is returned"))



;;; Helper function for list-pos-after-1-move
;;; list-of-pos-after-ne : Integer Integer PosInt -> ListOfPosition
;;; GIVEN : 2 Integers, representing the x and y coordinate of current
;;;         position of robot, and a PosInt indicating the number of steps
;;; WHERE : the robot has to move in north-east direction
;;; RETURNS :  A list of positions, containing all the positions the robot has
;;;           moved in north-east direction for the given steps
;;; EXAMPLES :
;;; (list-of-pos-after-ne 2 5 2) => MOVE-START1-NE-BY-2

;;; DESIGN STRATEGY : Call a General function list-of-pos-after-dir
(define (list-of-pos-after-ne x y ct)
  (list-of-pos-after-dir x y + + ct))

;;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-pos-after-ne 2 5 2)
   MOVE-START1-NE-BY-2
   "The list of all positions after traversing in NE direction is returned"))



;;; Helper function for list-pos-after-1-move
;;; list-of-pos-after-se : Integer Integer PosInt -> ListOfPosition
;;; GIVEN : 2 Integers, representing the x and y coordinate of current
;;;         position of robot, and a PosInt indicating the number of steps
;;; WHERE : the robot has to move in south-east direction
;;; RETURNS :  A list of positions, containing all the positions the robot has
;;;           moved in south-east direction for the given steps
;;; EXAMPLES :
;;; (list-of-pos-after-se 2 5 2) => MOVE-START1-SE-BY-2

;;; DESIGN STRATEGY : Call a General function list-of-pos-after-dir
(define (list-of-pos-after-se x y ct)
  (list-of-pos-after-dir x y + - ct))

;;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-pos-after-se 2 5 2)
   MOVE-START1-SE-BY-2
   "The list of all positions after traversing in SE direction is returned"))



;;; Helper function for list-pos-after-1-move
;;; list-of-pos-after-sw : Integer Integer PosInt -> ListOfPosition
;;; GIVEN : 2 Integers, representing the x and y coordinate of current
;;;         position of robot, and a PosInt indicating the number of steps
;;; WHERE : the robot has to move in south-west direction
;;; RETURNS :  A list of positions, containing all the positions the robot has
;;;           moved in south-west direction for the given steps
;;; EXAMPLES :
;;; (list-of-pos-after-sw 2 5 2) => MOVE-START1-SW-BY-2

;;; DESIGN STRATEGY : Call a General function list-of-pos-after-dir
(define (list-of-pos-after-sw x y ct)
  (list-of-pos-after-dir x y - - ct))

;;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-pos-after-sw 2 5 2)
   MOVE-START1-SW-BY-2
   "The list of all positions after traversing in SW direction is returned"))



;;; Helper function for list-pos-after-1-move
;;; list-of-pos-after-nw : Integer Integer PosInt -> ListOfPosition
;;; GIVEN : 2 Integers, representing the x and y coordinate of current
;;;         position of robot, and a PosInt indicating the number of steps
;;; WHERE : the robot has to move in north-west direction
;;; RETURNS :  A list of positions, containing all the positions the robot has
;;;           moved in north-west direction for the given steps
;;; EXAMPLES :
;;; (list-of-pos-after-nw 2 5 2) => MOVE-START1-NW-BY-2

;;; DESIGN STRATEGY : Call a General function list-of-pos-after-dir
(define (list-of-pos-after-nw x y ct)
  (list-of-pos-after-dir x y - + ct))

;;; TESTS :
(begin-for-test
  (check-equal?
   (list-of-pos-after-nw 2 5 2)
   MOVE-START1-NW-BY-2
   "The list of all positions after traversing in NW direction is returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
