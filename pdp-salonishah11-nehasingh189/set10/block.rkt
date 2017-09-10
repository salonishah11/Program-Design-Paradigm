;; block.rkt
;; This file contains Block% class.
;; It is included in cubelets.rkt, playgroundstate_cubelets.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(require "interfaces_cubelets.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 Block%
 make-block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class Block%
;; Interface : Block<%>
;; A Block is an object of class Block%
;; A Block is (new Block% [x Int] [y Int]
;;                        [mx-dist Int] [my-dist Int]
;;                        [selected? Boolean]
;;                        [teammates LIstOfBlock<%>)

(define Block%
  (class* object% (Block<%>)

    ; x and y coordinates of Block
    (init-field x y)

    ; List of all the blocks in world minus the
    ; current block
    (init-field blocks-in-world)

    ; The distance of x and y from mx and my
    ; Default value is 0
    (init-field [mx-dist 0] [my-dist 0])

    ; Indicates whether the Block is selected or not
    ; Default value is false
    (init-field [selected? false])

    ; List of teammates of the given Block
    ; Default value is empty
    (init-field [teammates empty])
    

    ; Private data for objects of this class

    ; Width of the cubelet
    (field [CUBELET-WIDTH 20])

    ; Half of width of the cubelet
    (field [HALF-CUBELET-WIDTH (/ CUBELET-WIDTH 2)])

    ; Image of Selected cubelet
    (field [SEL-CUBELET (square CUBELET-WIDTH "outline" "red")])

    ; Image of Unselected cubelet
    (field [UNSEL-CUBELET (square CUBELET-WIDTH "outline" "green")])

    
    (super-new)


    ; Methods of Block<%>

    ; get-team : -> ListOfBlock<%>
    ; GIVEN : No arguments
    ; RETURNS : The teammates of this block
    (define/public (get-team) teammates)


    ; add-teammate : Block<%> -> Void
    ; GIVEN : An object whose class implements Block<%>
    ; EFFECT : Adds the given block to this block's team
    (define/public (add-teammate block)
      (begin
        (set! teammates (set-union (set-minus (cons block (send block get-team))
                                              this)
                                   teammates))
        ; Updates teammates of each block present in teammates of this block
        (update-teammates-of-block)))

    
    ; block-x : -> Int
    ; GIVEN : No arguments
    ; RETURNS : x coordinate of this block
    (define/public (block-x) x)

    
    ; block-y : -> Int
    ; GIVEN : No arguments
    ; RETURNS : y coordinate of this block
    (define/public (block-y) y)

    
    ; update-x-y-of-block : Int Int -> Void
    ; GIVEN : The distance the selected block has moved i.e. the
    ;         difference of orginal position and position after dragging
    ;         the selected block
    ; EFFECT : Updated block with block moved by the given difference
    (define/public (update-x-y-of-block diff-x diff-y)
      (begin
        (set! x (+ x diff-x))
        (set! y (+ y diff-y))))

    
    ; add-teammates-of-block : ListOfBlock<%> -> Void
    ; GIVEN : Teammates of the block that called the function
    ; EFFECT : Updates the teammates of this block by appending
    ;          the blocks which are not present in teammates from given list 
    (define/public (add-teammates-of-block lst-sobj)
      (set! teammates (set-union teammates 
                                 (set-diff lst-sobj teammates))))

    
    ; update-blocks-in-world : ListOfBlock<%> -> Void
    ; GIVEN : List of blocks in the world
    ; EFFECT : Updated value for field blocks-in-world
    (define/public (update-blocks-in-world lst-blocks)
      (set! blocks-in-world (set-minus lst-blocks this)))


    ; Methods of SWidget<%>

    ; after-tick -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the block to the state it should have
    ;          following a tick
    ; DETAILS : Block doesn't change at next tick
    (define/public (after-tick ) this)


    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN : An x and a y coordinate
    ; EFFECT : Updates the block to the state it should have
    ;          following the button down mouse event at the given
    ;          location
    (define/public (after-button-down mx my)
      (begin
        (set! mx-dist (- mx x))
        (set! my-dist (- my y))
        (set! selected? (inside-cubelet? mx my))))
    
    
    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN : An x and y coordinate
    ; EFFECT : Updates the block to the state it should have
    ;          following the drag mouse event at the given
    ;          location
    ; DESIGN STRATEGY : Divide into cases based on whether block
    ;                   is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (local
            ((define old-x x)
             (define old-y y))
             (begin
               (set! x (- mx mx-dist))
               (set! y (- my my-dist))
               ; moves the teammates based on movement of this block
               (update-x-y-of-teammates (- x old-x) (- y old-y))
               (local
                 (; List of all blocks that intersect with this block)
                  (define intersected-blocks (list-of-intersected-blocks teammates)))
                 ; Adds each block and its teammates of intersected-blocks to
                 ; teammates of this block
                 (for-each
                  ; Block<%> -> Void
                  ; GIVEN : An object of class that implements Block<%>
                  ; EFFECT : Updates the teammates of this block
                  (lambda (sobj) (add-teammate sobj))
                  intersected-blocks))))
          this))                                 


    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : An x and a y coordinate
    ; EFFECT : Updates this block to the state it should have
    ;          following the button up mouse event at the given
    ;          location
    (define/public (after-button-up mx my)
      (set! selected? false))


    ; after-key-event : KeyEvent -> Void
    ; GIVEN : A key event
    ; EFFECT : Updates this block to the state it should have
    ;          following the given key event
    ; DETAILS : Block ignores the key events
    (define/public (after-key-event ke) this)
 

    ; Scene -> Scene
    ; GIVEN : A scene
    ; RETURNS : A scene like the given one, but with this block
    ;           painted on it
    ; DESIGN STRATEGY : Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image (if selected? SEL-CUBELET UNSEL-CUBELET)
                   x y
                   scene)) 


    ; Local functions

    ; inside-cubelet? : NonNegInt NonNegInt -> Boolean
    ; GIVEN : Mouse location
    ; RETURNS : True iff the given mouse is inside this block
    ; DESIGN STRATEGY : Combine simpler functions
    (define (inside-cubelet? mx my)
      (and (<= (- x HALF-CUBELET-WIDTH)
               mx
               (+ x HALF-CUBELET-WIDTH))
           (<= (- y HALF-CUBELET-WIDTH)
               my
               (+ y HALF-CUBELET-WIDTH))))


    ; update-x-y-of-teammates : Int Int -> Void
    ; GIVEN : The distance this block has moved i.e. the difference of
    ;         orginal position and position after dragging this block
    ; EFFECT : Updated teammates with each in list block moved by
    ;          the given difference
    (define (update-x-y-of-teammates diff-x diff-y)
      (for-each
       ; Block<%> -> Void
       ; GIVEN : An object of class that implements Block<%>
       ; EFFECT : Updated block after updating the x and y coordinates
       ;          of block based on the difference of new and old
       ;          positions of this block
       (lambda (sobj) (send sobj update-x-y-of-block diff-x diff-y))
       teammates))


    ; list-of-intersected-blocks : ListOfBlock<%> -> ListOfBlock<%>
    ; GIVEN : Teammates of this block
    ; RETURNS : List of blocks that are intersecting with this block
    ; DESIGN STRATEGY : Use HOF filter on (set-diff blocks-in-world old-tm)
    (define (list-of-intersected-blocks old-tm)
      (filter
       (lambda (sobj) (intersects? (send sobj block-x) (send sobj block-y)))
       (set-diff blocks-in-world old-tm)))


    ; intersects? : Int Int -> Boolean
    ; GIVEN : x and y coordinates of the block with whom the intersection
    ;         of this block is to be checked
    ; RETURNS : True, iff this block and block with center at given
    ;           x and y coordinates intersect
    ; DESIGN STRATEGY : Combine simpler functions
    (define (intersects? other-x other-y)
      (local
        ((define x1 (- other-x 10))
         (define x2 (- x 10))
         (define y1 (- other-y 10))
         (define y2 (- y 10)))
         (not (or (< (+ x1 CUBELET-WIDTH) x2)
                  (< (+ x2 CUBELET-WIDTH) x1)
                  (< (+ y1 CUBELET-WIDTH) y2)
                  (< (+ y2 CUBELET-WIDTH) y1)))))


    ; update-teammates-of-block : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the teammates of each block present in this
    ;          block's teammates
    (define (update-teammates-of-block)
      (for-each
       ; Block<%> -> Void
       ; GIVEN : A stateful object whose class implements Block<%>
       ; EFFECT : Adds the teammantes of this block to given object 
       (lambda (sobj) (send sobj add-teammates-of-block
                            (set-minus (cons this teammates) sobj)))
       teammates))


    ; Functions for Testing

    ; for-test:selected? : -> Boolean
    ; GIVEN : No arguments
    ; RETURNS : The value of selected? of this block
    (define/public (for-test:selected?) selected?)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-block : NonNegInt NonNegInt ListOfBlock<%> -> Block<%>
;; GIVEN : An x and y position, and a list of blocks
;; WHERE : the list of blocks is the list of blocks already on the playground
;; RETURNS : A new block, at the given position, with no teammates
(define (make-block x y blocks)
  (new Block%
       [x x]
       [y y]
       [blocks-in-world blocks]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS :
(begin-for-test
  (local
    ((define BLOCK-1 (make-block 250 300 empty))
     (define BLOCK-2 (make-block 280 300 (list BLOCK-1)))
     (define BLOCK-3 (make-block 291 300 (list BLOCK-1 BLOCK-2)))

     (define BLOCK-4 (make-block 400 400 (list BLOCK-1 BLOCK-2 BLOCK-3)))
     (define BLOCK-5 (make-block 400 420 (list BLOCK-1 BLOCK-2 BLOCK-3 BLOCK-4)))

     (define BLOCK-6 (make-block 200 200
                                     (list BLOCK-1 BLOCK-2 BLOCK-3 BLOCK-4 BLOCK-5)))

     (define BLOCK-7
       (make-block 225 200
                   (list BLOCK-1 BLOCK-2 BLOCK-3 BLOCK-4 BLOCK-5 BLOCK-6)))

     (define EMPTY-CANVAS (empty-scene 500 600))

     ; Image of unselected BLOCK5
     (define UNSEL-BLOCK5-IMAGE
       (place-image (square 20 "outline" "green")
                    400 420
                    EMPTY-CANVAS))

     ; Image of selected BLOCK1
     (define SEL-BLOCK1-IMAGE
       (place-image (square 20 "outline" "red")
                    250 300
                    EMPTY-CANVAS))

     ; List of blocks in world
     (define BLOCKS-IN-WORLD
       (list BLOCK-1 BLOCK-2 BLOCK-3 BLOCK-4 BLOCK-5 BLOCK-6 BLOCK-7))

     ; Updates Block1's blocks-in-world
    (define BLOCK1-list-of-blocks
      (send BLOCK-1 update-blocks-in-world BLOCKS-IN-WORLD))

    ; Updates Block2's blocks-in-world
    (define BLOCK2-list-of-blocks
    (send BLOCK-2 update-blocks-in-world BLOCKS-IN-WORLD))

    ; Updates Block6's blocks-in-world
    (define BLOCK6-list-of-blocks
      (send BLOCK-6 update-blocks-in-world BLOCKS-IN-WORLD))


    ; Mouse events on Block 3
    ; Button down on Block 3 at (291, 300)
    (define BLOCK3-BUTTON-DOWN
      (send BLOCK-3 after-button-down 291 300))

    ; Dragging Block 3 to (290, 300)
    (define BLOCK3-DRAG (send BLOCK-3 after-drag 290 300))

    ; Button up on Block 3 at (290, 300)
    (define BLOCK3-BUTTON-UP (send BLOCK-3 after-button-up 290 300))

    
    ; Adds Block 5 to teammates of Block 4
    (define BLOCK4-TEAMMATE (send BLOCK-4 add-teammate BLOCK-5)))

    ; Checks the image of Block 5 after placing it on Canvas
    (check-equal?
     (send BLOCK-5 add-to-scene EMPTY-CANVAS)
     UNSEL-BLOCK5-IMAGE
     "The image of BLOCK-5 on canvas of (500, 600)")

    ; Checks the number of teammates of Block 2
    (check-equal?
     (length (send BLOCK-2 get-team))
     1
     "The teammates of BLOCK-2 consists of BLOCK-3")

    ; Checks the number of teammates of Block 4
    (check-equal?
     (length (send BLOCK-4 get-team))
     1
     "The teammate of BLOCK-4 is BLOCK-5")

    ; Mouse events on Block 1
    ; Button down on Block 1 at (250, 300)
    (send BLOCK-1 after-button-down 250 300)

    ; Checks the selected? property of Block 1
    (check-equal?
     (send BLOCK-1 for-test:selected?)
     true
     "Since the mouse coordinates (250, 300) is inside cubelet, it
      is selected")

    ; Checks the image of selected Block 1 after placing it on Canvas
    (check-equal?
     (send BLOCK-1 add-to-scene EMPTY-CANVAS)
    SEL-BLOCK1-IMAGE
    "Image of BLOCK-1 on canvas of (500, 600)")

    ; Dragging Block 1 to (260, 300)
    (send BLOCK-1 after-drag 260 300)
    ; Button up on Block 1 at (260, 300)
    (send BLOCK-1 after-button-up 260 300)
     

    ; Checks the number of teammates of Block 1
    (check-equal?
     (length (send BLOCK-1 get-team))
     2
     "The teammates of BLOCK-1 consist of BLOCK-2 and BLOCK-3")

    ; Checks the x-coordinate of Block 1
    (check-equal?
     (send BLOCK-1 block-x)
     260
     "The x coordinate of BLOCK-1 is 260")

    ; Checks the y-coordinate of Block 1
    (check-equal?
     (send BLOCK-1 block-y)
     300
     "The y coordinate of BLOCK-1 is 300")

    ; After tick event on Block 3
    (send BLOCK-3 after-tick)

    ; Checks the x-coordinate of Block 3 after tick
    (check-equal?
     (send BLOCK-3 block-x)
     290
     "The block doesn't change at next tick")

    ; Mouse events on Block 1
    ; Button down on Block 1 at (260, 300)
    (send BLOCK-1 after-button-down 260 300)
    ; Dragging Block 1 to (265, 300)
    (send BLOCK-1 after-drag 265 300)
    ; Button up on Block 1 at (265, 300)
    (send BLOCK-1 after-button-up 265 300)

    ; Checks x-coordinate of Block 2
    ; Since it is teammate of Block 1, it is also dragged
    (check-equal?
     (send BLOCK-2 block-x)
     285
     "The x coordinate of BLOCK-2 after dragging BLOCK-1 is 285")

    ; Checks y-coordinate of Block 2
    ; Since it is teammate of Block 1, it is also dragged
    (check-equal?
     (send BLOCK-2 block-y)
     300
     "The y coordinate of BLOCK-2 after dragging BLOCK-1 is 300")

    ; Key event
    (send BLOCK-2 after-key-event "b")

    ; Mouse event on Block 2
    ; Button down on Block 2 at (300, 300)
    (send BLOCK-2 after-button-down 300 300)
    ; Dragging Block 2 to (301, 301)
    (send BLOCK-2 after-drag 301 301)

    ; Checks the selected? property of Block 2
    (check-equal?
     (send BLOCK-2 for-test:selected?)
     false
     "Since the mouse coordinates (300, 300) is not inside cubelet, it
      is not selected")
    
    ; Mouse events on BLOCK-6
    ; Button down on BLOCK-6 at (200, 200)
    (send BLOCK-6 after-button-down 200 200)
    ; Dragging BLOCK-6 to (204, 200)
    (send BLOCK-6 after-drag 204 200)
    ; Dragging BLOCK-6 to (200, 200)
    (send BLOCK-6 after-drag 200 200)
    ; Button up on BLOCK-6 at (200, 200)
    (send BLOCK-6 after-button-up 200 200)

    ; Checks the teammates of BLOCK-6
    (check-equal?
     (length (send BLOCK-6 get-team))
     0
     "Since BLOCK-6 didn't intersect with BLOCK-7, it will not be added
      to its teammates")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;