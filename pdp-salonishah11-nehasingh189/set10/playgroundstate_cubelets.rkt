;; playgroundstate_cubelets.rkt
;; The file contains the PlaygroundState% class.
;; It is included in cubelets.rkt

#lang racket
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "block.rkt")
(require "interfaces_cubelets.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide PlaygroundState%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class PlaygroundState%
;; Interface : SWidget<%>
;; PlaygroundState is an object of class PlaygroundState%
;; A PlaygroundState is (new PlaygroundState% [x Int]
;;                                            [y Int]
;;                                            [blocks ListOfBlock<%>])

(define PlaygroundState%
  (class* object% (SWidget<%>)

    ; x and y coordinates of last button-up
    ; Default value is center of the canvas
    (init-field [x-pos 10] [y-pos 10])
    
    ; ListOfBlock<%> in the PlaygroundState
    ; By default the list is empty
    (init-field [blocks empty])
    

    ; Private data for objects of this class
    
    ; Key events
    (field [NEW-BLOCK "b"])
    

    (super-new)


    ; Methods of SWidget<%>

    ; after-tick : -> Void
    ; GIVEN : No arguments
    ; EFFECT : Updates the state of the playground at the next tick
    ; DETAILS : The playground doesn't change at the next tick
    (define/public (after-tick) this)


    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : Updates the state of the playground after button down mouse event
    ; DESIGN STRATEGY : Use of for-each on blocks
    (define/public (after-button-down mx my)
      (process-blocks
       ; Block<%> -> Void
       ; GIVEN : Object of class that implements Block<%>
       ; EFFECT : Object of class that implements Block<%>, with
       ;          button down event performed on it
       (lambda (sobj) (send sobj after-button-down mx my))))


    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : The state of the playground after drag mouse event
    ; DESIGN STRATEGY : Use of for-each on blocks
    (define/public (after-drag mx my)
      (process-blocks
       ; Block<%> -> Void
       ; GIVEN : Object of class that implements Block<%>
       ; EFFECT : Object of class that implements Block<%>, with
       ;          drag event performed on it
       (lambda (sobj) (send sobj after-drag mx my))))


    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN : A mouse location
    ; EFFECT : The state of the playground after button up mouse event
    ; DESIGN STRATEGY : Use of for-each on blocks
    (define/public (after-button-up mx my)
      (begin
        (set! x-pos mx)
        (set! y-pos my)
        (process-blocks
         ; Block<%> -> Void 
         ; GIVEN : Object of class that implements Block<%>
         ; EFFECT : Object of class that implements Block<%>, with
         ;          button up event performed on it
         (lambda (sobj) (send sobj after-button-up mx my)))))


     
    ; after-key-event : KeyEvent -> Void
    ; GIVEN : A key event
    ; EFFECT : The state of the playground that should follow the
    ;          given key event
    ; DESIGN STRATEGY : Divide into cases based on the key event ke
    (define/public (after-key-event ke)
      (cond
        [(key=? ke NEW-BLOCK)
         (begin
           (set! blocks (cons (make-block x-pos y-pos blocks) blocks))
           (process-blocks
            ; Block<%> -> Void
            ; GIVEN : An object of class that implements Block<%>
            ; EFFECT : Updates the list of blocks in given block
            (lambda (sobj) (send sobj update-blocks-in-world blocks))))]
        [else this]))


    ; add-to-scene : Scene -> Scene
    ; GIVEN : Scene
    ; RETURNS : A scene, like the orginal one but with this playground
    ;           painted on it
    ; DESIGN STRATEGY : Use HOF foldr on blocks
    (define/public (add-to-scene scene)
      (foldr
       ; Block<%> Scene ->  Scene
       ; GIVEN : Object of class that implements Block<%> and
       ;         a scene
       ; RETURNS : A scene like the given one, but with the given
       ;           block painted on it
       (lambda (sobj scene) (send sobj add-to-scene scene))
       scene
       blocks))

    ; Local functions

    ; process-blocks : (Block<%> -> Void) -> Void
    ; GIVEN : A function which takes an object of class that implements
    ;         Block<%> and effects the block based on function to be
    ;         implemented
    ; EFFECT : Updates each block in blocks based on the given function
    (define (process-blocks fn)
      (for-each fn blocks))


    ; Functions for testing

    ; for-test:blocks : -> ListOfBlock<%>
    ; GIVEN : No arguments
    ; RETURNS : The list of blocks in the playground state
    (define/public (for-test:blocks) blocks)

    ; for-test:block-x : Block<%> -> Int
    ; GIVEN : An object of the class that implemets Block<%>
    ; RETURNS : The x coordinate of the given block
    (define/public (for-test:block-x block) (send block block-x))

    ; for-test:block-y : Block<%> -> Int
    ; GIVEN : An object of the class that implemets Block<%>
    ; RETURNS : The y coordinate of the given block
    (define/public (for-test:block-y block) (send block block-y))

    ; for-test:teammates-of-block : Block<%> -> ListOfBlock<%>
    ; GIVEN : An object of the class that implemets Block<%>
    ; RETURNS : The teammates of the given block
    (define/public (for-test:teammates-of-block block) (send block get-team))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS :
(begin-for-test
  (local
    ((define BLOCK-1 (make-block 250 300 empty))
     (define BLOCK-2 (make-block 280 300 (list BLOCK-1)))
     
     (define BLOCK-3 (make-block 400 399 (list BLOCK-1 BLOCK-2)))
     (define BLOCK-4 (make-block 400 420 (list BLOCK-1 BLOCK-2 BLOCK-3)))

     (define EMPTY-CANVAS (empty-scene 500 600))

     ; List of blocks in world
     (define BLOCKS-IN-WORLD (list BLOCK-1 BLOCK-2 BLOCK-3 BLOCK-4 ))

     ; Object of PlaygroundState% having no blocks
     (define PLAYGROUND-1 (new PlaygroundState%))

     ; Image of Playground containing BLOCK-1 at (260, 300) after drag
     (define PLAYGROUND-WITH-BLOCK1-IMAGE (place-image (square 20 "outline" "green")
                                                       260 300
                                                       EMPTY-CANVAS))

     ; Image of Playground containing BLOCK-1 and BLOCK-2
     (define PLAYGROUND1-IMAGE (place-image (square 20 "outline" "green")
                                            280 300
                                            PLAYGROUND-WITH-BLOCK1-IMAGE)))

    ; Mouse events
    ; Button down at (250, 300)
    (send PLAYGROUND-1 after-button-down 250 300)
    ; Button up at (250, 300)
    (send PLAYGROUND-1 after-button-up 250 300)

    ; Key event "b"
    ; Creating block 1
    (send PLAYGROUND-1 after-key-event "b")

    ; Checks the number of blocks in Playground-1
    (check-equal?
     (length (send PLAYGROUND-1 for-test:blocks))
     1
     "After first key event, the playground consists of 1 block, BLOCK-1")

    ; Mouse events
    ; Button down at (280, 300)
    (send PLAYGROUND-1 after-button-down 280 300)
    ; Button up at (280, 300)
    (send PLAYGROUND-1 after-button-up 280 300)

    ; Key event "b"
    ; Creating block 2
    (send PLAYGROUND-1 after-key-event "b")

    ; Checks the number of blocks in Playground-1
    (check-equal?
     (length (send PLAYGROUND-1 for-test:blocks))
     2
     "After second key event, the playground consists of 2 blocks, BLOCK-1
      and BLOCK-2")

    ; Mouse events
    ; Button down at (250, 300)
    (send PLAYGROUND-1 after-button-down 250 300)
    ; Dragging BLOCK-1 to (260, 300)
    (send PLAYGROUND-1 after-drag 260 300)
    ; Button up at (260, 300)
    (send PLAYGROUND-1 after-button-up 260 300)

    ; Checks whether 2 blocks intersects, and forms teammates
    (check-equal?
     (length (send PLAYGROUND-1 for-test:teammates-of-block
                   (first (send PLAYGROUND-1 for-test:blocks))))
     1
     "After dragging BLOCK-1 to (260, 300), it intersects with BLOCK-2, which
      becomes it's teammate")

    ; Checks whether 2 blocks intersects, and forms teammates
    (check-equal?
     (length (send PLAYGROUND-1 for-test:teammates-of-block
                   (second (send PLAYGROUND-1 for-test:blocks))))
     1
     "After dragging BLOCK-1 to (260, 300), it intersects with BLOCK-2, which
      becomes it's teammate")

    ; Key event "s"
    ; Does not affect PLAYGROUND-1
    (send PLAYGROUND-1 after-key-event "s")

    ; Checks the number of blocks in Playground-1, after key event other
    ; than "b"
    (check-equal?
     (length (send PLAYGROUND-1 for-test:blocks))
     2
     "After 's' key event, the playground will consists of 2 blocks, BLOCK-1
      and BLOCK-2")

    ; After tick event
    (send PLAYGROUND-1 after-tick)

    ; Checks the x-ccordinate of BLOCK-2 after tick event
    ; Its value remains same as before
    (check-equal?
     (send PLAYGROUND-1 for-test:block-x
           (first (send PLAYGROUND-1 for-test:blocks)))
     280
     "No blocks after affected by after-tick event")

    ; Checks the y-ccordinate of BLOCK-2 after tick event
    ; Its value remains same as before
    (check-equal?
     (send PLAYGROUND-1 for-test:block-y
           (first (send PLAYGROUND-1 for-test:blocks)))
     300
     "No blocks after affected by after-tick event")

    ; Checks the image of PLAYGROUND-1 containing BLOCK-1 and BLOCK-2
    (check-equal?
     (send PLAYGROUND-1 add-to-scene EMPTY-CANVAS)
     PLAYGROUND1-IMAGE
     "Image of Playground containing BLOCK-1 and BLOCK-2")

    
    ; Mouse events
    ; Button down at (400, 399)
    (send PLAYGROUND-1 after-button-down 400 399)
    ; Button up at (400, 400)
    (send PLAYGROUND-1 after-button-up 400 399)

    ; Key event "b"
    ; Creates BLOCK-3
    (send PLAYGROUND-1 after-key-event "b")

     ; Mouse events
    ; Button down at (400, 420)
    (send PLAYGROUND-1 after-button-down 400 420)
    ; Button up at (400, 420)
    (send PLAYGROUND-1 after-button-up 400 420)

    ; Key event "b"
    ; Creates BLOCK-4
    (send PLAYGROUND-1 after-key-event "b")

    ; Checks the number of blocks in Playground-1
    (check-equal?
     (length (send PLAYGROUND-1 for-test:blocks))
     4
     "After 2 key event, the playground consists of 4 blocks, BLOCK-1, BLOCK-2
      BLOCK-3 and BLOCK-4")

    ; Mouse events
    ; Button down at (400, 399)
    (send PLAYGROUND-1 after-button-down 400 399)
    ; Button up at (400, 400)
    (send PLAYGROUND-1 after-drag 400 400)
    ; Button up at (400, 400)
    (send PLAYGROUND-1 after-button-up 400 400)

    ; Checks whether 2 blocks intersects, and forms teammates
    (check-equal?
     (length (send PLAYGROUND-1 for-test:teammates-of-block
                   (first (send PLAYGROUND-1 for-test:blocks))))
     1
     "After button down on BLOCK-3 , it intersects with BLOCK-4, which
      becomes it's teammate")

    ; Mouse events
    ; Button down at (400, 420)
    (send PLAYGROUND-1 after-button-down 400 420)
    ; Button up at (285, 330)
    (send PLAYGROUND-1 after-drag 285 330)
    ; Button up at (285, 300)
    (send PLAYGROUND-1 after-button-up 285 330)

    ; Checks the x-ccordinate of BLOCK-3 after dragging
    (check-equal?
     (send PLAYGROUND-1 for-test:block-x
           (second (send PLAYGROUND-1 for-test:blocks)))
     285
     "After dragging BLOCK-4, it's teammate BLOCK-3 is at (285, 310)")

    ; Checks the y-ccordinate of BLOCK-3 after tick event
    (check-equal?
     (send PLAYGROUND-1 for-test:block-y
           (second (send PLAYGROUND-1 for-test:blocks)))
     310
     "After dragging BLOCK-4, it's teammate BLOCK-3 is at (285, 310)")

    ; Checks whether 2 blocks intersects, and forms teammates
    (check-equal?
     (length (send PLAYGROUND-1 for-test:teammates-of-block
                   (first (send PLAYGROUND-1 for-test:blocks))))
     1
     "After dragging BLOCK-4 to (285, 330), BLOCK-3 it intersects with BLOCK-2, but
      since BLOCK-3 is not selected, it does not become teammate of BLOCK-2")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;