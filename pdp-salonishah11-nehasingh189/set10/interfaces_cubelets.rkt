;; interfaces_cubelets.rkt
;; The file contains the interfaces and data definitions for cubelets.rkt
;; It is included in cubelets.rkt

;; It provides
;; INTERFACES :
;; Block<%>

;; DATA DEFINITIONS :
;; ListOfBlock<%>

#lang racket
(require "WidgetWorks.rkt")

(provide Block<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; ListOfBlock<%> :
;; A ListOfBlock<%> is one of :
;; -- empty
;; -- (cons Block<%> ListOfBlock<%>)

;; INTERPRETATION :
;; empty                          : a sequence of Block<%> with no elements
;; (cons Block<%> ListOfBlock<%>) : a sequence of Block<%> whose first element is a
;;                                  Block<%> and whose other elements are
;;                                  represented by ListOfBlock<%>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES :

;; Every object of class Block% implements the Block<%> interface.
;; The interface provides methods relating to the updating coordinates
;; of a block's teammates, adding a teammate, fetching teammates, adding
;; teammates of block and updating the blocks present in world.
(define Block<%>
  (interface (SWidget<%>)

    ;  -> ListOfBlock<%>
    ; RETURNS: the teammates of this block
    get-team

    ; Block<%> -> Void
    ; EFFECT: adds the given block to this block's team
    add-teammate

    ; -> Integer
    ; RETURNS : the x or y coordinates of this block
    block-x
    block-y

    ; Int Int -> Void
    ; GIVEN : The distance the selected block has moved i.e. the
    ;         difference of orginal position and position after dragging
    ;         the selected block
    ; EFFECT : Updated block with block moved by the given difference
    update-x-y-of-block

    ; ListOfBlock<%> -> Void
    ; GIVEN : Teammates of the block that called the function
    ; EFFECT : Updates the teammates of this block by appending
    ;          the blocks which are not present in teammates from given list 
    add-teammates-of-block

    ; ListOfBlock<%> -> Void
    ; GIVEN : List of bloks in the world
    ; EFFECT : Updated value for field blocks-in-world
    update-blocks-in-world
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;