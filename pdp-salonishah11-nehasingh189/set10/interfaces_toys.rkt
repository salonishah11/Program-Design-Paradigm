;; interfaces_toys.rkt
;; The file contains the constants, interfaces and data definitions for toys.rkt
;; It is included in toys.rkt

;; It provides
;; INTERFACES :
;; Toy<%>
;; PlaygroundState<%>
;; Target<%>

;; DATA DEFINITIONS :
;; ListOfSWidget<%>
;; ListOfToy<%>

#lang racket
(require "WidgetWorks.rkt")

(provide
 CANVAS-WIDTH
 CANVAS-HEIGHT
 HALF-CANVAS-WIDTH
 HALF-CANVAS-HEIGHT
 Toy<%>
 PlaygroundState<%>
 Target<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; CANVAS ATTRIBUTES:
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; ListOfSWidget<%> (LOSW) :
;; ListOfSWidget<%> contains the list of stateful widgets present in the
;; PlaygroundState

;; A ListOfSWidget<%> (LOSW) is either
;; -- empty
;; (cons SWidget<%> LOSW)
;; TEMPLATE:
;; losw-fn : LOSW -> ??
#;(define (losw-fn losw)
  (cond
    [(empty? losw) ...]
    [else ... (first losw)
          losw-fn(rest losw)]))


;; ListOfToy<%> :
;; A ListOfToy<%> is a ListOfSWidget<%> containing only Toy<%>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES :

;; The PlaygroundState implements the SWidget<%> interface provided by the
;; widgetworks framework.
;; It has methods that deal with extracting information of target.
(define PlaygroundState<%>
  (interface (SWidget<%>) ;; include all the methods of SWidget<%>. 
    
    ;; -> Integer
    ;; RETURNS : the x and y coordinates of the target    
    target-x
    target-y
    
    ;; -> Boolean
    ;; RETURNS : a Boolean representing whether the target is selected
    ;;           or not
    target-selected?
    
    ;; -> ListOfToy<%>
    ;; RETURNS : The list of all the toys present in the playground
    get-toys))


;; The Toy interface implements the SWidget<%> interface provided by the
;; widgetworks.
;; It has methods that deal with extracting information of each toy.
;; Every object that lives in the World must implement the Toy<%> interface.
(define Toy<%>
  (interface (SWidget<%>)  ; this means : include all the methods of
                           ; SWidget<%>.
    
    ;; -> Int
    ;; RETURNS : The x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS : Some data related to the toy.  The interpretation of
    ;;           this data depends on the class of the toy. For
    ;;           -- Square   : it is the velocity of the square (rightward is
    ;;                         positive)
    ;;           -- Throbber : it is the current radius of the throbber
    ;;           -- Clock    : it is the current value of the clock
    ;;           -- Football : it is the current size of the football (in
    ;;                         arbitrary units; bigger is more)
    toy-data
    ))

;; Every object of Target% implements Target<%>
;; It provides methods that deal with extracting information related to Target.
(define Target<%>
  (interface (SWidget<%>)  ; this means : include all the methods of
                           ; SWidget<%>.

    ;; -> Int
    ;; RETURNS : The x or y position of the center of the target
    get-x
    get-y

    ;; -> Boolean
    ;; RETURNS : a Boolean representing whether the target is selected
    ;;           or not
    get-selected?
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;