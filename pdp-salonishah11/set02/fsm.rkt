;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "fsm.rkt")
(provide
 make-state
 state-number
 state-name
 state?
 initial-state
 next-state
 accepting-state?
 error-state?
 )

;; DATA DEFINITION :
(define-struct state(number name))

;; CONSTRUCTOR TEMPLATE :
;; A state is (make-state (NonNegInt StateName))

;; INTERPRETATION :
;; (make-state (NonNegInt name) is a state where
;; number is an NonNegInt assigned to each state
;; name is the StateName of each state

;; DESTRUCTOR TEMPLATE :
;; state-fn : state -> ??
#;(define (state-fn st)
    (...
     (State-number st)
     (State-name st)))

;; A StateName is one of : 
;; -- "ABC" : Initial State, expects inputs "a", "b" or "c" next
;; -- "ABD" : Expects inputs "a", "b" or "d" next
;; -- "EF"  : Expects inputs "e" or "f" next, Accepting State
;; -- "ER"  : Error State, invalid input

;; DESTRUCTOR TEMPLATE :
;; name-fn : StateName -> ??
#;(define (name-fn nm)
    (...
     (string=? nm "ABC")
     (string=? nm "ABD")
     (string=? nm "EF")
     (string=? nm "ER")))



;; A MachineInput is one of :
;; -- "a"
;; -- "b"
;; -- "c"
;; -- "d"
;; -- "e"
;; -- "f"

;; DESTRUCTOR TEMPLATE :
;; machineinput-fn : MachineInput -> ??
#;(define (machineinput-fn mi)
    (...
     (string=? mi "a")
     (string=? mi "b")
     (string=? mi "c")
     (string=? mi "d")
     (string=? mi "e")
     (string=? mi "f")))



;; initial-state : Number -> State
;; GIVEN : A number
;; RETURNS : A representation of the initial state of your machine.
;;          The given number is ignored
;; EXAMPLES :
;; (initial-state 1) => (make-state 1 "ABC")

;; STRATEGY : Use of state template
(define (initial-state i)
  (make-state 1 "ABC"))

;; TESTS :
(begin-for-test
  (check-equal?
   (initial-state 1)
   (make-state 1 "ABC")
   "Initial State should be (make-state 1 'ABC')"))



;; state-after-ABC : String -> State
;; GIVEN : A MachineInput
;; RETURNS : The next state after "ABC" based on the MachineInput
;; EXAMPLES :
;; (state-after-ABC "a") => (make-state 1 "ABC")
;; (state-after-ABC "b") => (make-state 1 "ABC")
;; (state-after-ABC "c") => (make-state 2 "ABD")
;; (state-after-ABC "d") => (make-state 4 "ER")
;; (state-after-ABC "e") => (make-state 4 "ER")
;; (state-after-ABC "f") => (make-state 4 "ER")

;; STRATEGY : Divide into cases on <condition> 
(define (state-after-ABC input)
  (cond
    [(or (string=? "a" input)
         (string=? "b" input))
     (make-state 1 "ABC")]   
    [(string=? "c" input)
     (make-state 2 "ABD")]   
    [else (make-state 4 "ER")]))

;; TESTS :
(begin-for-test
  (check-equal?
   (state-after-ABC "a")
   (make-state 1 "ABC")
   "Output should be (make-state 1 'ABC')")
  (check-equal?
   (state-after-ABC "b")
   (make-state 1 "ABC")
   "Output should be (make-state 1 'ABC')")
  (check-equal?
   (state-after-ABC "c")
   (make-state 2 "ABD")
   "Output should be (make-state 2 'ABD')")
  (check-equal?
   (state-after-ABC "d")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')")
  (check-equal?
   (state-after-ABC "e")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')")
  (check-equal?
   (state-after-ABC "f")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')"))



;; state-after-ABD : MachineInput -> State
;; GIVEN : A MachineInput
;; RETURNS : The next state after "ABD" based on the given MachineInput
;; EXAMPLES :
;; (state-after-ABD "a") => (make-state 2 "ABD")
;; (state-after-ABD "b") => (make-state 2 "ABD")
;; (state-after-ABD "d") => (make-state 3 "EF")
;; (state-after-ABD "c") => (make-state 4 "ER")
;; (state-after-ABD "e") => (make-state 4 "ER")
;; (state-after-ABD "f") => (make-state 4 "ER")

;; STRATEGY : Divide into cases based on <condition>
(define (state-after-ABD input)
  (cond
    [(or (string=? "a" input)
         (string=? "b" input))
     (make-state 2 "ABD")]   
    [(string=? "d" input)
     (make-state 3 "EF")]  
    [else (make-state 4 "ER")]))

;; TESTS :
(begin-for-test
  (check-equal?
   (state-after-ABD "a")
   (make-state 2 "ABD")
   "Output should be (make-state 2 'ABD')")
  (check-equal?
   (state-after-ABD "b")
   (make-state 2 "ABD")
   "Output should be (make-state 2 'ABD')")
  (check-equal?
   (state-after-ABD "d")
   (make-state 3 "EF")
   "Output should be (make-state 3 'EF')")
  (check-equal?
   (state-after-ABD "c")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')")
  (check-equal?
   (state-after-ABD "e")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')")
  (check-equal?
   (state-after-ABD "f")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')"))



;; state-after-EF : MachineInput -> State
;; GIVEN : A MachineInput
;; RETURNS : The next state after "EF" based on the given MachineInput
;; EXAMPLES :
;; (state-after-EF "e") => (make-state 3 "EF")
;; (state-after-EF "f") => (make-state 3 "EF")
;; (state-after-EF "a") => (make-state 4 "ER")
;; (state-after-EF "b") => (make-state 4 "ER")
;; (state-after-EF "c") => (make-state 4 "ER")
;; (state-after-EF "d") => (make-state 4 "ER")

;; STRATEGY : Divide into cases based on <condition>
(define (state-after-EF input)
  (cond
    [(or (string=? "e" input)
         (string=? "f" input))
     (make-state 3 "EF")]   
    [else (make-state 4 "ER")]))

;; TESTS :
(begin-for-test
  (check-equal?
   (state-after-EF "e")
   (make-state 3 "EF")
   "Output should be (make-state 3 'EF')")
  (check-equal?
   (state-after-EF "f")
   (make-state 3 "EF")
   "Output should be (make-state 3 'EF')")
  (check-equal?
   (state-after-EF "a")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')")
  (check-equal?
   (state-after-EF "b")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')")
  (check-equal?
   (state-after-EF "c")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')")
  (check-equal?
   (state-after-EF "d")
   (make-state 4 "ER")
   "Output should be (make-state 4 'ER')"))



;; next-state : State MachineInput -> State
;; GIVEN : A state of the machine and a machine input
;; RETURNS : The state that should follow the given input
;; EXAMPLES :
;; (next-state (make-state 1 "ABC") "a") => (make-state 1 "ABC")
;; (next-state (make-state 1 "ABC") "c") => (make-state 2 "ABD")
;; (next-state (make-state 2 "ABD") "d") => (make-state 3 "EF")
;; (next-state (make-state 3 "EF") "c") => (make-state 4 "ER")

;; STRATEGY : Use state template on st
(define (next-state st input)
  (cond
    [(string=? (state-name st) "ABC") (state-after-ABC input)]   
    [(string=? (state-name st) "ABD") (state-after-ABD input)]  
    [(string=? (state-name st) "EF") (state-after-EF input)]
    [else (make-state 4 "ER")]))

;; TESTS :
(begin-for-test
  (check-equal?
   (next-state (make-state 1 "ABC") "a")
   (make-state 1 "ABC")
   "Next State should be (make-state 1 'ABC')")
  (check-equal?
   (next-state (make-state 2 "ABD") "d")
   (make-state 3 "EF")
   "Next State should be (make-state 3 'EF')")
  (check-equal?
   (next-state (make-state 3 "EF") "e")
   (make-state 3 "EF")
   "Next State should be (make-state 3 'EF')")
  (check-equal?
   (next-state (make-state 4 "ER") "c")
   (make-state 4 "ER")
   "Next State should be (make-state 4 'ER')"))
 


;; accepting-state? : State -> Boolean
;; GIVEN : A state of the machine
;; RETURNS : True iff the given state is a final (accepting) state
;; EXAMPLES :
;; (accepting-state? (make-state 3 "EF")) => #true
;; (accepting-state? (make-state 1 "ABC")) => #false
;; (accepting-state? (make-state 2 "ABD")) => #false
;; (accepting-state? (make-state 4 "ER")) => #false

;; STRATEGY : Use state template on st
(define (accepting-state? st)
  (if (string=? (state-name st) "EF")
      #true
      #false))

;; TESTS :
(begin-for-test
  (check-equal?
   (accepting-state? (make-state 3 "EF"))
   #true
   "Output should be #true")
  (check-equal?
   (accepting-state? (make-state 1 "ABC"))
   #false
   "Output should be #false")
  (check-equal?
   (accepting-state? (make-state 2 "ABD"))
   #false
   "Output should be #false")
  (check-equal?
   (accepting-state? (make-state 4 "ER"))
   #false
   "Output should be #false"))



;; error-state? : State -> Boolean
;; GIVEN : A state of the machine
;; RETURNS : True iff there is no path (empty or non-empty) from the given
;;           state to an accepting state
;; EXAMPLES :
;; (error-state? (make-state 1 "ABC")) => #true
;; (error-state? (make-state 4 "ER")) => #true
;; (error-state? (make-state 2 "ABD")) => #false
;; (error-state? (make-state 3 "EF")) => #false

;; STRATEGY : Use state template on st
(define (error-state? st)
  (cond
    [(string=? (state-name st) "ABC")
     #true]
    [(string=? (state-name st) "ABD")
     #false]
    [(string=? (state-name st) "EF")
     #false]
    [(string=? (state-name st) "ER")
     #true]))

;; TESTS :
(begin-for-test
  (check-equal?
   (error-state? (make-state 1 "ABC"))
   #true
   "Output should be #true")
  (check-equal?
   (error-state? (make-state 4 "ER"))
   #true
   "Output should be #true")
  (check-equal?
   (error-state? (make-state 2 "ABD"))
   #false
   "Output should be #false")
  (check-equal?
   (error-state? (make-state 3 "EF"))
   #false
   "Output should be #false"))
