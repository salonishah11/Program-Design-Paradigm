;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "coffee-machine.rkt")
(provide
 make-machinestate
 machinestate-coffee
 machinestate-chocolate
 machinestate-bank
 machinestate-cust-money
 machinestate?
 initial-machine
 machine-next-state
 machine-output
 machine-remaining-coffee
 machine-remaining-chocolate
 machine-bank
 )



;; DATA DEFINITION :
(define-struct machinestate(coffee chocolate bank cust-money))

;; CONSTRUCTOR TEMPLATE :
;; A machinestate is (make-machinestate NonNegInt NonNegInt NonNegInt NonNegInt)

;; INTERPRETATION :
;; (make-machinestate NonNegInt NonNegInt NonNegInt NonNegInt) is a machinestate where
;; coffee is number of cups of coffee available
;; chocolate is the number of cups of hot chocolate available
;; bank is the money stored in machine's bank after successful purchases, in cents
;; cust-money is the money inserted or unspent money left by the customer, in cents

;; DESTRUCTOR TEMPLATE :
;; machinestate-fn : MachineState -> ??
#;(define (machinestate-fn ms)
    (...
     (machinestate-coffee ms)
     (machinestate-chocolate ms)
     (machinestate-bank ms)
     (machinestate-cust-money ms)))



;; A CustomerInput is one of
;; -- a PosInt          interp : insert the specified amount of money, in cents
;; -- "coffee"          interp : request a coffee
;; -- "hot chocolate"   interp : request a hot chocolate
;; -- "change"          interp : return all the unspent money that the
;;                                       customer has inserted

;; DESTRUCTOR TEMPLATE :
;; customerinput-fn : CustomerInput -> ??
#;(define (customerinput-fn ci)
  (...
   (number? ci)
   (string=? ci "coffee")
   (string=? ci "hot chocolate")
   (string=? ci "change")))



;; A MachineOutput is one of
;; -- "coffee"         interp : machine dispenses a cup of coffee
;; -- "hot chocolate"  interp : machine dispenses a cup of hot chocolate
;; -- "Out of Item"    interp : machine displays "Out of Item"
;; -- a PosInt         interp : machine releases the specified amount of
;;                                      money, in cents
;; -- "Nothing"        interp : the machine does nothing

;; DESTRUCTOR TEMPLATE :
;; machineoutput-fn : MachineOutput -> ??
#;(define (machineoutput mo)
  (...
   (string=? mo "coffee")
   (string=? mo "hot chocolate")
   (string=? mo "Out of Item")
   (string=? mo "Nothing")
   (number? mo)))



;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN : A number of cups of coffee and of hot chocolate
;; RETURNS : The state of a machine loaded with the given number of cups
;;           of coffee and of hot chocolate, with an empty bank and cust-money
;; EXAMPLES :
;; (initial-machine 50 50) => (make-machinestate 50 50 0 0)

;; STRATEGY : Use of machinestate template
(define (initial-machine num-c num-hc)
  (make-machinestate num-c num-hc 0 0))

;; TESTS :
(begin-for-test
  (check-equal?
   (initial-machine 50 50)
   (make-machinestate 50 50 0 0)
   "Initial MachineState should be (make-machinestate 50 50 0 0)"))



;; cust-ip-money : MachineState NonNegInt -> MachineState
;; GIVEN : A machine state and customer input
;; RETURNS : The machine state with amount of money inserted in cust-money
;; EXAMPLES :
;; (cust-ip-money (make-machinestate 50 50 0 0) 150) => (make-machinestate 50 50 0 150)

;; STRATEGY : Use machinestate template on ms
(define (cust-ip-money ms cust-ip)
  (make-machinestate (machinestate-coffee ms)
                     (machinestate-chocolate ms)
                     (machinestate-bank ms)
                     (+ (machinestate-cust-money ms) cust-ip)))

;; TESTS :
(begin-for-test
  (check-equal?
   (cust-ip-money (make-machinestate 50 50 0 0) 150)
   (make-machinestate 50 50 0 150)
   "MachineState should be (make-machinestate 50 50 0 150)"))



;; cust-ip-coffee : MachineState -> MachineState
;; GIVEN : A machine state
;; RETURNS : A machine state based on the amount of money present in cust-money
;;           for coffee
;; EXAMPLES :
;; (cust-ip-coffee (make-machinestate 50 50 0 300)) => (make-machinestate 49 50 150 150)
;; (cust-ip-coffee (make-machinestate 50 50 150 140)) => (make-machinestate 50 50 150 140)

;; STRATEGY : Use machinestate template on ms
(define (cust-ip-coffee ms)
  (if (and (>= (machinestate-cust-money ms) 150) (> (machinestate-coffee ms) 0))
      (make-machinestate (- (machinestate-coffee ms) 1)
                         (machinestate-chocolate ms)
                         (+ (machinestate-bank ms) 150)
                         (- (machinestate-cust-money ms) 150))
      (make-machinestate (machinestate-coffee ms)
                         (machinestate-chocolate ms)
                         (machinestate-bank ms)
                         (machinestate-cust-money ms))))

;; TESTS :
(begin-for-test
  (check-equal?
   (cust-ip-coffee (make-machinestate 50 50 0 300))
   (make-machinestate 49 50 150 150)
   "MachineState should be (make-machinestate (49 50 150 150))")
  (check-equal?
   (cust-ip-coffee (make-machinestate 50 50 150 140))
   (make-machinestate 50 50 150 140)
   "MachineState should be (make-machinestate 50 50 150 140)"))
           


;; cust-ip-chocolate : MachineState -> MachineState
;; GIVEN : A machine state
;; RETURNS :  A machine state based on the amount of money present in cust-money
;;           for hot chocolate
;; EXAMPLES :
;; (cust-ip-chocolate (make-machinestate 50 50 60 60)) => (make-machinestate 50 49 120 0)
;; (cust-ip-chocolate (make-machinestate 50 50 60 50)) => (make-machinestate 50 50 60 50)

;; STRATEGY : Use of machinestate template on ms
(define (cust-ip-chocolate ms)
  (if (and (>= (machinestate-cust-money ms) 60) (> (machinestate-chocolate ms) 0))
      (make-machinestate (machinestate-coffee ms)
                         (- (machinestate-chocolate ms) 1)
                         (+ (machinestate-bank ms) 60)
                         (- (machinestate-cust-money ms) 60))
      (make-machinestate (machinestate-coffee ms)
                         (machinestate-chocolate ms)
                         (machinestate-bank ms)
                         (machinestate-cust-money ms))))

;; TESTS :
(begin-for-test
  (check-equal?
   (cust-ip-chocolate (make-machinestate 50 50 60 60))
   (make-machinestate 50 49 120 0)
   "MachineState should be (make-machinestate 50 49 120 0)")
  (check-equal?
   (cust-ip-chocolate (make-machinestate 50 50 60 50))
   (make-machinestate 50 50 60 50)
   "MachineState should be (make-machinestate 50 50 60 50)"))



;; cust-ip-change : MachineState -> MachineState
;; GIVEN : A machine state
;; RETURNS :  A machine state based on the amount of money remaining in cust-money
;; EXAMPLES :
;; (cust-ip-change (make-machinestate 50 50 150 20)) => (make-machinestate 50 50 150 0)
;; (cust-ip-change (make-machinestate 50 50 150 0)) => (make-machinestate 50 50 150 0)

;; STRATEGY : Use of machinestate template on ms
(define (cust-ip-change ms)
  (if (> (machinestate-cust-money ms) 0)
      (make-machinestate (machinestate-coffee ms)
                         (machinestate-chocolate ms)
                         (machinestate-bank ms)
                         0)
      (make-machinestate (machinestate-coffee ms)
                         (machinestate-chocolate ms)
                         (machinestate-bank ms)
                         (machinestate-cust-money ms))))

;; TESTS :
(begin-for-test
  (check-equal?
   (cust-ip-change (make-machinestate 50 50 150 20))
   (make-machinestate 50 50 150 0)
   "MachineState should be (make-machinestate 50 50 150 20)")
  (check-equal?
   (cust-ip-change (make-machinestate 50 50 150 0))
   (make-machinestate 50 50 150 0)
   "MachineState should be (make-machinestate 50 50 150 0)"))



;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN : A machine state and a customer input
;; RETURNS : The state of the machine that should follow the customer's input
;; EXAMPLES :
;; (machine-next-state (make-machinestate 50 50 0 0) 200) => (make-machinestate 50 50 0 200)
;; (machine-next-state (make-machinestate 50 50 0 300) "coffee") => (make-machinestate 49 50 150 150)
;; (machine-next-state (make-machinestate 50 50 150 150) "change") => (make-machinestate 50 50 150 0)
;; (machine-next-state (make-machinestate 50 0 120 70) "hot chocolate") => (make-machinestate 50 0 120 70)

;; STRATEGY : Divide into cases based on <condition>
(define (machine-next-state ms cust-ip)
  (cond
    [(number? cust-ip)
     (cust-ip-money ms cust-ip)]
    [(string=? "coffee" cust-ip)
     (cust-ip-coffee ms)]
    [(string=? "hot chocolate" cust-ip)
     (cust-ip-chocolate ms)]
    [(string=? "change" cust-ip)
     (cust-ip-change ms)]))

;; TESTS :
(begin-for-test
  (check-equal?
   (machine-next-state (make-machinestate 50 50 0 0) 200)
   (make-machinestate 50 50 0 200)
   "MachineState should be (make-machinestate 50 50 0 200)")
  (check-equal?
   (machine-next-state (make-machinestate 50 50 0 300) "coffee")
   (make-machinestate 49 50 150 150)
   "MachineState should be (make-machinestate 49 50 150 150)")
  (check-equal?
   (machine-next-state (make-machinestate 50 50 150 150) "change")
   (make-machinestate 50 50 150 0)
   "MachineState should be (make-machinestate 50 50 150 0)")
  (check-equal?
   (machine-next-state (make-machinestate 50 50 150 0) "change")
   (make-machinestate 50 50 150 0)
   "MachineState should be (make-machinestate 50 50 150 0)")
  (check-equal?
   (machine-next-state (make-machinestate 50 50 150 100) "hot chocolate")
   (make-machinestate 50 49 210 40)
   "MachineState should be (make-machinestate 50 49 210 40)")
  (check-equal?
   (machine-next-state (make-machinestate 50 0 120 70) "hot chocolate")
   (make-machinestate 50 0 120 70)
   "MachineState should be (make-machinestate 50 0 120 70)")
  (check-equal?
   (machine-next-state (make-machinestate 50 50 150 140) "coffee")
   (make-machinestate 50 50 150 140)
   "MachineState should be (make-machinestate 50 50 150 140)")
  (check-equal?
   (machine-next-state (make-machinestate 50 50 150 20) "hot chocolate")
   (make-machinestate 50 50 150 20)
   "MachineState should be (make-machinestate 50 50 150 20)"))



;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: A machine state and a customer input
;; RETURNS : A MachineOutput that describes the machine's response to the
;;           customer input
;; EXAMPLES :
;; (machine-output (make-machinestate 50 50 0 300) "coffee") => "coffee"
;; (machine-output (make-machinestate 50 0 120 70) "hot chocolate") => "Out of Item"
;; (machine-output (make-machinestate 50 50 60 10) "coffee") => "Nothing"
;; (machine-output (make-machinestate 50 50 60 60) "hot chocolate") => "hot chocolate"
;; (machine-output (make-machinestate 50 50 150 20) "change") => 20

;; STRATEGY : Use machinestate template on ms
(define (machine-output ms cust-ip)
  (cond
    [(number? cust-ip)
     "Nothing"]
    [(string=? "coffee" cust-ip) (cond
                                   [(= (machinestate-coffee ms) 0) "Out of Item"]
                                   [(>= (machinestate-cust-money ms) 150) "coffee"]
                                   [else "Nothing"])]
    [(string=? "hot chocolate" cust-ip) (cond
                                          [(= (machinestate-chocolate ms) 0) "Out of Item"]
                                          [(>= (machinestate-cust-money ms) 60) "hot chocolate"]
                                          [else "Nothing"])]
    [(string=? "change" cust-ip) (if (> (machinestate-cust-money ms) 0)
                                     (machinestate-cust-money ms) "Nothing")]))

;; TESTS :
(begin-for-test
  (check-equal?
   (machine-output (make-machinestate 50 50 0 0) 200)
   "Nothing"
   "MachineOutput should be Nothing")
  (check-equal?
   (machine-output (make-machinestate 50 50 0 300) "coffee")
   "coffee"
   "MachineOutput should be coffee")
  (check-equal?
   (machine-output (make-machinestate 50 50 60 60) "hot chocolate")
   "hot chocolate"
   "MachineOutput should be hot chocolate")
  (check-equal?
   (machine-output (make-machinestate 50 50 60 10) "coffee")
   "Nothing"
   "MachineOutput should be Nothing")
  (check-equal?
   (machine-output (make-machinestate 50 50 150 20) "change")
   20
   "MachineOutput should be 20")
  (check-equal?
   (machine-output (make-machinestate 50 50 150 0) "change")
   "Nothing"
   "MachineOutput should be Nothing")
  (check-equal?
   (machine-output (make-machinestate 50 0 120 70) "hot chocolate")
   "Out of Item"
   "MachineOutput should be Out of Item")
  (check-equal?
   (machine-output (make-machinestate 0 50 120 70) "coffee")
   "Out of Item"
   "MachineOutput should be Out of Item")
  (check-equal?
   (machine-output (make-machinestate 50 50 60 40) "hot chocolate")
   "Nothing"
   "MachineOutput should be Nothing"))



;; machine-remaining-coffee : MachineState -> NonNegInt
;; GIVEN : A machine state
;; RETURNS : The number of cups of coffee left in the machine
;; EXAMPLES :
;; (machine-remaining-coffee (make-machinestate 30 20 150 0)) => 30
;; (machine-remaining-coffee (make-machinestate 0 20 150 0)) => 0

;; STRATEGY : Use machinestate template on ms
(define (machine-remaining-coffee ms)
  (machinestate-coffee ms))

;; TESTS :
(begin-for-test
  (check-equal?
   (machine-remaining-coffee (make-machinestate 30 20 150 0))
   30
   "Remaining cups of coffee should be 30")
  (check-equal?
   (machine-remaining-coffee (make-machinestate 0 20 150 0))
   0
   "Remaining cups of hot chocolate should be 0"))




;; machine-remaining-chocolate : MachineState -> NonNegInt
;; GIVEN : A machine state
;; RETURNS : The number of cups of hot chocolate left in the machine
;; EXAMPLES :
;; (machine-remaining-chocolate (make-machinestate 30 20 60 0)) => 20
;; (machine-remaining-chocolate (make-machinestate 30 0 120 0)) => 0

;; STRATEGY : Use machinestate template on ms
(define (machine-remaining-chocolate ms)
  (machinestate-chocolate ms))

;; TESTS :
(begin-for-test
  (check-equal?
   (machine-remaining-chocolate (make-machinestate 30 20 60 0))
   20
   "Remaining cups of hot chocolate should be 20")
  (check-equal?
   (machine-remaining-chocolate (make-machinestate 30 0 120 0))
   0
   "Remaining cups of hot chocolate should be 0"))



;; machine-bank : MachineState -> NonNegInt
;; GIVEN : A machine state
;; RETURNS : The amount of money in the machine's bank, in cents
;; EXAMPLES :
;; (machine-bank (make-machinestate 30 20 150 0)) => 150
;; (machine-bank (make-machinestate 30 20 0 0)) => 0

;; STRATEGY : Use machinestate template on ms
(define (machine-bank ms)
  (machinestate-bank ms))

;; TESTS :
(begin-for-test
  (check-equal?
   (machine-bank (make-machinestate 30 20 150 0))
   150
   "Amount in bank should be 150")
  (check-equal?
   (machine-bank (make-machinestate 30 20 0 0))
   0
   "Amount in bank should be 0"))