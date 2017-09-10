;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "probe.rkt")
(provide
 make-probe
 probe-x
 probe-y
 probe-direction
 probe?
 probe-at
 probe-turned-left
 probe-turned-right
 probe-forward
 probe-north?
 probe-south?
 probe-east?
 probe-west?
 )


;; DATA DEFINITION :
(define-struct probe(x y direction))

;; CONSTRUCTOR TEMPLATE :
;; A probe is (make-probe Integer Integer Direction)

;; INTERPRETATION :
;; (make-probe Integer Integer Direction) is a probe where :
;; x is the x-coordinate of the center of probe in cm
;; y is the y-coordinate of the center of probe in cm
;; direction is the Direction in which probe faces

;; DESTRUCTOR TEMPLATE :
;; probe-fn : Probe -> ??
#;(define (probe-fn pr)
    (...
     (probe-x pr)
     (probe-y pr)
     (probe-direction pr)))

;; Direction is one of :
;; --"north" : face of the probe is in north
;; --"south" : face of the probe is in south
;; --"east"  : face of the probe is in east
;; --"west"  : face of the probe is in west

;; DESTRUCTOR TEMPLATE :
;; direction-fn : Direction -> ??
#;(define (direction-fn dir)
    (...
     (string=? dir "north")
     (string=? dir "south")
     (string=? dir "east")
     (string=? dir "west")))



;; probe-at : Integer Integer -> Probe
;; GIVEN : An x-coordinate and a y-coordinate(both in cm)
;; WHERE : these coordinates leave the robot entirely inside the trap
;; RETURNS : A probe with its center at those coordinates, facing north
;; EXAMPLES :
;; (probe-at 0 0) => (make-probe 0 0 "north")
;; (probe-at -60 -80) => (make-probe -60 -80 "north")

;; STRATEGY : Use probe template
(define (probe-at x-cord y-cord)
  (make-probe x-cord y-cord "north"))

;; TESTS :
(begin-for-test
  (check-equal?
   (probe-at 0 0)
   (make-probe 0 0 "north")
   "Output should be (make-probe 0 0 'north')")
  (check-equal?
   (probe-at -60 -80)
   (make-probe -60 -80 "north")
   "Output should be (make-probe -60 -80 'north')"))




;; probe-turned-left : Probe -> Probe
;; probe-turned-right : Probe -> Probe
;; GIVEN : A probe
;; RETURNS : A probe like the original, but turned 90 degrees either left or right

;; EXAMPLES :
;; (probe-turned-left (make-probe 120 -90 "north")) => (make-probe 120 -90 "west")
;; (probe-turned-left (make-probe -10 80 "south")) => (make-probe -10 80 "east")
;; probe-turned-left (make-probe -30 -170 "east")) => (make-probe -30 -170 "north")
;; (probe-turned-left (make-probe 150 150 "west")) => (make-probe 150 150 "south")

;; STRATEGY : Use of probe and direction template
(define (probe-turned-left pr)
  (cond
    [(string=? "north" (probe-direction pr))
     (make-probe (probe-x pr) (probe-y pr) "west")]
    [(string=? "west" (probe-direction pr))
     (make-probe (probe-x pr) (probe-y pr) "south")]
    [(string=? "south" (probe-direction pr))
     (make-probe (probe-x pr) (probe-y pr) "east")]
    [(string=? "east" (probe-direction pr))
     (make-probe (probe-x pr) (probe-y pr) "north")]))

;; TESTS :
(begin-for-test
  (check-equal?
   (probe-turned-left (make-probe 120 -90 "north"))
   (make-probe 120 -90 "west")
   "Output should be (make-probe 120 -90 'west')")
  (check-equal?
   (probe-turned-left (make-probe -10 80 "south"))
   (make-probe -10 80 "east")
   "Output should be (make-probe -10 80 'east')")
  (check-equal?
   (probe-turned-left (make-probe -30 -170 "east"))
   (make-probe -30 -170 "north")
   "Output should be (make-probe -30 -170 'north')")
  (check-equal?
   (probe-turned-left (make-probe 150 150 "west"))
   (make-probe 150 150 "south")
   "Output should be (make-probe 150 150 'south')"))



;; EXAMPLES :
;; (probe-turned-right (make-probe 120 -90 "north")) => (make-probe 120 -90 "east")
;; (probe-turned-right (make-probe -10 80 "south")) => (make-probe -10 80 "west")
;; (probe-turned-right (make-probe -30 -170 "east")) => (make-probe -30 -170 "south")
;; (probe-turned-right (make-probe 150 150 "west")) => (make-probe 150 150 "north")

;; STRATEGY : Use of probe and direction template
(define (probe-turned-right pr)
  (cond
    [(string=? "north" (probe-direction pr))
     (make-probe (probe-x pr) (probe-y pr) "east")]
    [(string=? "east" (probe-direction pr))
     (make-probe (probe-x pr) (probe-y pr) "south")]
    [(string=? "south" (probe-direction pr))
     (make-probe (probe-x pr) (probe-y pr) "west")]
    [(string=? "west" (probe-direction pr))
     (make-probe (probe-x pr) (probe-y pr) "north")]))

;; TESTS :
(begin-for-test
  (check-equal?
   (probe-turned-right (make-probe 120 -90 "north"))
   (make-probe 120 -90 "east")
   "Output should be (make-probe 120 -90 'east')")
  (check-equal?
   (probe-turned-right (make-probe -10 80 "south"))
   (make-probe -10 80 "west")
   "Output should be (make-probe -10 80 'west')")
  (check-equal?
   (probe-turned-right (make-probe -30 -170 "east"))
   (make-probe -30 -170 "south")
   "Output should be (make-probe -30 -170 'south')")
  (check-equal?
   (probe-turned-right (make-probe 150 150 "west"))
   (make-probe 150 150 "north")
   "Output should be (make-probe 150 150 'north')"))



;; probe-north? : Probe -> Boolean
;; probe-south? : Probe -> Boolean
;; probe-east? : Probe -> Boolean
;; probe-west? : Probe -> Boolean
;; GIVEN: A probe
;; RETURNS : True iff the probe is facing in the specified direction

;; EXAMPLES :
;; (probe-north? (make-probe 120 -90 "north")) => #true
;; (probe-north? (make-probe -5 -150 "south")) => #false
;; (probe-north? (make-probe -30 40 "east")) => #false
;; (probe-north? (make-probe 10 60 "west")) => #false

;; STRATEGY : Use of probe and direction template
(define (probe-north? pr)
  (if (string=? "north" (probe-direction pr))
      #true
      #false))

;; TESTS :
(begin-for-test
  (check-equal?
   (probe-north? (make-probe 120 -90 "north"))
   #true
   "Output should be #true")
  (check-equal?
   (probe-north? (make-probe -5 -150 "south"))
   #false
   "Output should be #false")
  (check-equal?
   (probe-north? (make-probe -30 40 "east"))
   #false
   "Output should be #false")
  (check-equal?
   (probe-north? (make-probe 10 60 "west"))
   #false
   "Output should be #false"))



;; EXAMPLES :
;; (probe-south? (make-probe -5 -150 "south")) => #true
;; (probe-south? (make-probe 120 -90 "north")) => #false
;; (probe-south? (make-probe -30 40 "east")) => #false
;; (probe-south? (make-probe 10 60 "west")) => #false

;; STRATEGY : Use of probe and direction template
(define (probe-south? pr)
  (if (string=? "south" (probe-direction pr))
      #true
      #false))

;; TESTS :
(begin-for-test
  (check-equal?
   (probe-south? (make-probe -5 -150 "south"))
   #true
   "Output should be #true")
  (check-equal?
   (probe-south? (make-probe 120 -90 "north"))
   #false
   "Output should be #false")
  (check-equal?
   (probe-south? (make-probe -30 40 "east"))
   #false
   "Output should be #false")
  (check-equal?
   (probe-south? (make-probe 10 60 "west"))
   #false
   "Output should be #false"))




;; EXAMPLES :
;; (probe-east? (make-probe -30 40 "east")) => #true
;; (probe-east? (make-probe 120 -90 "north")) => #false
;; (probe-east? (make-probe -5 -150 "south")) => #false
;; (probe-east? (make-probe 10 60 "west")) => #false

;; STRATEGY : Use of probe and direction template
(define (probe-east? pr)
  (if (string=? "east" (probe-direction pr))
      #true
      #false))

;; TESTS :
(begin-for-test
  (check-equal?
   (probe-east? (make-probe -30 40 "east"))
   #true
   "Output should be #true")
  (check-equal?
   (probe-east? (make-probe 120 -90 "north"))
   #false
   "Output should be #false")
  (check-equal?
   (probe-east? (make-probe -5 -150 "south"))
   #false
   "Output should be #false")
  (check-equal?
   (probe-east? (make-probe 10 60 "west"))
   #false
   "Output should be #false"))



;; EXAMPLES :
;; (probe-west? (make-probe 10 60 "west")) => #true
;; (probe-west? (make-probe 120 -90 "north")) => #false
;; (probe-west? (make-probe -5 -150 "south")) => #false
;; (probe-west? (make-probe -30 40 "east")) => #false

;; STRATEGY : Use of probe and direction template
(define (probe-west? pr)
  (if (string=? "west" (probe-direction pr))
      #true
      #false))

;; TESTS :
(begin-for-test
  (check-equal?
   (probe-west? (make-probe 10 60 "west"))
   #true
   "Output should be #true")
  (check-equal?
   (probe-west? (make-probe 120 -90 "north"))
   #false
   "Output should be #false")
  (check-equal?
   (probe-west? (make-probe -5 -150 "south"))
   #false
   "Output should be #false")
  (check-equal?
   (probe-west? (make-probe -30 40 "east"))
   #false
   "Output should be #false"))



;; probe-forward : Probe PosInt -> Probe
;; GIVEN : A probe and a distance
;; RETURNS : A probe like the given one, but moved forward by the specified distance.
;;           If moving forward the specified distance would cause the probe to hit any
;;           wall of the trap, then the probe should move as far as it can inside the trap, and then stop
;; EXAMPLES :
;; (probe-forward (make-probe 60 50 "north") 30) => (make-probe 60 20 "north")
;; (probe-forward (make-probe 70 -60 "south") 60) => (make-probe 70 0 "south")
;; (probe-forward (make-probe -40 -130 "east") 250) => (make-probe 153 -130 "east")
;; (probe-forward (make-probe -60 -180 "west") 110) => (make-probe -153 -180 "west")

;; STRATEGY : Use probe template on pr
(define (probe-forward pr dist)
  (cond
    [(probe-north? pr) (if (<= (+ (- (probe-y pr)) dist) 153)
                        (make-probe (probe-x pr) (- (+ (- (probe-y pr)) dist)) "north")
                       (make-probe (probe-x pr) -153 "north"))]
    
    [(probe-south? pr) (if (<= (+ (probe-y pr) dist) 153)
                        (make-probe (probe-x pr) (+ (probe-y pr) dist) "south")
                       (make-probe (probe-x pr) 153 "south"))]
    
    [(probe-east? pr) (if (<= (+ (probe-x pr) dist) 153)
                        (make-probe (+ (probe-x pr) dist)(probe-y pr) "east")
                       (make-probe 153 (probe-y pr) "east"))]
    
    [(probe-west? pr) (if (<= (+ (- (probe-x pr)) dist) 153)
                        (make-probe (- (+ (- (probe-x pr)) dist))(probe-y pr) "west")
                       (make-probe -153 (probe-y pr) "west"))]))


;; TESTS :
(begin-for-test 
  (check-equal?
   (probe-forward (make-probe 60 50 "north") 30)
   (make-probe 60 20 "north")
   "Output should be (make-probe 60 20 'north')")
  (check-equal?
   (probe-forward (make-probe 40 -70 "north") 100)
   (make-probe 40 -153 "north")
   "Output should be (make-probe 40 -153 'north')")
  (check-equal?
   (probe-forward (make-probe 40 40 "south") 50)
   (make-probe 40 90 "south")
   "Output should be (make-probe 40 90 'south')")
  (check-equal?
   (probe-forward (make-probe -150 10 "south") 155)
   (make-probe -150 153 "south")
   "Output should be (make-probe -150 153 'south')")
  (check-equal?
   (probe-forward (make-probe 110 10 "east") 10)
   (make-probe 120 10 "east")
   "Output should be (make-probe 120 10 'east')")
  (check-equal?
   (probe-forward (make-probe -40 -130 "east") 250)
   (make-probe 153 -130 "east")
   "Output should be (make-probe 153 -130 'east')")
  (check-equal?
   (probe-forward (make-probe 0 0 "west") 154)
   (make-probe -153 0 "west")
   "Output should be (make-probe -153 0 'west')")
  (check-equal?
   (probe-forward (make-probe 20 -90 "west") 25)
   (make-probe -5 -90 "west")
   "Output should be (make-probe -5 -90 'west')"))

