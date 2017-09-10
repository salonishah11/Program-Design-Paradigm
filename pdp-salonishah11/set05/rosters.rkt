;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; rosters.rkt
;; A list of (student, class) pairs is given.
;; The class roster for each class that has at least one student
;; enrolled is to be given as output. 

(require rackunit)
(require "extras.rkt")
(check-location "05" "rosters.rkt")

(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 roster=?
 rosterset=?
 enrollments-to-rosters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS :

(define TRUE #true)
(define FALSE #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; Student :
;; A Student is of type Any


;; Class :
;; A Class is of type Any



;; Structure Enrollment : 
(define-struct enrollment (student class))

;; CONSTRUCTOR TEMPLATE :
;; An Enrollment is a (make-enrollment Student Class)

;; INTERPRETATION :
;; (make-enrollment Student Class) represents the assertion
;; that Student s is enrolled in Class c

;; DESTRUCTOR TEMPLATE :
;; enrollment-fn : Enrollment -> ??
#;(define (enrollment-fn en)
    (...
     (enrollment-student en)
     (enrollment-class en) ...))



;; Structure Roster :
(define-struct roster (classname students))

;; CONSTRUCTOR TEMPLATE :
;; A ClassRoster is a (make-roster Class SetOfStudents)

;; INTERPRETATION :
;; (make-roster Class SetOfStudents) represents that the students in
;; Class c are exactly the Students in set ss

;; DESTRUCTOR TEMPLATE :
;; roster-fn : Roster -> ??
#;(define (roster-fn r)
    (...
     (roster-classname r)
     (roster-students r) ...))



;; A SetOfX is a list of X's without duplication.
;; Two SetOfX's are considered equal if they have the same members

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples

(define STUDENT-LIST1-PDP (list "John" "Feng" "Amy"))
(define STUDENT-LIST1-PDP-POSITION-EXCHANGED (list "Amy" "Feng" "John"))

(define ROSTER-PDP1 (make-roster "PDP" STUDENT-LIST1-PDP))
(define ROSTER-PDP1-WITH-STUDENT-POSITION-EXCHANGED
  (make-roster "PDP" STUDENT-LIST1-PDP-POSITION-EXCHANGED))

(define STUDENT-LIST-DB1 (list "Saloni" empty))
(define ROSTER-DB1 (make-roster "DB" STUDENT-LIST-DB1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; roster=? : ClassRoster ClassRoster -> Boolean
;; GIVEN : 2 ClassRosters
;; RETURNS: True iff the two arguments represent the same roster
;; EXAMPLES :
;; (roster=? ROSTER-PDP1 ROSTER-PDP1-WITH-STUDENT-POSITION-EXCHANGED) =>
;;   TRUE
;; (roster=? ROSTER-PDP1 ROSTER-DB1) => FALSE

;; DESIGN STRATEGY : Combine simpler functions
(define (roster=? cr1 cr2)
  (and (equal? (roster-classname cr1)
               (roster-classname cr2))
       (set-of-students-equal? (roster-students cr1)
                               (roster-students cr2))))

;; TESTS :
(begin-for-test
  (check-equal?
   (roster=? ROSTER-PDP1
             ROSTER-PDP1-WITH-STUDENT-POSITION-EXCHANGED)
   TRUE
   "Since two rosters are same, #true is returned")
  (check-equal?
   (roster=? ROSTER-PDP1
             ROSTER-DB1)
   FALSE
   "Since two rosters are not same, #false is returned"))



;; Helper function for roster=?
;; set-of-students-equal? : SetOfStudents SetOfStudents -> Boolean
;; GIVEN : 2 SetOfStudents
;; RETURNS : True iff the 2 SetOfStudents have the same members
;; EXAMPLES :
;; (set-of-students-equal? STUDENT-LIST1-PDP STUDENT-LIST1-PDP-POSITION-EXCHANGED)
;;   => TRUE
;; (set-of-students-equal? STUDENT-LIST-DB1 STUDENT-LIST1-PDP) => FALSE

;; DESIGN STRATEGY : Combine simpler functions
(define (set-of-students-equal? student-set1 student-set2)
  (and (student-subset? student-set1 student-set2)
       (student-subset? student-set2 student-set1)))

;; TESTS :
(begin-for-test
  (check-equal?
   (set-of-students-equal? STUDENT-LIST1-PDP
                           STUDENT-LIST1-PDP-POSITION-EXCHANGED)
   TRUE
   "Since both sets represent the same set, #true is returned")
  (check-equal?
   (set-of-students-equal? STUDENT-LIST-DB1
                           STUDENT-LIST1-PDP)
   FALSE
   "Since both sets do not represent the same set, #false is returned"))



;; Helper function for set-of-students-equal?
;; student-subset? : SetOfStudents SetOfStudents -> Boolean
;; GIVEN : 2 SetOfStudents
;; RETURNS : True if the first set is the subset of second set
;; EXAMPLES :
;; (student-subset? STUDENT-LIST1-PDP STUDENT-LIST1-PDP-POSITION-EXCHANGED)
;;   => TRUE
;; (student-subset? STUDENT-LIST-DB1 STUDENT-LIST1-PDP) => FALSE

;; DESIGN STRATEGY : Use HOF andmap on student-set1
(define (student-subset? student-set1 student-set2)
  (andmap
   ;; Student -> Boolean
   ;; GIVEN : A Student from first set
   ;; RETURNS : True if the given Student is a
   ;;           member of second set
   (lambda (st) (member-of-set? st student-set2))
   student-set1))

;; TESTS :
(begin-for-test
  (check-equal?
   (student-subset? STUDENT-LIST1-PDP
                    STUDENT-LIST1-PDP-POSITION-EXCHANGED)
   TRUE
   "Since first set is subset of second set, #true is returned")
  (check-equal?
   (student-subset? STUDENT-LIST-DB1
                    STUDENT-LIST1-PDP)
   FALSE
   "Since first set is not a subset of second set, #false is returned"))



;; Helper function for is-subset?
;; member-of-set? : Student SetOfStudents -> Boolean
;; GIVEN : A Student and SetOfStudents
;; RETURNS : True if the given Student is member of the given
;;           SetOfStudents
;; EXAMPLES :
;; (member-of-set? "John" STUDENT-LIST1-PDP) => TRUE
;; (member-of-set? "Saloni" STUDENT-LIST1-PDP) => FALSE

;; DESIGN STRATEGY : Use HOF ormap on student-set
(define (member-of-set? x student-set)
  (ormap
   ;; Student -> Boolean
   ;; GIVEN : A Student
   ;; RETURNS : True if x and given Student are equal
   (lambda (st) (equal? x st))
   student-set))

;; TESTS :
(begin-for-test
  (check-equal?
   (member-of-set? "John" STUDENT-LIST1-PDP)
   TRUE
   "Since Student is member of given set, #true is returned")
  (check-equal?
   (member-of-set? "Saloni" STUDENT-LIST1-PDP)
   FALSE
   "Since Student is not a member of given set, #false is returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples

(define ROSTER-SET1
  (list
   (make-roster "PDP" (list "John" "Feng" "Amy"))
   (make-roster "Networks" (list "Kathryn" "Amy"))))

(define ROSTER-SET1-WITH-POSITION-EXCHANGED
  (list
   (make-roster "Networks" (list "Kathryn" "Amy"))
   (make-roster "PDP" (list "John" "Amy" "Feng"))))

(define ROSTER-SET2
  (list
  (make-roster "DB" (list "1" "2" "3"))
  (make-roster "DB" (list "6" "8" "5"))
  (make-roster "DB" (list "4" "7" "9"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rosterset=? : SetOfClassRoster SetOfClassRoster -> Boolean
;; GIVEN : 2 SetOfClassRoster
;; RETURNS : True iff the two arguments represent the same set of rosters
;; EXAMPLES :
;; (rosterset=? ROSTER-SET1 ROSTER-SET1-WITH-POSITION-EXCHANGED) => TRUE
;; (rosterset=? ROSTER-SET2 ROSTER-SET1) => FALSE

;; DESIGN STRATEGY : Combine simpler functions
(define (rosterset=? roster-set1 roster-set2)
  (and (roster-subset? roster-set1 roster-set2)
       (roster-subset? roster-set2 roster-set1)))

;; TESTS :
(begin-for-test
  (check-equal?
   (rosterset=? ROSTER-SET1
                ROSTER-SET1-WITH-POSITION-EXCHANGED)
   TRUE
   "Since given roster sets represent same set, #true is returned")
  (check-equal?
   (rosterset=? ROSTER-SET2
                ROSTER-SET1)
   FALSE
   "Since given roster sets do not represent same set, #false is returned"))



;; Helper function for rosterset=?
;; roster-subset? : SetOfClassRoster SetOfClassRoster -> Boolean
;; GIVEN : 2 SetOfClassRoster
;; RETURNS : True iff the first roster-set is the subset of second
;;           roster-set
;; EXAMPLES :
;; (roster-subset? ROSTER-SET1 ROSTER-SET1-WITH-POSITION-EXCHANGED)
;;   => TRUE
;; (roster-subset? ROSTER-SET2 ROSTER-SET1-WITH-POSITION-EXCHANGED)
;;   => FALSE

;; DESIGN STRATEGY : Use HOF andmap on roster-set1
(define (roster-subset? roster-set1 roster-set2)
  (andmap
   ;; ClassRoster -> Boolean
   ;; GIVEN : A ClassRoster
   ;; RETURNS : True iff the given roster is the member of the
   ;;           given SetOfClassRoster
   (lambda (roster) (is-roster-member? roster roster-set2))
   roster-set1))

;; TESTS :
(begin-for-test
  (check-equal?
   (roster-subset? ROSTER-SET1
                   ROSTER-SET1-WITH-POSITION-EXCHANGED)
   TRUE
   "Since the first roster set is subset of second roster set,
    #true is returned")
  (check-equal?
   (roster-subset? ROSTER-SET2
                   ROSTER-SET1-WITH-POSITION-EXCHANGED)
   FALSE
   "Since the first roster set is not a subset of second roster set,
    #false is returned"))
                   


;; Helper function for rosterset=?
;; is-roster-member? : ClassRoster SetOfClassRoster -> Boolean
;; GIVEN : A ClassRoster and SetOfClassRoster
;; RETURNS : True iff the given ClassRoster is member of SetOfClassRoster
;; EXAMPLES :
;; (is-roster-member? ROSTER-PDP1 ROSTER-SET1-WITH-POSITION-EXCHANGED)
;;   => TRUE
;; (is-roster-member? ROSTER-DB1 ROSTER-SET2) => FALSE

;; DESIGN STRATEGY : Use HOF ormap on roster-set
(define (is-roster-member? roster1 roster-set)
  (ormap
   ;; ClassRoster -> Boolean
   ;; GIVEN : A ClassRoster
   ;; RETURNS : True iff the given ClassRoster is equal to
   ;;           the first ClassRoster of given roster set
   (lambda (roster2) (roster=? roster1 roster2))
   roster-set))

;; TESTS :
(begin-for-test
  (check-equal?
   (is-roster-member? ROSTER-PDP1
                      ROSTER-SET1-WITH-POSITION-EXCHANGED)
   TRUE
   "Since the given roster is member of the roster set,
    #true is returned")
  (check-equal?
   (is-roster-member? ROSTER-DB1
                      ROSTER-SET2)
   FALSE
   "Since the given roster is not a member of the roster set,
    #false is returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples

(define ENROLLMENT-LIST1
  (list (make-enrollment "John" "PDP")
        (make-enrollment "Kathryn" "Networks")
        (make-enrollment "Feng" "PDP")
        (make-enrollment "Amy" "PDP")
        (make-enrollment "Amy" "Networks")))

(define ENROLLMENT-AMY-NETWORKS
  (make-enrollment "Amy" "Networks"))

(define ENROLLMENT-KATHRYN-NETWORKS
  (make-enrollment "Kathryn" "Networks"))

(define ROSTER-SET-WITH-AMY-NETWORKS
  (list
   (make-roster "Networks" (list "Amy"))))

(define ROSTER-AMY-NETWORKS
  (make-roster "Networks" (list "Amy")))

(define ROSTER-SET-WITH-AMY-KATHRYN
  (list
   (make-roster "Networks" (list "Kathryn" "Amy"))))

(define ROSTER-AMY-KATHRYN-NETWORKS
  (make-roster "Networks" (list "Kathryn" "Amy")))

(define ROSTER-SET-WITH-AMY-FENG-PDP-NETWORKS
  (list
   (make-roster "PDP" (list "Feng" "Amy"))
   (make-roster "Networks" (list "Amy"))))

(define ROSTER-SET-WITH-AMY-FENG-KATHRYN-PDP-NETWORKS
  (list
   (make-roster "PDP" (list "Feng" "Amy"))
   (make-roster "Networks" (list "Kathryn" "Amy"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enrollments-to-rosters : SetOfEnrollment -> SetOfClassRoster
;; GIVEN : A set of Enrollments
;; RETURNS : The set of ClassRoster for the given enrollments
;; EXAMPLES :
;; (enrollments-to-rosters ENROLLMENT-LIST1) => ROSTER-SET1-WITH-POSITION-EXCHANGED
;; (enrollments-to-rosters ENROLLMENT-LIST1) => ROSTER-SET1

;; DESIGN STRATEGY : Use of HOF foldr on set-enroll
(define (enrollments-to-rosters set-enroll)
  (foldr
   ;; Enrollment SetOfRosters -> SetOfRosters
   ;; GIVEN : An Enrollment and SetOfRosters
   ;; RETURNS : SetOfRosters with the Class and Student from
   ;;           Enrollment added to the given SetOfRosters
   (lambda (enroll set-rosters)
     (add-enroll-to-set-rosters enroll set-rosters))
   empty
   set-enroll))

;; TESTS :
(begin-for-test
  (check rosterset=?
         (enrollments-to-rosters ENROLLMENT-LIST1)
         ROSTER-SET1-WITH-POSITION-EXCHANGED
  "The output will be a set of rosters")
  (check rosterset=?
         (enrollments-to-rosters ENROLLMENT-LIST1)
         ROSTER-SET1
  "The output will be a set of rosters"))
          



;; Helper function for enrollments-to-rosters
;; add-enroll-to-set-rosters : Enrollment SetOfRosters -> SetOfRosters
;; GIVEN : An Enrollment and SetOfRosters
;; RETURNS : A SetOfRosters with the given Class and Student from Enrollment
;;           added to the given SetOfRosters
;; EXAMPLES :
;; (add-enroll-to-set-rosters ENROLLMENT-AMY-NETWORKS empty) =>
;;   ROSTER-SET-WITH-AMY-NETWORKS
;; (add-enroll-to-set-rosters ENROLLMENT-KATHRYN-NETWORKS
;;                            ROSTER-SET-WITH-AMY-NETWORKS) =>
;;   ROSTER-SET-WITH-AMY-KATHRYN

;; DESIGN STRATEGY : Use template for Enrollment on en
(define (add-enroll-to-set-rosters en sor)
  (if (class-present-in-set? sor (enrollment-class en))
      (add-student-to-set-rosters sor
                                  (enrollment-student en)
                                  (enrollment-class en))
      (add-class-student-to-set-rosters sor
                                        (enrollment-student en)
                                        (enrollment-class en))))

;; TESTS :
(begin-for-test
  (check-equal?
   (add-enroll-to-set-rosters ENROLLMENT-AMY-NETWORKS empty)
   ROSTER-SET-WITH-AMY-NETWORKS
   "Since the SOR is empty, given class and student is added to SOR")
  (check-equal?
   (add-enroll-to-set-rosters ENROLLMENT-KATHRYN-NETWORKS
                              ROSTER-SET-WITH-AMY-NETWORKS)
   ROSTER-SET-WITH-AMY-KATHRYN
   "Since the class is already present, the student is appended to
    the list of students of that roster"))
                                     


;; Helper function for add-enroll-to-set-rosters
;; GIVEN : SetOfRosters Class -> Boolean
;; RETURNS : True if the given class-name is present in the SetOfRosters
;; EXAMPLES :
;; (class-present-in-set? ROSTER-SET-WITH-AMY-NETWORKS "Networks") => TRUE
;; (class-present-in-set? ROSTER-SET-WITH-AMY-NETWORKS "PDP") => FALSE

;; DESIGN STRATEGY : Use of HOF ormap on sor
(define (class-present-in-set? sor class-name)
  (ormap
   ;; Roster -> Boolean
   ;; GIVEN : A Roster
   ;; RETURNS : True iff the roster-classname is equal to given
   ;;           class-name
    (lambda (roster)
      (equal? class-name (roster-classname roster)))
    sor))

;; TESTS :
(begin-for-test
  (check-equal?
   (class-present-in-set? ROSTER-SET-WITH-AMY-NETWORKS "Networks")
   TRUE
   "Since the given class is already present in SOR, #true is returned")
  (check-equal?
   (class-present-in-set? ROSTER-SET-WITH-AMY-NETWORKS "PDP")
   FALSE
   "Since the given class is not present in SOR, #false is returned"))
   


;; Helper function for add-enroll-to-set-rosters
;; add-student-to-set-rosters : SetOfRosters Student Class -> SetOfRosters
;; GIVEN : A SetOfRosters and values representing the Student name
;;         and Class name
;; RETURNS : A SetOfRosters with the given Student added to the
;;          approriate ClassRoster containing that Class
;; EXAMPLES :
;; (add-student-to-set-rosters ROSTER-SET-WITH-AMY-FENG-PDP-NETWORKS
;;                             "Kathryn"
;;                             "Networks") =>
;;   ROSTER-SET-WITH-AMY-FENG-KATHRYN-PDP-NETWORKS

;; DESIGN STRATEGY : Use HOF map on sor
(define (add-student-to-set-rosters sor student class-name)
  (map
   ;; Roster -> Roster
   ;; GIVEN : A Roster
   ;; RETURNS : An updated ClassRoster with Student added to the
   ;;           approrpriate ClassRoster, or the given ClassRoster
   ;;           based on whether the Class names match or not
   (lambda (roster)
     (if (equal? class-name (roster-classname roster))
         (update-roster roster student)
         roster))
   sor))

;; TESTS :
(begin-for-test
  (check-equal?
   (add-student-to-set-rosters ROSTER-SET-WITH-AMY-FENG-PDP-NETWORKS
                               "Kathryn"
                               "Networks")
   ROSTER-SET-WITH-AMY-FENG-KATHRYN-PDP-NETWORKS
   "The student is added to the list of students for given roster-classname"))
   
    

;; Helper function for add-student-to-set-rosters
;; update-roster : ClassRoster Student -> ClassRoster
;; GIVEN : A ClassRoster and value representing the Student
;; RETURNS : A ClassRoster with given Student added to the ListOfStudents
;;           of the ClassRoster
;; EXAMPLES :
;; (update-roster ROSTER-AMY-NETWORKS "Kathryn") =>
;;   ROSTER-AMY-KATHRYN-NETWORKS

;; DESIGN STRATEGY : Use of template for ClassRoster on roster
(define (update-roster roster student)
  (make-roster (roster-classname roster)
               (cons student (roster-students roster))))

;; TESTS :
(begin-for-test
  (check-equal?
   (update-roster ROSTER-AMY-NETWORKS
                  "Kathryn")
   ROSTER-AMY-KATHRYN-NETWORKS
   "The given student is addded to the roster"))



;; Helper function for add-enroll-to-set-rosters
;; add-class-student-to-set-rosters : SetOfRosters Student Class -> SetOfRosters
;; GIVEN : A SetOfRosters and 2 values representing the Student name
;;         and Class name
;; RETURNS : A SetOfRosters with the new ClassRoster added to the given one
;; EXAMPLES :
;; (add-class-student-to-set-rosters empty "Amy" "Networks") =>
;;   ROSTER-SET-WITH-AMY-NETWORKS

;; DESIGN STRATEGY : Use of template for ClassRoster
(define (add-class-student-to-set-rosters sor student class-name)
  (cons
   (make-roster class-name
                (cons student empty))
   sor))

;; TESTS :
(begin-for-test
  (check-equal?
   (add-class-student-to-set-rosters empty "Amy" "Networks")
   ROSTER-SET-WITH-AMY-NETWORKS
   "The given student and class is added to the SOR"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;