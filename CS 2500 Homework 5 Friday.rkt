;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |CS 2500 Homework 5 Friday|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 1
;; a
(define-struct charm [type material bracelet])
;; A CharmBracelet is one of:
;; - "clasp"
;; - (make-charm String Material CharmBracelet)
;; A CharmBracelet is either empty and just has a clasp or a (make-charm s m cb) where s is the
;; description of and m is the material of the charm added to the charm bracelet cb 
(define ex-charmbracelet-1 "clasp")
(define ex-charmbracelet-2 (make-charm "Unicorn" "silver" ex-charmbracelet-1))
(define ex-charmbracelet-3 (make-charm "Double-Heart" "gold" ex-charmbracelet-2))
(define ex-charmbracelet-4 (make-charm "Skull-n-Bones" "pewter" ex-charmbracelet-3))
(define ex-charmbracelet-5 (make-charm "Sailboat" "gold"
                                       (make-charm "Bear" "silver" ex-charmbracelet-4)))
;; charmbracelet-templ : CharmBracelet -> ??
#; (define (charmbracelet-templ cb)
     (cond
       [(and (string? cb) (string=? cb "clasp")) ...]
       [(charm? cb) (...(charm-type cb)...
                        (material-templ (charm-material cb))...
                        (charmbracelet-templ (charm-bracelet cb))...)]))

;; A Material is one of:
;; - "gold"
;; - "silver"
;; - "pewter"
;; Interpretation: the material that the charm bracelet is made out of
(define SILVER "silver")
(define GOLD "gold")
(define PEWTER "pewter")
;; material-templ : Material -> ??
#; (define (material-templ mt)
     (cond
       [(string=? mt "gold") ...]
       [(string=? mt "silver") ...]
       [(string=? mt "pewter") ...]))

;; b
;; bracelet-cost : CharmBracelet -> Natural
;; Takes a charm bracelet and computes the sale price for the bracelet based on the cost
;; of each charm
(check-expect (bracelet-cost ex-charmbracelet-1) 0)
(check-expect (bracelet-cost ex-charmbracelet-2) 12)
(check-expect (bracelet-cost ex-charmbracelet-3) 27)
(check-expect (bracelet-cost ex-charmbracelet-4) 37)
(check-expect (bracelet-cost ex-charmbracelet-5) 64)
(define (bracelet-cost cb)
  (cond
    [(and (string? cb) (string=? cb "clasp")) 0]
    [(charm? cb) (+ (material-cost (charm-material cb))
                    (bracelet-cost (charm-bracelet cb)))]))

;; HELPER FUNCTION
;; materical-cost : ______ -> Natural
;; Takes the material of the charm and calculates how much it costs
(check-expect (material-cost (charm-material ex-charmbracelet-2)) 12)
(check-expect (material-cost (charm-material ex-charmbracelet-3)) 15)
(check-expect (material-cost (charm-material ex-charmbracelet-4)) 10)
(check-expect (material-cost (charm-material ex-charmbracelet-5)) 15)
(check-expect (material-cost (charm-material
                              (make-charm "Sailboat" "gold"
                                          (make-charm "Bear" "silver"
                                                      (make-charm "Dog" "pewter"
                                                                  ex-charmbracelet-1))))) 15)
(define (material-cost cm)
  (cond
    [(string=? cm "gold") 15]
    [(string=? cm "silver") 12]
    [(string=? cm "pewter") 10]))
  
;; Exercise 2
;; a
(define-struct coloredbead [color size bracelet])
;; A FancyBracelet is one of:
;; - "clasp"
;; - (make-charm String Material FancyBracelet)
;; - (make-coloredbead String Number FancyBracelet)
;; A FancyBracelet is either empty and just has a clasp or a (make-charm s m cb) where s is the
;; description of and m is the material of the charm added to the charm bracelet cb or a
;; (make-coloredbead c s cb) where c is the color of and s is the size of the bead added to the
;; charm bracelet cb
(define ex-fancybracelet-1 "clasp")
(define ex-fancybracelet-2 (make-charm "Sailboat" "gold"
                                       (make-charm "Bear" "silver" ex-fancybracelet-1)))
(define ex-fancybracelet-3 (make-coloredbead "blue" 5 ex-fancybracelet-2))
(define ex-fancybracelet-4 (make-coloredbead "yellow" 10
                                             (make-coloredbead "red" 15 ex-fancybracelet-3)))
(define ex-fancybracelet-5 (make-charm "Star" "pewter"
                                       (make-coloredbead "purple" 20 ex-fancybracelet-4)))
(define ex-fancybracelet-6 (make-coloredbead "yellow" 20 ex-fancybracelet-4))
;; charmbracelet-templ : FancyBracelet -> ??
#; (define (fancybracelet-templ fb)
     (cond
       [(and (string? fb) (string=? fb "clasp")) ...]
       [(charm? fb) (...(charm-type fb)...
                        (material-templ (charm-material fb))...
                        (fancybracelet-templ (charm-bracelet fb))...)]
       [(coloredbead? fb) (...(coloredbead-color fb)...
                              (coloredbead-size fb)...
                              (fancybracelet-templ (coloredbead-bracelet fb)...))]))

;; b
;; count-charms : FancyBracelet -> Natural
;; Takes a fancy bracelet and counts the number of charms (and not beads) on the bracelet
(check-expect (count-charms ex-fancybracelet-1) 0)
(check-expect (count-charms ex-fancybracelet-2) 2)
(check-expect (count-charms ex-fancybracelet-3) 2)
(check-expect (count-charms ex-fancybracelet-4) 2)
(check-expect (count-charms ex-fancybracelet-5) 3)
(define (count-charms fb)
  (cond
    [(and (string? fb) (string=? fb "clasp")) 0]
    [(charm? fb) (add1 (count-charms (charm-bracelet fb)))]
    [(coloredbead? fb) (count-charms (coloredbead-bracelet fb))]))

;; c
;; upgrade-bracelet : FancyBracelet Color String -> FancyBracelet
;; Exchanges all of the beads of the given color in the bracelet for silver charms with the
;; requested figure. Any beads of other colors, as well as any current charms, should be left as-is.
(check-expect (upgrade-bracelet ex-fancybracelet-1 "red" "Tiger") "clasp")
(check-expect (upgrade-bracelet ex-fancybracelet-2 "blue" "Bear") ex-fancybracelet-2)
(check-expect (upgrade-bracelet ex-fancybracelet-3 "blue" "Bear")
              (make-charm "Bear" "silver" ex-fancybracelet-2))
(check-expect (upgrade-bracelet ex-fancybracelet-4 "yellow" "Star")
              (make-charm "Star" "silver" (make-coloredbead "red" 15 ex-fancybracelet-3)))
(check-expect(upgrade-bracelet ex-fancybracelet-5 "purple" "Star")
             (make-charm "Star" "pewter"
                         (make-charm "Star" "silver" ex-fancybracelet-4)))
(check-expect (upgrade-bracelet ex-fancybracelet-6 "yellow" "plane")
              (make-charm "plane" "silver"
                          (make-charm "plane" "silver"
                                      (make-coloredbead "red" 15 ex-fancybracelet-3))))
(define (upgrade-bracelet fb c f)
  (cond
    [(and (string? fb) (string=? fb "clasp")) "clasp"]
    [(charm? fb)
     (make-charm
      (charm-type fb)
      (charm-material fb)
      (upgrade-bracelet (charm-bracelet fb) c f))]
    [(coloredbead? fb)
     (if (string=? (coloredbead-color fb) c)
         (make-charm f "silver"(upgrade-bracelet (coloredbead-bracelet fb) c f))
         (make-coloredbead
          (coloredbead-color fb)
          (coloredbead-size fb)
          (upgrade-bracelet (coloredbead-bracelet fb) c f)))]))

;; Exercise 3
;; a
(define-struct student [firstname lastname gpa on-coop])
; A Student is a (make-student String String Number Boolean)
; Interpretation: A (make-student fn ln g c) represents a
; Northeastern student whose first name is fn and last name is ln, with 
; cumulative grade point average g, and for whom c is #true if they are
; currently doing a coop experience this term and #false otherwise.
(define student1 (make-student "Jane" "Smith" 4.0 #true))
(define student2 (make-student "Ashok" "Singhal" 0.0 #false))
(define student3 (make-student "Jack" "Frost" 2.0 #true))
(define (student-templ st)
  (... (student-firstname st) ...
       (student-lastname st) ...
       (student-gpa st) ...
       (student-on-coop st) ...))

(define-struct los [cons Student empty])
;; A ListOfStudents is one of:
;;- empty
;; (cons Student los)
;; Interpretation: Represents a list of students
(define los1 empty)
(define los2(cons student1 los1))
(define los3(cons student2 los2))
(define los4(cons student3 los3))
;;los-templ: los -> ??
(define (los-templ los)
  (cond
    [(empty? los)...]
    [(cons? los)(...(first los)...(los-templ(rest los))...)]))

;; b
;; count-coop-students: LOS -> Number
;; takes in a list of students and returns the number of students currently on coop
(check-expect(count-coop-students los1) 0)
(check-expect(count-coop-students los2) 1)
(check-expect(count-coop-students los3) 1)
(check-expect(count-coop-students los4) 2)
(define (count-coop-students los)
  (cond
    [(empty? los) 0]
    [(cons? los)(if(boolean=? (student-coop(first los)) #true)(add1(count-coop-students(rest los)))
                   (count-coop-students(rest los)))]))

;; HELPER FUNCTION
;; student-coop: LOS -> Boolean
;; determines if the first student in the ListOfStudents is on coop
(check-expect(student-coop(first los2)) #t)
(check-expect(student-coop(first los3)) #f)
(check-expect(student-coop(first los4)) #t)
(define (student-coop los)
  (if(boolean=? (student-on-coop los) #true) #true #false))

;; c
;; exchange-coop-students: ListofStudents -> ListOfStudents
;; Take in a list of students and switch the status of all the students coop status
(check-expect(exchange-coop-students los1) empty)
(check-expect(exchange-coop-students los2)(cons (make-student "Jane" "Smith" 4.0 #false) empty))
(check-expect(exchange-coop-students los3)(cons (make-student "Ashok" "Singhal" 0.0 #true)
                                                (cons (make-student "Jane" "Smith" 4.0 #false)
                                                      empty)))
(check-expect(exchange-coop-students los4)(cons (make-student "Jack" "Frost" 2.0 #false)
                                                (cons (make-student "Ashok" "Singhal" 0.0 #true)
                                                      (cons (make-student "Jane" "Smith" 4.0 #false)
                                                            empty))))
(define (exchange-coop-students los)
  (cond
    [(empty? los)empty]
    [(cons? los)(cons (change-coop (first los))
                      (exchange-coop-students(rest los)))]))

;; HELPER FUNCTION
;; change-coop: Student-> Student
;; flips the coop status of the student
(check-expect(change-coop student1)(make-student "Jane" "Smith" 4.0 #false))
(check-expect(change-coop student2)(make-student "Ashok" "Singhal" 0.0 #true))
(check-expect(change-coop student3)(make-student "Jack" "Frost" 2.0 #false))
(define (change-coop st)
  (make-student
   (student-firstname st)
   (student-lastname st)
   (student-gpa st)
   (not(student-on-coop st))))



