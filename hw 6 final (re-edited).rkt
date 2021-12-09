;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |hw 6 final (re-edited)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Tetris
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;; DATA DEFINITIONS ;;;;;;;;;;
(define-struct brick [x y color])

;; A Color is one of:
;; - "green"
;; - "dark blue"
;; - "purple"
;; - "sky blue"
;; - "orange"
;; - "pink"
;; - "red"
;; represents the color of a brick

;; A Brick is a (make-brick Integer Integer Color)
;; A (make-brick x-g y-g c) represents a square brick 
;; at position (x-g, y-g) in the grid, to be rendered in color c.

;; A Piece is one of:
;; - '()
;; (cons Brick Piece)

;; A Direction is one of:
;; - "left"
;; - "right"
;; represents the movement of a piece

(define-struct piece [center lob]) 
;; A Piece is a (make-piece Posn ListofBricks)
;; Interpretation: a (make-piece p l) represents a piece with its center at position _p_ and made
;; up of a list of bricks _l_

(define-struct world [piece])
;; A World is a (make-world Piece)
;; represents the world

;;-------------------------------------------
;; Initial Wish List:

;; big-bang clauses we think we'll need
#; (big-bang world0
    (to-draw world->scene)
    (on-tick world->world)
    (on-key key-handler))

;; world->scene : World -> Scene

;; world->world : World -> World

;; key-handler : World Key-Event -> World

;;;;;; Examples ;;;;;;
;; Color
(define green "green")
(define darkblue "dark blue")
(define purple "purple")
(define skyblue "sky blue")
(define orange "orange")
(define pink "pink")
(define red "red")
;; Brick
(define o1 (make-brick 0 0 green))
(define o2 (make-brick 1 0 green))
(define o3 (make-brick 1 1 green))
(define o4 (make-brick 0 1 green))
(define i1 (make-brick 0 0 darkblue))
(define i2 (make-brick 1 0 darkblue))
(define i3 (make-brick 2 0 darkblue))
(define i4 (make-brick 3 0 darkblue))
(define l1 (make-brick 2 0 purple))
(define l2 (make-brick 0 1 purple))
(define l3 (make-brick 1 1 purple))
(define l4 (make-brick 2 1 purple))
(define j1 (make-brick 0 0 skyblue))
(define j2 (make-brick 0 1 skyblue))
(define j3 (make-brick 1 1 skyblue))
(define j4 (make-brick 2 1 skyblue))
(define t1 (make-brick 1 0 orange))
(define t2 (make-brick 0 1 orange))
(define t3 (make-brick 1 1 orange))
(define t4 (make-brick 2 1 orange))
(define z1 (make-brick 0 0 pink))
(define z2 (make-brick 1 0 pink))
(define z3 (make-brick 1 1 pink))
(define z4 (make-brick 2 1 pink))
(define s1 (make-brick 1 0 red))
(define s2 (make-brick 2 0 red))
(define s3 (make-brick 0 1 red))
(define s4 (make-brick 1 1 red))
;; Piece
(define piece-o (list o1 o2 o3 o4))
(define piece-i (list i1 i2 i3 i4))
(define piece-l (list l1 l2 l3 l4))
(define piece-j (list j1 j2 j3 j4))
(define piece-t (list t1 t2 t3 t4))
(define piece-z (list z1 z2 z3 z4))
(define piece-s (list s1 s2 s3 s4))

;; World
(define world0 (make-world (list (make-brick 0 21 green))))
(define world1 (make-world piece-o))
(define world2 (make-world (list (make-brick 3 10 "purple")
                                 (make-brick 1 11 "purple")
                                 (make-brick 2 11 "purple")
                                 (make-brick 3 11 "purple"))))
(define world3 (make-world (list (make-brick 1 19 "orange")
                                 (make-brick 0 20 "orange")
                                 (make-brick 1 20 "orange")
                                 (make-brick 2 20 "orange"))))



;;;;;; CONSTANTS ;;;;;;;
(define GRID-SQSIZE 50) ; the width and height of the grid squares
(define BOARD-HEIGHT 20) ; the hieght of the game board in grid squares
(define BOARD-WIDTH 10) ; the width of the gram eboard in grid squares
(define BOARD-HEIGHT/PIX (* BOARD-HEIGHT GRID-SQSIZE)) ; board height in pixels
(define BOARD-WIDTH/PIX (* BOARD-WIDTH GRID-SQSIZE)) ; board width in pixels

(define BACKGROUND (empty-scene BOARD-WIDTH/PIX BOARD-HEIGHT/PIX))

;;;;;; Templates ;;;;;;
#; (define (color-templ c)
     (cond
       [(string=? "green") ...]
       [(string=? "dark blue") ...]
       [(string=? "purple") ...]
       [(string=? "sky blue") ...]
       [(string=? "orange") ...]
       [(string=? "pink") ...]
       [(string=? "red") ...]))

#; (define (piece-templ p)
     (cond
       [(empty? p) ...]
       [(cons? p) (... (first p) ...
                       (rest p) ...)]))

#; (define (brick-templ b)
     (... (brick-x b) ...
          (brick-y b) ...
          (color-templ (brick-color b) ...)))

#; (define (world-templ w)
     (... (world-piece w) ...))

;;;;;;;;; SHAPE FUNCTIONS ;;;;;;;;;
;; oshape: Gx Gy -> Piece
;; Takes in a grid coordinate and creates the corresponding o shape on the grid
(check-expect (oshape 2 2) (make-piece (make-posn 2 2)
                                       (list
                                        (make-brick 1.5 2.5 GREEN)
                                        (make-brick  1.5 1.5 GREEN)
                                        (make-brick  2.5 2.5 GREEN)
                                        (make-brick  2.5 1.5 GREEN))))
(check-expect (oshape 6 10) (make-piece (make-posn 6 10)
                                        (list
                                         (make-brick 5.5 10.5 GREEN)
                                         (make-brick  5.5 9.5 GREEN)
                                         (make-brick  6.5 10.5 GREEN)
                                         (make-brick  6.5 9.5 GREEN))))
                                        
(check-expect (oshape 8 18) (make-piece (make-posn 8 18)
                                        (list
                                         (make-brick 7.5 18.5 GREEN)
                                         (make-brick  7.5 17.5 GREEN)
                                         (make-brick  8.5 18.5 GREEN)
                                         (make-brick  8.5 17.5 GREEN))))
(define (oshape x y)
  (make-piece
   (make-posn x y)
   (list
    (make-brick  (- x 0.5) (+ y 0.5) GREEN)
    (make-brick  (- x 0.5) (- y 0.5) GREEN)
    (make-brick  (+ 0.5 x) (+ 0.5 y) GREEN)
    (make-brick  (+ 0.5 x) (- y 0.5) GREEN))))
;; ishape: Gx Gy -> Piece
;; Takes in a grid coordinate and creates the corresponding i shape on the grid
(check-expect (ishape 2 2) (make-piece (make-posn 2 2)
                                       (list
                                        (make-brick 1.5 2 BLUE)
                                        (make-brick 0.5 2 BLUE)
                                        (make-brick 2.5 2 BLUE)
                                        (make-brick 3.5 2 BLUE))))
                                        
(check-expect (ishape 6 10) (make-piece (make-posn 6 10)
                                        (list
                                         (make-brick 5.5 10 BLUE)
                                         (make-brick 4.5 10 BLUE)
                                         (make-brick 6.5 10 BLUE)
                                         (make-brick 7.5 10 BLUE))))
                                        
(check-expect (ishape 8 18) (make-piece (make-posn 8 18)
                                        (list
                                         (make-brick 7.5 18 BLUE)
                                         (make-brick 6.5 18 BLUE)
                                         (make-brick 8.5 18 BLUE)
                                         (make-brick 9.5 18 BLUE))))
(define (ishape x y)
  (make-piece
   (make-posn x y)
   (list
    (make-brick  (- x 0.5) y BLUE)
    (make-brick (- x 1.5) y BLUE)
    (make-brick (+ .5 x) y BLUE)
    (make-brick (+ x 1.5) y BLUE))))                                       
;; lshape: Gx Gy -> Piece
;; Takes in a grid coordinate and creates the corresponding l shape on the grid
(check-expect (lshape 2 2) (make-piece (make-posn 2 2)
                                       (list
                                        (make-brick 2 2 PURPLE)
                                        (make-brick 1 2 PURPLE)
                                        (make-brick 3 2 PURPLE)
                                        (make-brick 3 3 PURPLE))))
                                    
(check-expect (lshape 6 10) (make-piece (make-posn 6 10)
                                        (list
                                         (make-brick 6 10 PURPLE)
                                         (make-brick 5 10 PURPLE)
                                         (make-brick 7 10 PURPLE)
                                         (make-brick 7 11 PURPLE))))
                                        
(check-expect (lshape 8 18) (make-piece (make-posn 8 18)
                                        (list
                                         (make-brick 8 18 PURPLE)
                                         (make-brick 7 18 PURPLE)
                                         (make-brick 9 18 PURPLE)
                                         (make-brick 9 19 PURPLE))))                                   
(define (lshape x y)
  (make-piece
   (make-posn x y)
   (list
    (make-brick x y PURPLE)
    (make-brick (- x 1) y PURPLE)
    (make-brick (+ 1 x) y PURPLE)
    (make-brick (+ 1 x) (+ 1 y) PURPLE))))
;; jshape: Gx Gy -> Piece
;; Takes in a grid coordinate and creates the corresponding j shape on the grid
(check-expect (jshape 2 2) (make-piece (make-posn 2 2)
                                       (list
                                        (make-brick 2 2 CYAN)
                                        (make-brick 3 2 CYAN)
                                        (make-brick 1 2 CYAN)
                                        (make-brick 1 3 CYAN))))
                                     
(check-expect (jshape 6 10) (make-piece (make-posn 6 10)
                                        (list
                                         (make-brick 6 10 CYAN)
                                         (make-brick 7 10 CYAN)
                                         (make-brick 5 10 CYAN)
                                         (make-brick 5 11 CYAN))))                                      
(check-expect (jshape 8 18) (make-piece (make-posn 8 18)
                                        (list
                                         (make-brick 8 18 CYAN)
                                         (make-brick 9 18 CYAN)
                                         (make-brick 7 18 CYAN)
                                         (make-brick 7 19 CYAN))))                                      
(define (jshape x y)
  (make-piece
   (make-posn x y)
   (list
    (make-brick x y CYAN)
    (make-brick (+ 1 x) y CYAN)
    (make-brick  (- x 1) y CYAN)
    (make-brick (- x 1) (+ 1 y) CYAN))))
;; tshape: Gx Gy -> Piece
;; Takes in a grid coordinate and creates the corresponding t shape on the grid
(check-expect (tshape 2 2) (make-piece (make-posn 2 2)
                                       (list
                                        (make-brick 2 2 ORANGE)
                                        (make-brick 2 3 ORANGE)
                                        (make-brick 3 2 ORANGE)
                                        (make-brick 1 2 ORANGE))))
                                      
(check-expect (tshape 6 10) (make-piece (make-posn 6 10)
                                        (list
                                         (make-brick 6 10 ORANGE)
                                         (make-brick 6 11 ORANGE)
                                         (make-brick 7 10 ORANGE)
                                         (make-brick 5 10 ORANGE))))
                                      
(check-expect (tshape 8 18) (make-piece (make-posn 8 18)
                                        (list
                                         (make-brick 8 18 ORANGE)
                                         (make-brick 8 19 ORANGE)
                                         (make-brick 9 18 ORANGE)
                                         (make-brick 7 18 ORANGE))))                                      
(define (tshape x y)
  (make-piece
   (make-posn x y)
   (list
    (make-brick  x y ORANGE)
    (make-brick  x (+ 1 y) ORANGE)
    (make-brick  (+ 1 x) y ORANGE)
    (make-brick (- x 1) y ORANGE))))
;; zshape: Gx Gy -> Piece
;; Takes in a grid coordinate and creates the corresponding z shape on the grid
(check-expect (zshape 2 2) (make-piece (make-posn 2 2)
                                       (list
                                        (make-brick 2 2 PINK)
                                        (make-brick 1 2 PINK)
                                        (make-brick 2 1 PINK)
                                        (make-brick 3 1 PINK))))
                                      
(check-expect (zshape 6 10) (make-piece (make-posn 6 10)
                                        (list
                                         (make-brick 6 10 PINK)
                                         (make-brick 5 10 PINK)
                                         (make-brick 6 9 PINK)
                                         (make-brick 7 9 PINK))))
                                      
(check-expect (zshape 8 18) (make-piece (make-posn 8 18)
                                        (list
                                         (make-brick 8 18 PINK)
                                         (make-brick 7 18 PINK)
                                         (make-brick 8 17 PINK)
                                         (make-brick 9 17 PINK))))                                      
(define (zshape x y)
  (make-piece
   (make-posn x y)
   (list
    (make-brick  x y PINK)
    (make-brick (- x 1) y PINK)
    (make-brick  x (- y 1) PINK)
    (make-brick (+ 1 x) (- y 1) PINK))))
;; sshape: Gx Gy -> Piece
;; Takes in a grid coordinate and creates the corresponding s shape on the grid
(check-expect (sshape 2 2) (make-piece (make-posn 2 2)
                                       (list
                                        (make-brick 2 2 RED)
                                        (make-brick 3 2 RED)
                                        (make-brick 2 1 RED)
                                        (make-brick 1 1 RED))))
(check-expect (sshape 6 10) (make-piece (make-posn 6 10)
                                        (list
                                         (make-brick 6 10 RED)
                                         (make-brick 7 10 RED)
                                         (make-brick 6 9 RED)
                                         (make-brick 5 9 RED))))

(check-expect (sshape 8 18) (make-piece (make-posn 8 18)
                                        (list
                                         (make-brick 8 18 RED)
                                         (make-brick 9 18 RED)
                                         (make-brick 8 17 RED)
                                         (make-brick 7 17 RED))))                                      
(define (sshape x y)
  (make-piece
   (make-posn x y)
   (list
    (make-brick  x y RED)
    (make-brick  (+ 1 x) y RED)
    (make-brick x (- y 1) RED)
    (make-brick (- x 1) (- y 1) RED))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image/scene painting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; place-piece : Piece -> Piece
;; Places a piece at a given x, y grid position
(check-expect (place-piece piece-z 8 7) (list
                                         (make-brick 8 7 "pink")
                                         (make-brick 9 7 "pink")
                                         (make-brick 9 8 "pink")
                                         (make-brick 10 8 "pink")))
(check-expect (place-piece piece-j 6 6) (list
                                         (make-brick 6 6 "sky blue")
                                         (make-brick 6 7 "sky blue")
                                         (make-brick 7 7 "sky blue")
                                         (make-brick 8 7 "sky blue")))
(check-expect (place-piece piece-s 4 4) (list
                                         (make-brick 5 4 "red")
                                         (make-brick 6 4 "red")
                                         (make-brick 4 5 "red")
                                         (make-brick 5 5 "red")))
(define (place-piece s x y)
  (cond
    [(empty? s) empty]
    [(cons? s) (cons
                (make-brick (place-piece-helper (brick-x (first s)) x)
                            (place-piece-helper (brick-y (first s)) y)
                                  (brick-color (first s)))
                      (place-piece (rest s) x y))]))

;; place-piece-helper: Number -> Number
;; Takes a number between 0 and 2
;; and Depending on the number taken in perform a diffrent operation with n, an arbitrary number
(check-expect (place-piece-helper 2 3) 5)
(check-expect (place-piece-helper 0 2) 2)
(check-expect (place-piece-helper 1 4) 5)
(define (place-piece-helper x n)
  (cond
    [(= x 0) (+ x n)]
    [(= x 1) (+ (* x n) 1)]
    [(= x 2) (+ x n) ]))

;; random-piece : Piece Nat[0,9] -> Piece
;; generates random piece
(check-satisfied (random-piece piece-s (random 10)) cons?)
(define (random-piece p n)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (make-brick (+ n (brick-x (first p)))
                                 (brick-y (first p))
                                 (brick-color (first p)))
                     (random-piece (rest p) n))]))

;; regenerate-piece : Piece Nat[0,9] -> Piece
;; determines if a piece is in the grid, and returns a piece within the grid
(check-satisfied (regenerate-piece (random-piece piece-s (random 10))) cons?)
(define (regenerate-piece p)
  (if (piece-out-grid? p)
      (regenerate-piece (random-piece p (random 10)))
      p))
 
;; piece-out-grid? : Piece -> Boolean
;; Uses the brick-out-grid? function to determine whether any bricks' x values in the piece list are
;; greater than 9
(check-expect (piece-out-grid? (list (make-brick 10 0 "red")
                                     (make-brick 10 0 "red")
                                     (make-brick 10 0 "red")
                                     (make-brick 10 0 "red"))) #true)
(check-expect (piece-out-grid? (list (make-brick 5 5 "red")
                                     (make-brick 5 5 "red")
                                     (make-brick 5 5 "red")
                                     (make-brick 5 5 "red"))) #f)
(define (piece-out-grid? p)
  (cond
    [(empty? p) #false]
    [(cons? p) (or (brick-out-grid? (first p))
                   (piece-out-grid? (rest p)))]))

;; brick-out-grid? : Brick - >Boolean
;; Determines whether the given brick's x value is greater than 9
(check-expect (brick-out-grid? o1) #false)
(check-expect (brick-out-grid? (make-brick 30 30 "blue")) #t)
(define (brick-out-grid? b)
  (> (brick-x b) 9))
;; ---------------------------------------------------
;; draw-piece : Piece -> Image
;; takes a Piece, and produces its image
(check-expect (draw-piece piece-s) (place-images
                                    (list (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "red"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "red"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "red"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "red")))
                                    (list
                                     (make-posn 75 25)
                                     (make-posn 125 25)
                                     (make-posn 25 75)
                                     (make-posn 75 75)) BACKGROUND))

(check-expect (draw-piece piece-t) (place-images
                                    (list (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "orange"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "orange"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "orange"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "orange")))
                                    (list
                                     (make-posn 75 25)
                                     (make-posn 25 75)
                                     (make-posn 75 75)
                                     (make-posn 125 75)) BACKGROUND))

(check-expect (draw-piece piece-j) (place-images
                                    (list (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "skyblue"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "skyblue"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "skyblue"))
                                          (overlay (square GRID-SQSIZE "outline" "black")
                                                   (square GRID-SQSIZE "solid" "skyblue")))
                                    (list
                                     (make-posn 25 25)
                                     (make-posn 25 75)
                                     (make-posn 75 75)
                                     (make-posn 125 75)) BACKGROUND))
(define (draw-piece p)
  (place-images (image-list p)
                (posn-list p)
                BACKGROUND))

;; image-list : Piece-> List of Images
;; Takes in a piece and produces a list of images coorelating to the color of the piece
(check-expect (image-list piece-j) (list (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "skyblue"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "skyblue"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "skyblue"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "skyblue"))))

(check-expect (image-list piece-t) (list (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "orange"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "orange"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "orange"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "orange"))))

(check-expect (image-list piece-z) (list (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "pink"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "pink"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "pink"))
                                         (overlay (square GRID-SQSIZE "outline" "black")
                                                  (square GRID-SQSIZE "solid" "pink"))))
(define (image-list p)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (draw-brick-with-outline (first p))
                     (image-list (rest p)))]))

;; posn-list : Piece -> List of Posns
;; Takes in a piece and makes a list of posns from the values of the bricks inside the piece 
(check-expect (posn-list piece-t) (list
                                   (make-posn 75 25)
                                   (make-posn 25 75)
                                   (make-posn 75 75)
                                   (make-posn 125 75)))
(check-expect (posn-list piece-j) (list
                                   (make-posn 25 25)
                                   (make-posn 25 75)
                                   (make-posn 75 75)
                                   (make-posn 125 75)))
(check-expect (posn-list piece-i) (list
                                   (make-posn 25 25)
                                   (make-posn 75 25)
                                   (make-posn 125 25)
                                   (make-posn 175 25)))
(define (posn-list p)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (make-posn (+ (/ GRID-SQSIZE 2) (* GRID-SQSIZE (brick-x (first p))))
                                (+ (/ GRID-SQSIZE 2) (* GRID-SQSIZE (brick-y (first p)))))
                     (posn-list (rest p)))]))

;; draw-brick-with-outline : Brick -> Image
;; takes a Brick, and produces the image of a square with outline
(check-expect (draw-brick-with-outline z1) (overlay (square GRID-SQSIZE "outline" "black")
                                                    (square GRID-SQSIZE "solid" "pink")))
(check-expect (draw-brick-with-outline i1) (overlay (square GRID-SQSIZE "outline" "black")
                                                    (square GRID-SQSIZE "solid" "dark blue")))
(check-expect (draw-brick-with-outline s1) (overlay (square GRID-SQSIZE "outline" "black")
                                                    (square GRID-SQSIZE "solid" "red")))
(define (draw-brick-with-outline b)
  (overlay (draw-brick-outline b)
           (draw-brick b)))

;; draw-brick-outline : Brick -> Image
;; takes a Brick, and produces a square outline
(check-expect (draw-brick-outline z1) (square GRID-SQSIZE "outline" "black"))
(check-expect (draw-brick-outline j1) (square GRID-SQSIZE "outline" "black"))
(check-expect (draw-brick-outline o1) (square GRID-SQSIZE "outline" "black"))
(define (draw-brick-outline b)
  (square GRID-SQSIZE "outline" "black"))

;; draw-brick : Brick -> Image
;; takes a Brick, and produces a solid square image
(check-expect (draw-brick z1) (square GRID-SQSIZE "solid" "pink"))
(check-expect (draw-brick t1) (square GRID-SQSIZE "solid" "orange"))
(check-expect (draw-brick i1) (square GRID-SQSIZE "solid" "dark blue"))
(define (draw-brick b)
  (square GRID-SQSIZE "solid" (brick-color b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Piece movement functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; piece-rotate-ccw : Piece -> Piece
;; rotates the piece _p_ 90 degrees counterclockwise around the center _c_.
(check-expect (piece-rotate-ccw (make-posn 0 0) piece-s) (cons
                                                          (make-brick 0 -1 "red")
                                                          (cons (make-brick 0 -2 "red")
                                                                (cons (make-brick 1 0 "red")
                                                                      (cons
                                                                       (make-brick 1 -1 "red")
                                                                       '())))))
(check-expect (piece-rotate-ccw (make-posn 1 1) piece-z) (cons
                                                          (make-brick 0 2 "pink")
                                                          (cons (make-brick 0 1 "pink")
                                                                (cons (make-brick 1 1 "pink")
                                                                      (cons
                                                                       (make-brick 1 0 "pink")
                                                                       '())))))
(check-expect (piece-rotate-ccw (make-posn 1 1) piece-j) (cons
                                                          (make-brick 0 2 "sky blue")
                                                          (cons (make-brick 1 2 "sky blue")
                                                                (cons (make-brick 1 1 "sky blue")
                                                                      (cons
                                                                       (make-brick 1 0 "sky blue")
                                                                       '())))))
(define (piece-rotate-ccw c p)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (brick-rotate-ccw c (first p))
                     (piece-rotate-ccw c (rest p)))]))


;; brick-rotate-ccw : Posn Brick -> Brick
;; Rotate the brick _b_ 90 degrees counterclockwise around the center _c_.
(check-expect (brick-rotate-ccw (make-posn 0 0) z1) (make-brick 0 0 "pink"))
(check-expect (brick-rotate-ccw (make-posn 1 1) i1) (make-brick 0 2 "dark blue"))
(check-expect (brick-rotate-ccw (make-posn 1 0) t1) (make-brick 1 0 "orange"))
(define (brick-rotate-ccw c b)
  (make-brick (- (posn-x c)
                 (- (posn-y c)
                    (brick-y b)))
              (- (posn-y c)
                 (- (brick-x b)
                    (posn-x c)))
              (brick-color b)))
;; ----------------------------------------------------
;; piece-rotate-cw : Piece -> Piece
;; rotates the piece _p_ 90 degrees clockwise around the center _c_.
(check-expect (piece-rotate-cw (make-posn 1 1) piece-j) (cons
                                                         (make-brick 2 0 "sky blue")
                                                         (cons (make-brick 1 0 "sky blue")
                                                               (cons (make-brick 1 1 "sky blue")
                                                                     (cons
                                                                      (make-brick 1 2 "sky blue")
                                                                      '())))))
(check-expect (piece-rotate-cw (make-posn 1 1) piece-l) (cons
                                                         (make-brick 2 2 "purple")
                                                         (cons (make-brick 1 0 "purple")
                                                               (cons (make-brick 1 1 "purple")
                                                                     (cons (make-brick 1 2 "purple")
                                                                           '())))))
(check-expect (piece-rotate-cw (make-posn 1 1) piece-t) (cons
                                                         (make-brick 2 1 "orange")
                                                         (cons (make-brick 1 0 "orange")
                                                               (cons (make-brick 1 1 "orange")
                                                                     (cons
                                                                      (make-brick 1 2 "orange")
                                                                      '()))))) 
(define (piece-rotate-cw c p)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (brick-rotate-cw c (first p))
                     (piece-rotate-cw c (rest p)))]))

;; brick-rotate-cw : Posn Brick -> Brick
;; Rotate the brick _b_ 90 degrees clockwise around the center _c_.
(check-expect (brick-rotate-cw (make-posn 0 0) s1) (make-brick 0 1 "red"))
(check-expect (brick-rotate-cw (make-posn 1 1) o1) (make-brick 2 0 "green"))
(check-expect (brick-rotate-cw (make-posn 1 1) j1) (make-brick 2 0 "sky blue"))
(define (brick-rotate-cw c b)
  (make-brick (+ (posn-x c)
                 (- (posn-y c)
                    (brick-y b)))
              (+ (posn-y c)
                 (- (brick-x b)
                    (posn-x c)))
              (brick-color b)))
;; ----------------------------------------------------
;; find-center : Piece -> Posn
;; takes a Piece, and returns the Posn of the center of the Piece
(check-expect (find-center piece-s) (make-posn 1 1))
(check-expect (find-center piece-o) (make-posn 0.5 0.5))
(check-expect (find-center piece-i) (make-posn 1.5 0.5))
(check-expect (find-center piece-l) (make-posn 2 1))
(check-expect (find-center piece-j) (make-posn 0 1))
(check-expect (find-center piece-z) (make-posn 1 1))
(check-expect (find-center piece-t) (make-posn 1 1))
(define (find-center p)
  (cond
    [(string=? (brick-color (first p)) "green") (find-center-o p)]
    [(string=? (brick-color (first p)) "dark blue") (find-center-i p)]
    [(string=? (brick-color (first p)) "purple") (find-center-ljtzs p)]
    [(string=? (brick-color (first p)) "sky blue") (find-center-ljtzs p)]
    [(string=? (brick-color (first p)) "orange") (find-center-ljtzs p)]
    [(string=? (brick-color (first p)) "pink") (find-center-ljtzs p)]
    [(string=? (brick-color (first p)) "red") (find-center-ljtzs p)]))
;; -----------------------
;; find-center-o : Piece -> Posn
;; takes a Piece of shape "O", and returns the Posn of the center of the Piece
(check-expect (find-center-o piece-o) (make-posn 0.5 0.5))
(check-expect (find-center-o (piece-rotate-ccw (make-posn 1 1) piece-o)) (make-posn 0.5 1.5))
(check-expect (find-center-o (piece-rotate-ccw (make-posn 1 0) piece-o)) (make-posn 1.5 0.5))
(define (find-center-o p)
  (make-posn (/ (find-center-o-x p) 4) (/ (find-center-o-y p) 4)))

;; find-center-o : Piece -> Number
;; takes a Piece of shape "O", and returns the x-coordinate of the center of the Piece
(check-expect (find-center-o-x piece-o) 2)
(check-expect (find-center-o-x (place-piece piece-o 2 2)) 10)
(check-expect (find-center-o-x (place-piece piece-o 1 1)) 6)
(define (find-center-o-x p)
  (cond
    [(empty? p) 0]
    [(cons? p) (+ (brick-x (first p))
                  (find-center-o-x (rest p)))]))

;; find-center-o : Piece -> Number
;; takes a Piece of shape "O", and returns the y-coordinate of the center of the Piece
(check-expect (find-center-o-y piece-o) 2)
(check-expect (find-center-o-y (place-piece piece-o 2 2)) 10)
(check-expect (find-center-o-y (place-piece piece-o 2 1)) 6)
(define (find-center-o-y p)
  (cond
    [(empty? p) 0]
    [(cons? p) (+ (brick-y (first p))
                  (find-center-o-y (rest p)))]))
;; -------------------------------------------------------- 
;; find-center-i : Piece -> Posn
;; takes a Piece of shape "I", and returns the Posn of the center of the Piece
(check-expect (find-center-i piece-i) (make-posn 1.5 0.5))
(check-expect (find-center-i (piece-rotate-ccw (make-posn 1 0.5) piece-i)) (make-posn 0 0))
(check-expect (find-center-i (piece-rotate-cw (make-posn 0 0.5) piece-i)) (make-posn 0 2))
(define (find-center-i p)
  (if (vertical-i? p)
      (find-center-vertical-i p)
      (find-center-horizontal-i p))) 

;; find-center-vertical-i : Piece -> Posn
;; takes a Piece of vertical shape "I", and returns the Posn of the center of the Piece
(check-expect (find-center-vertical-i piece-i) (make-posn 0.5 0))
(check-expect (find-center-vertical-i (piece-rotate-ccw (make-posn 1.5 0) piece-i)) (make-posn 1 0))
(check-expect (find-center-vertical-i (piece-rotate-ccw (make-posn 1 .5) piece-i)) (make-posn 0 0))
(define (find-center-vertical-i p)
  (make-posn (find-center-vertical-x-i p)
             (find-center-vertical-y-i p)))

;; find-center-vertical-x-i : Piece -> Number
;; takes a Piece of vertical shape "I", and returns the x-coordinate of the center of the Piece
(check-expect (find-center-vertical-x-i piece-i) 0.5)
(check-expect (find-center-vertical-x-i (piece-rotate-ccw (make-posn 1 0) piece-i)) 0.5)
(check-expect (find-center-vertical-x-i (piece-rotate-cw (make-posn 1 0) piece-i)) 0.5)
(define (find-center-vertical-x-i p)
  (if (< (posn-x (take-brick p 0 0)) (posn-x (take-brick p 3 0)))
      (+ (brick-x (first p)) 0.5)
      (- (brick-x (first p)) 0.5)))

;; find-center-vertical-y-i : Piece -> Number
;; takes a Piece of vertical shape "I", and returns the y-coordinate of the center of the Piece
(check-expect (find-center-vertical-y-i piece-i) 0)
(check-expect (find-center-vertical-y-i (piece-rotate-cw (make-posn 1 1) piece-i)) 1.5)
(check-expect (find-center-vertical-y-i (piece-rotate-cw (make-posn 2 1) piece-i)) 0.5)
(define (find-center-vertical-y-i p)
  (cond
    [(empty? p) 0]
    [(cons? p) (+ (/ (brick-y (first p)) 4)
                  (find-center-vertical-y-i (rest p)))]))

;; find-center-horizontal-i : Piece -> Posn
;; takes a Piece of horizontal shape "I", and returns the Posn of the center of the Piece
(check-expect (find-center-horizontal-i piece-i) (make-posn 1.5 0.5))
(check-expect (find-center-horizontal-i (piece-rotate-cw (make-posn 1 1.5) piece-i))
              (make-posn 2.5 0))
(check-expect (find-center-horizontal-i (piece-rotate-ccw (make-posn 2 0) piece-i)) (make-posn 2 2.5))
(define (find-center-horizontal-i p)
  (make-posn (find-center-horizontal-x-i p)
             (find-center-horizontal-y-i p)))

;; find-center-horizontal-x-i : Piece -> Number
;; takes a Piece of horizontal shape "I", and returns the x-coordinate of the center of the Piece
(check-expect (find-center-horizontal-x-i piece-i) 1.5)
(check-expect (find-center-horizontal-x-i (piece-rotate-ccw (make-posn 1 1) piece-i)) 0)
(check-expect (find-center-horizontal-x-i (piece-rotate-cw (make-posn 1 1) piece-i)) 2)
(define (find-center-horizontal-x-i p)
  (cond
    [(empty? p) 0]
    [(cons? p) (+ (/ (brick-x (first p)) 4)
                  (find-center-horizontal-x-i (rest p)))]))

;; find-center-horizontal-y-i : Piece -> Number
;; takes a Piece of horizontal shape "I", and returns the y-coordinate of the center of the Piece
(check-expect (find-center-horizontal-y-i piece-i) 0.5)
(check-expect (find-center-horizontal-y-i (piece-rotate-ccw (make-posn 1 1) piece-i)) 2.5)
(check-expect (find-center-horizontal-y-i (piece-rotate-ccw (make-posn 2 2) piece-i)) 4.5)
(define (find-center-horizontal-y-i p) 
  (if (< (posn-y (take-brick p 0 0)) (posn-y (take-brick p 3 0)))
      (- (brick-y (first p)) 0.5)
      (+ (brick-y (first p)) 0.5)))

;; vertical-i? : Piece -> Boolean
;; takes a Piece, and check if its Bricks are aligned vertically
(check-expect (vertical-i? piece-j) #t)
(check-expect (vertical-i? piece-o) #f)
(check-expect (vertical-i? piece-t) #f)
(define (vertical-i? p)
  (= (brick-x (first p))
     (posn-x (take-brick p 1 0))))

;;-----------------------------------------------------------
;; find-center-ljtzs : Piece -> Posn
;; takes a Piece of shape "L", "J", "T", "Z", or "S", and returns the Posn of the center of the Piece
(check-expect (find-center-ljtzs piece-s) (make-posn 1 1))
(check-expect (find-center-ljtzs piece-t) (make-posn 1 1))
(check-expect (find-center-ljtzs piece-j) (make-posn 0 1))
(check-expect (find-center-ljtzs piece-z) (make-posn 1 1))
(check-expect (find-center-ljtzs piece-l) (make-posn 2 1))
(define (find-center-ljtzs p)
  (cond
    [(string=? (brick-color (first p)) purple) (take-brick p 3 0)]
    [(string=? (brick-color (first p)) skyblue) (take-brick p 1 0)]
    [(string=? (brick-color (first p)) orange) (take-brick p 2 0)]
    [(string=? (brick-color (first p)) pink) (take-brick p 2 0)]
    [(string=? (brick-color (first p)) red) (take-brick p 3 0)]))
;; ---------------------------------------------------------
;; take-brick : Piece Number Number -> Piece
;; takes a Piece _p_, number _n_, and number _c_ (which is always 0),
;; and return the _n_th brick in the Piece _p_
(check-expect (take-brick piece-s 5 0) '())
(check-expect (take-brick piece-j 2 0) (make-posn 1 1))
(check-expect (take-brick piece-o 1 1) (make-posn 0 0))
(define (take-brick p n c)
  (cond
    [(empty? p) empty]
    [(cons? p) (if (= c n)
                   (make-posn (brick-x (first p))
                              (brick-y (first p)))
                   (take-brick (rest p) n (add1 c)))]))

;; Exercise 4
;; world->scene : World -> Scene
;; food+scene : Food Scene -> Scene
;; snake+scene : Snake Scene -> Scene

;; world->world : World -> World

;; snake-move : Snake -> Snake
;; change direction in response to key events 
;; snake-eat : 
;; snake-grow : 

;; eating? : 

;; key-handler : World Key-Event -> World

;; world-done? : World -> Boolean
;; below-grid? :
;; rotate-out-grid? :
;;-----------------------------------------------------------
;; below-grid? : World -> Boolean
;; check if the Piece in World is below grid
(check-expect (below-grid? world0) #true)
(check-expect (below-grid? world1) #false)
(check-expect (below-grid? world2) #false)
(define (below-grid? w)
  (> (piece-min-y (world-piece w)) BOARD-HEIGHT))

;; piece-min-y : Piece -> Boolean
;; check if the entire Piece has fallen below the grid
(check-expect (piece-min-y (world-piece world0)) 21)
(check-expect (piece-min-y (world-piece world1)) 0)
(check-expect (piece-min-y (world-piece world2)) 10)
(check-expect (piece-min-y (world-piece world3)) 19)
(define (piece-min-y p)
  (cond
    [(empty? p) 100]
    [(cons? p) (min (brick-y (first p))
                    (piece-min-y (rest p)))]))
;;-----------------------------------------------------------
;; piece-rotate-out-grid? : Piece -> Boolean
;; check if the rotation result in any part of the piece being outside the grid
(check-expect (piece-rotate-out-grid? (piece-rotate-cw (find-center piece-o) piece-o)) #false)
(check-expect (piece-rotate-out-grid? (piece-rotate-ccw (find-center piece-i) piece-i)) #true)
(check-expect (piece-rotate-out-grid? (piece-rotate-cw
                                       (find-center (list (make-brick 9 0 "purple")
                                                          (make-brick 7 1 "purple")
                                                          (make-brick 8 1 "purple")
                                                          (make-brick 9 1 "purple")))
                                       (list (make-brick 9 0 "purple")
                                             (make-brick 7 1 "purple")
                                             (make-brick 8 1 "purple")
                                             (make-brick 9 1 "purple")))) #true)
(define (piece-rotate-out-grid? p)
  (cond
    [(empty? p) #false]
    [(cons? p) (or (brick-rotate-out-grid? (first p))
                   (piece-rotate-out-grid? (rest p)))]))

;; brick-rotate-out-grid? : Brick -> Boolean
;; check if the rotation results in the Brick being outside the grid
(check-expect (brick-rotate-out-grid? (brick-rotate-cw (find-center piece-l) l1)) #false)
(check-expect (brick-rotate-out-grid? (brick-rotate-ccw (find-center piece-z) z1)) #false)
(check-expect (brick-rotate-out-grid? (brick-rotate-cw (make-posn 9 1)
                                                       (make-brick 9 0 "purple"))) #true)
(define (brick-rotate-out-grid? b)
  (or (brick-x-rotate-out-grid? b)
      (brick-y-rotate-out-grid? b)))

;; brick-x-rotate-out-grid? : Brick -> Boolean
;; check if the rotation results in the x-coordinate of the Brick being outside the grid
(check-expect (brick-x-rotate-out-grid? (brick-rotate-cw (find-center piece-l) l1)) #false)
(check-expect (brick-x-rotate-out-grid? (brick-rotate-ccw (find-center piece-z) z1)) #false)
(check-expect (brick-x-rotate-out-grid? (brick-rotate-cw (make-posn 9 1)
                                                         (make-brick 9 0 "purple"))) #true)
(define (brick-x-rotate-out-grid? b)
  (or (< (brick-x b) 0)
      (> (brick-x b) (- BOARD-WIDTH 1))))

;; brick-y-rotate-out-grid? : Brick -> Boolean
;; check if the rotation results in the y-coordinate of the Brick being outside the grid
(check-expect (brick-y-rotate-out-grid? (brick-rotate-cw (find-center piece-l) l1)) #false)
(check-expect (brick-y-rotate-out-grid? (brick-rotate-ccw (find-center piece-z) z1)) #false)
(check-expect (brick-y-rotate-out-grid? (brick-rotate-ccw (make-posn 7 1)
                                                          (make-brick 9 1 "purple"))) #true)
(define (brick-y-rotate-out-grid? b)
  (< (brick-y b) 0))
;;-----------------------------------------------------------
;; world->scene : World -> Image
;; takes a World, and produces its image
(check-expect (world->scene (make-world piece-s))
              (place-images (list (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "red"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "red"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "red"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "red")))
                            (list
                             (make-posn 75 25)
                             (make-posn 125 25)
                             (make-posn 25 75)
                             (make-posn 75 75)) BACKGROUND))
(check-expect (world->scene (make-world piece-j))
              (place-images (list (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "skyblue"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "skyblue"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "skyblue"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "skyblue")))
                            (list
                             (make-posn 25 25)
                             (make-posn 25 75)
                             (make-posn 75 75)
                             (make-posn 125 75)) BACKGROUND))
(check-expect (world->scene (make-world piece-t))
              (place-images (list (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "orange"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "orange"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "orange"))
                                  (overlay (square GRID-SQSIZE "outline" "black")
                                           (square GRID-SQSIZE "solid" "orange")))
                            (list
                             (make-posn 75 25)
                             (make-posn 25 75)
                             (make-posn 75 75)
                             (make-posn 125 75)) BACKGROUND))
(define (world->scene w)
  (draw-piece (world-piece w)))

;; take-random-piece : Nat[0,6] Piece
;; takes a random piece
(check-satisfied (take-ramdom-piece (random 7)) cons?)
(define (take-ramdom-piece n)
  (cond
    [(= 0 n) piece-o]
    [(= 1 n) piece-i]
    [(= 2 n) piece-l]
    [(= 3 n) piece-j]
    [(= 4 n) piece-t]
    [(= 5 n) piece-z]
    [(= 6 n) piece-s]))
;;-----------------------------------------------------------
;; world->world : World -> World
;; takes the current world, generate a new world with a random piece
;; at a random x-coordinate on the top of the grid if the entire piece in the current world
;; is below the grid. Otherwise, make the piece in the current world fall.
(check-expect (world->world world1) (make-world (list (make-brick 0 1 "green")
                                                      (make-brick 1 1 "green")
                                                      (make-brick 1 2 "green")
                                                      (make-brick 0 2 "green"))))
(define (world->world w)
  (if (below-grid? w) 
      (make-world (regenerate-piece
                   (random-piece
                    (take-ramdom-piece (random 7)) (random 10))))
      (world-fall w)))
                  
;; world-fall : World -> World
;; takes a World, and maked the piece in the world fall by 1 unit of y-coordinate
;(check-expect (world-fall world0))
(check-expect (world-fall world0) (make-world (list (make-brick 0 22 "green"))))
(define (world-fall w)
  (make-world (piece-fall (world-piece w))))

;; piece-fall : Piece -> Piece
;; takes a Piece, and returns a Piece with 1 added to the y-coordinate of every Brick in it
(check-expect (piece-fall piece-o) (list (make-brick 0 1 "green")
                                         (make-brick 1 1 "green")
                                         (make-brick 1 2 "green")
                                         (make-brick 0 2 "green")))
(define (piece-fall p)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (brick-fall (first p))
                     (piece-fall (rest p)))]))

;; brick-fall : Brick -> Brick
;; takes a Brick, and returns a Brick with its y-coordinate added 1
(check-expect (brick-fall o1) (make-brick 0 1 green))
(check-expect (brick-fall (make-brick 3 10 green)) (make-brick 3 11 green))
(check-expect (brick-fall (make-brick 8 19 red)) (make-brick 8 20 red)) 
(define (brick-fall b)
  (make-brick (brick-x b)
              (add1 (brick-y b))
              (brick-color b)))
;;-----------------------------------------------------------
;; key-handler : World Key-Event -> World
;; handle things when the user hits a key on the keyboard.
(check-expect (key-handler world1 "left") (make-world (list (make-brick 0 0 "green")
                                                            (make-brick 1 0 "green")
                                                            (make-brick 1 1 "green")
                                                            (make-brick 0 1 "green"))))
(check-expect (key-handler world1 "right") (make-world (list (make-brick 1 0 "green")
                                                             (make-brick 2 0 "green")
                                                             (make-brick 2 1 "green")
                                                             (make-brick 1 1 "green"))))
(check-expect (key-handler world2 "s") (make-world (list (make-brick 4 11 "purple")
                                                         (make-brick 3 9 "purple")
                                                         (make-brick 3 10 "purple")
                                                         (make-brick 3 11 "purple"))))
(check-expect (key-handler world3 "a") (make-world (list (make-brick 0 20 "orange")
                                                         (make-brick 1 21 "orange")
                                                         (make-brick 1 20 "orange")
                                                         (make-brick 1 19 "orange"))))
(check-expect (key-handler world3 "c") (make-world (list (make-brick 1 19 "orange")
                                                         (make-brick 0 20 "orange")
                                                         (make-brick 1 20 "orange")
                                                         (make-brick 2 20 "orange"))))
(define (key-handler w k)
  (cond
    [(key=? k "left") (return-left-world w)]
    [(key=? k "right") (return-right-world w)]
    [(key=? k "s") (return-cw-world w)]
    [(key=? k "a") (return-ccw-world w)]
    [else w]))

;; return-left-world : World -> World
;; takes a World, and returns a World with its piece shifted leftwards by 1
;; if this operation will not result in the piece going outside the grid.
;; Otherwise, return the current world.
(check-expect (return-left-world world1) (make-world (list (make-brick 0 0 "green")
                                                           (make-brick 1 0 "green")
                                                           (make-brick 1 1 "green")
                                                           (make-brick 0 1 "green"))))
(check-expect (return-left-world (make-world (list (make-brick 1 0 "green")
                                                   (make-brick 2 0 "green")
                                                   (make-brick 2 1 "green")
                                                   (make-brick 1 1 "green"))))
              (make-world (list (make-brick 0 0 "green")
                                (make-brick 1 0 "green")
                                (make-brick 1 1 "green")
                                (make-brick 0 1 "green"))))
(check-expect (return-left-world (make-world (list (make-brick 1 0 "green")
                                                   (make-brick 2 0 "green")
                                                   (make-brick 2 1 "green")
                                                   (make-brick 1 1 "green"))))
              (make-world (list (make-brick 0 0 "green")
                                (make-brick 1 0 "green")
                                (make-brick 1 1 "green")
                                (make-brick 0 1 "green")))) 
(define (return-left-world w)
  (if (piece-rotate-out-grid? (piece-shift-left (world-piece w)))
      w
      (make-world (piece-shift-left (world-piece w)))))

;; return-right-world : World -> World
;; takes a World, and returns a World with its piece shifted rightwards by 1
;; if this operation will not result in the piece going outside the grid.
;; Otherwise, return the current world.
(check-expect (return-right-world world1) (make-world (list (make-brick 1 0 "green")
                                                            (make-brick 2 0 "green")
                                                            (make-brick 2 1 "green")
                                                            (make-brick 1 1 "green"))))
(check-expect (return-right-world world2) (make-world (list
                                                       (make-brick 4 10 "purple")
                                                       (make-brick 2 11 "purple")
                                                       (make-brick 3 11 "purple")
                                                       (make-brick 4 11 "purple"))))
(check-expect (return-right-world (make-world (list (make-brick 8 1 "green")
                                                    (make-brick 9 1 "green")
                                                    (make-brick 9 2 "green")
                                                    (make-brick 8 2 "green"))))
              (make-world (list (make-brick 8 1 "green")
                                (make-brick 9 1 "green")
                                (make-brick 9 2 "green")
                                (make-brick 8 2 "green")))) 


(define (return-right-world w) 
  (if (piece-rotate-out-grid? (piece-shift-right (world-piece w)))
      w
      (make-world (piece-shift-right (world-piece w)))))

;; return-cw-world : World -> World
;; takes a World, and returns a World with its piece 90 degrees clockwise around its center
;; if this operation will not result in the piece going outside the grid.
;; Otherwise, return the current world.
(check-expect (return-cw-world world1) (make-world (list (make-brick 1 0 "green")
                                                         (make-brick 1 1 "green")
                                                         (make-brick 0 1 "green")
                                                         (make-brick 0 0 "green"))))
(check-expect (return-cw-world world2) (make-world (list (make-brick 4 11 "purple")
                                                         (make-brick 3 9 "purple")
                                                         (make-brick 3 10 "purple")
                                                         (make-brick 3 11 "purple"))))
(check-expect (return-cw-world (make-world (list (make-brick 9 0 "purple")
                                                 (make-brick 7 1 "purple")
                                                 (make-brick 8 1 "purple")
                                                 (make-brick 9 1 "purple"))))
              (make-world (list (make-brick 9 0 "purple")
                                (make-brick 7 1 "purple")
                                (make-brick 8 1 "purple")
                                (make-brick 9 1 "purple"))))
               
(define (return-cw-world w)
  (if (piece-rotate-out-grid? (piece-rotate-cw (find-center (world-piece w)) (world-piece w)))
      w
      (make-world (piece-rotate-cw (find-center (world-piece w)) (world-piece w)))))

;; return-ccw-world : World -> World
;; takes a World, and returns a World with its piece 90 degrees counterclockwise around its center
;; if this operation will not result in the piece going outside the grid.
;; Otherwise, return the current world.
(check-expect (return-ccw-world world2) (make-world (list (make-brick 2 11 "purple")
                                                          (make-brick 3 13 "purple")
                                                          (make-brick 3 12 "purple")
                                                          (make-brick 3 11 "purple"))))
(check-expect (return-ccw-world world3) (make-world (list (make-brick 0 20 "orange")
                                                          (make-brick 1 21 "orange")
                                                          (make-brick 1 20 "orange")
                                                          (make-brick 1 19 "orange"))))
(check-expect (return-ccw-world (make-world piece-j)) (make-world (list
                                                                   (make-brick 0 0 "sky blue")
                                                                   (make-brick 0 1 "sky blue")
                                                                   (make-brick 1 1 "sky blue")
                                                                   (make-brick 2 1 "sky blue"))))
(define (return-ccw-world w)
  (if (piece-rotate-out-grid? (piece-rotate-ccw (find-center (world-piece w)) (world-piece w)))
      w
      (make-world (piece-rotate-ccw (find-center (world-piece w)) (world-piece w)))))

;; piece-shift-left : Piece -> Piece
;; takes a Piece, and produces a Piece with each of the input Piece's
;; Brick's x-coordinate substracted by 1
(check-expect (piece-shift-left piece-o) (list (make-brick -1 0 "green")
                                               (make-brick 0 0 "green")
                                               (make-brick 0 1 "green")
                                               (make-brick -1 1 "green")))
(check-expect (piece-shift-left piece-z) (list (make-brick -1 0 "pink")
                                               (make-brick 0 0 "pink")
                                               (make-brick 0 1 "pink")
                                               (make-brick 1 1 "pink")))
(check-expect (piece-shift-left piece-s) (list (make-brick 0 0 "red")
                                               (make-brick 1 0 "red")
                                               (make-brick -1 1 "red")
                                               (make-brick 0 1 "red")))
(define (piece-shift-left p)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (brick-shift-left (first p))
                     (piece-shift-left (rest p)))]))

;; brick-shift-left : Brick -> Brick
;; takes a Brick, and produces a Brick with its x-coordinate substracted by 1
(check-expect (brick-shift-left o1) (make-brick -1 0 "green"))
(check-expect (brick-shift-left i2) (make-brick 0 0 "dark blue"))
(check-expect (brick-shift-left (make-brick 7 14 red)) (make-brick 6 14 red))
(define (brick-shift-left b)
  (make-brick (- (brick-x b) 1)
              (brick-y b)
              (brick-color b)))

;; piece-shift-right : Piece -> Piece
;; takes a Piece, and produces a Piece with each of the input Piece's
;; Brick's x-coordinate added by 1
(check-expect (piece-shift-right piece-o) (list (make-brick 1 0 "green")
                                                (make-brick 2 0 "green")
                                                (make-brick 2 1 "green")
                                                (make-brick 1 1 "green")))
(check-expect (piece-shift-right piece-z) (list (make-brick 1 0 "pink")
                                                (make-brick 2 0 "pink")
                                                (make-brick 2 1 "pink")
                                                (make-brick 3 1 "pink")))
(check-expect (piece-shift-right piece-s) (list (make-brick 2 0 "red")
                                                (make-brick 3 0 "red")
                                                (make-brick 1 1 "red")
                                                (make-brick 2 1 "red")))
(define (piece-shift-right p)
  (cond
    [(empty? p) empty]
    [(cons? p) (cons (brick-shift-right (first p))
                     (piece-shift-right (rest p)))]))

;; brick-shift-right : Brick -> Brick
;; takes a Brick, and produces a Brick with its x-coordinate added by 1
(check-expect (brick-shift-right o1) (make-brick 1 0 "green"))
(check-expect (brick-shift-right i2) (make-brick 2 0 "dark blue"))
(check-expect (brick-shift-right (make-brick 7 14 red)) (make-brick 8 14 red))
(define (brick-shift-right b)
  (make-brick (+ (brick-x b) 1)
              (brick-y b)
              (brick-color b)))

(define (main w)
  (big-bang w
    (to-draw world->scene)
    (on-tick world->world 0.5)
    (on-key key-handler)))

(main world0)


    


