;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Tetris(Lite BW)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)
(@htdw Game)
;==========================================================================
; Constants
(define WIDTH 200)  ;width of the screen
(define HEIGHT 400) ;height of the screen


(define UNIT (square 20 "solid" "white"))
(define FULLLINE-NUM (/ WIDTH (image-width UNIT)))
(define CELL (image-width UNIT))
(define DELTA (/ CELL 2))
(define SHAPE1 (above UNIT (beside UNIT UNIT UNIT)))
(define SHAPE2 (above/align "left" UNIT (beside UNIT UNIT UNIT)))
(define SHAPE3 (above/align "right" UNIT (beside UNIT UNIT UNIT)))
(define SHAPE4 (beside UNIT UNIT UNIT UNIT))
(define SHAPE5 (above (beside UNIT UNIT) (beside UNIT UNIT)))
(define SHAPE6 (above/align "right"
                            (above/align "left" UNIT (beside UNIT UNIT))
                            UNIT))

(define SHAPE7 (above/align "left"
                            (above/align "right" UNIT (beside UNIT UNIT))
                            UNIT))


(define MTS (rectangle WIDTH HEIGHT "solid" "black")) ;background

(define LEFT (+ 0 DELTA))         ;left boundary
(define RIGHT (- WIDTH DELTA))    ;right boundary
(define TOP (+ 0 DELTA))          ;upper boundary
(define BOTTOM (- HEIGHT DELTA))  ;lower boundary

(define SPEED CELL)   ;dropping speed of the shape per tick
(define H-MOVE CELL) ;moving speed of the shape along x-axis
(define V-MOVE CELL) ;moving speed of the shape along y-axis
(define SCORE-FONT 10)       ;the font size of score
(define SCORE-COLOR "white") ;the color of score

(define SHAPE-LIST (list SHAPE1 SHAPE2 SHAPE3 SHAPE4 SHAPE5 SHAPE6 SHAPE7))


;;=========================================================================
;; Data definition
(@htdd Cell)
(define-struct cell (x y))
;; Cell is (make-cell Number Number)
;; Number--x coordinate
;; Number--y coordiante
(define (fn-for-cell c)
  (... (cell-x c)
       (cell-y c)))

(@htdd ListOfCell)
;; ListOfCell is one of:
;; - empty
;; - (cons Cell ListOfCell)
;; interp. list of cells' coordinates in grid
(define (fn-for-loc loc)
  (cond [(empty? loc) ...]
        [else
         (... (fn-for-cell (first loc))
              (fn-for-loc (rest loc)))]))


(@htdd Block)
(define-struct block (x y cells rcx rcy r s))
; Block is (make-block Number Number ListOfCell Integer Integer Integer)
;; Number--central-x coordinate
;; Number--central-y coordiante
;; ListOfCell--the 4 cells within the block body
;; Number--x coord for block rotation center
;; Number--y coord for block rotation center
;; Integer--angle(0 90 180 270 360)
;; Integer--shape number
(define (fn-for-block b)
  (... (block-x b)
       (block-y b)
       (fn-for-loc (block-cells b))
       (block-rcx b)
       (block-rcy b)
       (block-r b)
       (block-s b)))



(@htdf generate-block)
(@signature Number Number Number Integer -> Block)
(define (generate-block x y r s)
  (cond [(= s 0) (make-block (+ x CELL) (- y DELTA)
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x (* 2 CELL)) y)
                              (make-cell (+ x CELL) (- y CELL)))
                             (+ x CELL) y
                             r s)]
        
        [(= s 1) (make-block (+ x CELL) (- y DELTA)
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x (* 2 CELL)) y)
                              (make-cell x (- y CELL)))
                             (+ x CELL) (- y CELL)
                             r s)]
        
        [(= s 2) (make-block (+ x CELL) (- y DELTA)
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x (* 2 CELL)) y)
                              (make-cell (+ x (* 2 CELL)) (- y CELL)))
                             (+ x CELL) (- y CELL)
                             r s)]
        
        [(= s 3) (make-block (+ x CELL DELTA) y
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x (* 2 CELL)) y)
                              (make-cell (+ x (* 3 CELL)) y))
                             (+ x CELL DELTA) (+ y CELL DELTA)
                             r s)]
        
        [(= s 4) (make-block (+ x DELTA) (- y DELTA)
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x CELL) (- y CELL))
                              (make-cell x (- y CELL)))
                             (+ x DELTA) (- y DELTA)
                             r s)]
        
        [(= s 5) (make-block (- x DELTA) (- y CELL)
                             (list
                              (make-cell x y)
                              (make-cell x (- y CELL))
                              (make-cell (- x CELL) (- y CELL))
                              (make-cell (- x CELL) (- y (* 2 CELL))))
                             (- x CELL) (- y CELL)
                             r s)]
        
        [(= s 6) (make-block (+ x DELTA) (- y CELL)
                             (list
                              (make-cell x y)
                              (make-cell x (- y CELL))
                              (make-cell (+ x CELL) (- y CELL))
                              (make-cell (+ x CELL) (- y (* 2 CELL))))
                             x (- y CELL)
                             r s)]))



(@htdd Game)
(define-struct game (block stack record))
;; Game is (make-game Block ListOfCell Integer)
;; interp. the current state of a game including:
;; EX:(Initial state)
(define G0 (make-game (generate-block (+ (+ CELL LEFT) (* CELL (random (- FULLLINE-NUM 4))))
                                      TOP
                                      0
                                      (random (length SHAPE-LIST)))
                      empty
                      0))

(define (fn-for-game g)
  (... (fn-for-block (game-block g))
       (fn-for-lob (game-stack g))
       (fn-for-record (game-record g))))




;;=======================================================================
;; Function definition
;; Main function
(@htdf main)
(@signature Game -> Game)
;; start with (main G0)
(define (main g)
  (big-bang g                   
    (on-tick next 1)      
    (to-draw render)    
    (on-key move)))


;;==================================
;; Next function
(@htdf next)
(@signature Game -> Game)
;; produce the next state of game
(define (next g)
  (local [(define blk (game-block g))
          (define stk (game-stack g))
          
          (define (next-block b loc)
            (if (or (touch-bottom? b) (reach-stack? b loc))
                (generate-block (+ (+ CELL LEFT) (* CELL (random (- FULLLINE-NUM 4))))
                                TOP
                                0
                                (random (length SHAPE-LIST)))
                (make-block (block-x b) (+ SPEED (block-y b))
                            (map (λ(c) (make-cell (cell-x c) (+ SPEED (cell-y c))))
                                 (block-cells b))
                            (block-rcx b) (+ SPEED (block-rcy b))
                            (block-r b) (block-s b))))
          
          (define (next-stack b loc)
            (if (or (touch-bottom? b) (reach-stack? b loc))
                (check-full-line (append loc (block-cells b)))
                loc))

          (define (next-record loc rsf) rsf)]
    
    (if (or (victory? g) (lost? g))
        g
        (make-game (next-block (game-block g) (game-stack g))
                   (next-stack (game-block g) (game-stack g))
                   (next-record (game-stack g) (game-record g))))))



;;Helpers
;;horizontal?
(@signature Block -> Boolean)
(define (horizontal? b)
  (local [(define angle (block-r b))]
    (= (modulo (/ angle 90) 2) 0)))

;;touch-bottom?
(@signature Block -> Boolean)
(define (touch-bottom? b)
  (local [(define shape (list-ref SHAPE-LIST (block-s b)))]
    (cond [(horizontal? b)
           (>= (block-y b) (- HEIGHT (/ (image-height shape) 2)))]
          [else
           (>= (block-y b) (- HEIGHT (/ (image-width shape) 2)))])))

;;touch-top?
(@signature Block -> Boolean)
(define (touch-top? b)
  (local [(define shape (list-ref SHAPE-LIST (block-s b)))]
    (cond [(horizontal? b)
           (<= (block-y b) (+ 0 (/ (image-height shape) 2)))]
          [else
           (<= (block-y b) (+ 0 (/ (image-width shape) 2)))])))

;;touch-left?
(@signature Block -> Boolean)
(define (touch-left? b)
  (local [(define shape (list-ref SHAPE-LIST (block-s b)))]
    (cond [(horizontal? b)
           (<= (block-x b) (+ 0 (/ (image-width shape) 2)))]
          [else
           (<= (block-x b) (+ 0 (/ (image-height shape) 2)))])))

;;touch-right?
(@signature Block -> Boolean)
(define (touch-right? b)
  (local [(define shape (list-ref SHAPE-LIST (block-s b)))]
    (cond [(horizontal? b)
           (>= (block-x b) (- WIDTH (/ (image-width shape) 2)))]
          [else
           (>= (block-x b) (- WIDTH (/ (image-height shape) 2)))])))

;;touch-cell?
(@signature Block ListOfCell -> Boolean)
(define (touch-cell? b loc)
  (ormap (λ(p) (ormap (λ(c) (and (< (abs (- (cell-y c) (cell-y p))) CELL)
                                 (<= (abs (- (cell-x c) (cell-x p))) CELL)))
                      loc))
         (block-cells b)))


;;reach-stack?
(@signature Block ListOfCell -> Boolean) 
(define (reach-stack? b loc)
  (ormap (λ(c) (member? c loc)) (block-cells (forward-block b))))
  
                  
;;forward-block
(@signature Block -> Block)
(define (forward-block b)
  (make-block (block-x b) (+ CELL (block-y b))
              (map (λ(c) (make-cell (cell-x c) (+ CELL (cell-y c)))) (block-cells b))
              (block-rcx b) (+ CELL (block-rcy b))
              (block-r b) (block-s b)))

                  

;;check-full-line
(@signature ListOfCell -> ListOfCell)
(check-expect
 (check-full-line (list (make-cell 10 40) (make-cell 20 40)
                        (make-cell 10 60) (make-cell 20 60) (make-cell 30 60) (make-cell 40 60) (make-cell 50 60)
                        (make-cell 60 60) (make-cell 70 60) (make-cell 80 60) (make-cell 90 60) (make-cell 100 60)
                        (make-cell 10 80) (make-cell 20 80) (make-cell 30 80) (make-cell 40 80) (make-cell 50 80)
                        (make-cell 60 80) (make-cell 70 80) (make-cell 80 80) (make-cell 90 80) (make-cell 100 80)
                        (make-cell 10 100) (make-cell 20 100) (make-cell 50 100) (make-cell 90 100)))
 (list
  (make-cell 10 80)
  (make-cell 20 80)
  (make-cell 10 100)
  (make-cell 20 100)
  (make-cell 50 100)
  (make-cell 90 100)))

(define (check-full-line loc0)
  (local [(define ylist (sort (map (λ(c) (cell-y c)) loc0) >))

          (define (fn-for-ylist yl loc)
            (cond [(empty? yl) loc]
                  [else
                   (if (has-full-line-at? loc (first yl))
                       (fn-for-ylist yl (remove-line loc (first yl)))
                       (fn-for-ylist (rest yl) loc))]))]

    (fn-for-ylist ylist loc0)))


;;cells-above-at
(@signature ListOfCell Number -> ListOfCell)
(define (cells-above-at loc ylevel)
  (filter (λ(c) (<= (cell-y c) ylevel)) loc))

;;has-full-line-at?
(@signature ListOfCell Number -> Boolean)
(define (has-full-line-at? loc ylevel)
  (= (length (filter (λ(c) (= ylevel (cell-y c))) loc)) FULLLINE-NUM))


;;remove-line
(@signature ListOfCell Number -> ListOfCell)
(define (remove-line loc ylevel)
  (local [(define cells-above (filter (λ(c) (< (cell-y c) ylevel)) loc))
          (define cells-below (filter (λ(c) (> (cell-y c) ylevel)) loc))]
    (append (drop-stack cells-above) cells-below)))


;;drop-stack
(@signature ListOfCell -> ListOfCell)
(define (drop-stack loc)
  (map (λ(c) (make-cell (cell-x c) (+ CELL (cell-y c)))) loc))



;;======================================

;;!!!
(@signature Game -> Boolean)
(define (victory? g) false)

;;!!!
(@signature Game -> Boolean)
(define (lost? g) false)



;;======================================
;; Render function
(@htdf render)
(@signature Game -> Image)
;; place an appropriate image on the previous scene at pos x and y
(define (render g)
  (local [(define blk (game-block g))
          (define stk (game-stack g))
          (define rd (game-record g))

          
          (define (render-block b img)
            (local [(define r (block-r b))
                    (define s (list-ref SHAPE-LIST (block-s b)))]              
              (place-image (rotate r s)
                           (block-x b)
                           (block-y b)
                           img)))

          (define (render-stack loc)
            (cond [(empty? loc) MTS]
                  [else
                   (place-image UNIT
                                (cell-x (first loc))
                                (cell-y (first loc))
                                (render-stack (rest loc)))]))

          (define score-bd
            (text (string-append "score: " (number->string rd))
                  SCORE-FONT
                  SCORE-COLOR))

          (define main-image
            (place-image score-bd
                         (image-width score-bd)
                         (image-height score-bd)
                         (render-block blk (render-stack stk))))]

    (cond [(victory? g) (overlay (above (text "VICTORY!" 50 "white")
                                        (text "Press R to restart" 20 "white"))
                                 main-image)]
          [(lost? g) (overlay (above (text "GAME OVER" 50 "white")
                                     (text "Press R to restart" 20 "white"))
                              main-image)]
          [else main-image])))






;;==================================
;; Move function
(@htdf move)
(@signature Game KeyEvent -> Game)
;; rotate block or move left & right when pressing corresponding key
(define (move g ke)
  (cond [(key=? ke " ")
           (make-game (flip-block (game-block g)) (game-stack g) (game-record g))]
          [(key=? ke "left")
           (make-game (move-left (game-block g) (game-stack g)) (game-stack g) (game-record g))]
          [(key=? ke "right")
           (make-game (move-right (game-block g) (game-stack g)) (game-stack g) (game-record g))]
          [(key=? ke "down")
           (make-game (accelerate (game-block g) (game-stack g)) (game-stack g) (game-record g))]
          [(key=? ke "r") G0]
          [else g]))




;;flip-block
;;Note: (+ π/2) in world system is (-π/2) in Cartesian system;
;;formula: (x, y) is original coord, (x', y') is coord after rotation;
;;         v = [x-rcx, y-rcy]; v' = [x'-rcx, y'-rcy] = [y-rcy, rcx-x]
;;         => x' = rcx-rxy+y; y' = rcx+rcy-x
(@signature Block -> Block)
(define (flip-block b)
  (make-block (+ (- (block-rcx b) (block-rcy b)) (block-y b))
              (- (+ (block-rcx b) (block-rcy b)) (block-x b))
              (map (λ(c) (make-cell (+ (- (block-rcx b) (block-rcy b)) (cell-y c))
                                    (- (+ (block-rcx b) (block-rcy b)) (cell-x c))))
                   (block-cells b))
              (block-rcx b) (block-rcy b)
              (+ 90 (block-r b)) (block-s b)))


;;move-left
(@signature Block ListOfCell -> Block)
(define (move-left b loc)
  (if (or (touch-left? b) (touch-cell? b loc))
      b
      (make-block (- (block-x b) H-MOVE) (block-y b)
                  (map (λ(p) (make-cell (- (cell-x p) H-MOVE) (cell-y p)))
                       (block-cells b))
                  (- (block-rcx b) H-MOVE) (block-rcy b)
                  (block-r b) (block-s b))))

;;move-right
(@signature Block ListOfCell -> Block)
(define (move-right b loc)
  (if (or (touch-right? b) (touch-cell? b loc))
      b
      (make-block (+ (block-x b) H-MOVE) (block-y b)
                  (map (λ(p) (make-cell (+ (cell-x p) H-MOVE) (cell-y p)))
                       (block-cells b))
                  (+ (block-rcx b) H-MOVE) (block-rcy b)
                  (block-r b) (block-s b))))

;;accelerate
(@signature Block ListOfCell -> Block)
(define (accelerate b loc)
            (if (or (reach-stack? b loc)
                    (touch-bottom? b))
                b
                (make-block (block-x b) (+ (block-y b) V-MOVE)
                            (map (λ(p) (make-cell (cell-x p) (+ (cell-y p) V-MOVE)))
                                 (block-cells b))
                            (block-rcx b) (+ V-MOVE (block-rcy b))
                            (block-r b) (block-s b))))