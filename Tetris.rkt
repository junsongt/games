;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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
(define MIN-ENEMIES 3)    ;threshold of # enemies to reinforce
(define WINNING-NUM 10)   ;# of enemies shot to win


(define LEFT (+ 0 DELTA))         ;left boundary
(define RIGHT (- WIDTH DELTA))    ;right boundary
(define TOP (+ 0 DELTA))          ;upper boundary
(define BOTTOM (- HEIGHT DELTA))  ;lower boundary

(define SPEED 2)   ;dropping speed of the shape per tick
(define H-MOVE CELL) ;moving speed of the shape along x-axis
(define V-MOVE CELL) ;moving speed of the shape along y-axis
(define SCORE-FONT 10)       ;the font size of score
(define SCORE-COLOR "white") ;the color of score

(define COLOR-LIST (list "red" "yellow" "blue" "purple" "orange" "pink"))
(define SHAPE-LIST (list SHAPE1 SHAPE2 SHAPE3 SHAPE4 SHAPE5 SHAPE6 SHAPE7))


;;=========================================================================
;; Data definition
(@htdd Cell)
(define-struct cell (x y))
;; Cell is (make-cell Number Number Boolean)
;; Number--x coordinate
;; Number--y coordiante
;; Boolean--true if cell is taken

(@htdd ListOfCell)

(@htdd Block)
(define-struct block (x y cells r s c))
;; Block is (make-block Number Number ListOfCell Integer Integer Integer)
;;; Number--central-x coordinate
;;; Number--central-y coordiante
;;; ListOfCell--
;;; Integer--angle(0 90 180 270 360)
;;; Integer--shape number
;;; Integer--colour number
;(define (fn-for-block b)
;  (... (block-x b)
;       (block-y b)
;       (fn-for-loc (block-cells b))
;       (block-r b)
;       (block-s b)       
;       (block-c b)))



(@htdf generate-block)
(@signature Number Number Number Number -> Block)
(define (generate-block x y r s c)
  (cond [(= s 0) (make-block (+ x CELL) (- y DELTA)
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x (* 2 CELL)) y)
                              (make-cell (+ x CELL) (- y CELL)))
                             r s c)]
        
        [(= s 1) (make-block (+ x CELL) (- y DELTA)
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x (* 2 CELL)) y)
                              (make-cell x (- y CELL)))
                             r s c)]
        
        [(= s 2) (make-block (+ x CELL) (- y DELTA)
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x (* 2 CELL)) y)
                              (make-cell (+ x (* 2 CELL)) (- y CELL)))
                             r s c)]
        
        [(= s 3) (make-block (+ x CELL DELTA) y
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x (* 2 CELL)) y)
                              (make-cell (+ x (* 3 CELL)) y))
                             r s c)]
        
        [(= s 4) (make-block (+ x DELTA) (- y DELTA)
                             (list
                              (make-cell x y)
                              (make-cell (+ x CELL) y)
                              (make-cell (+ x CELL) (- y CELL))
                              (make-cell x (- y CELL)))
                             r s c)]
        
        [(= s 5) (make-block (- x DELTA) (- y CELL)
                             (list
                              (make-cell x y)
                              (make-cell x (- y CELL))
                              (make-cell (- x CELL) (- y CELL))
                              (make-cell (- x CELL) (- y (* 2 CELL))))
                             r s c)]
        
        [(= s 6) (make-block (+ x DELTA) (- y CELL)
                             (list
                              (make-cell x y)
                              (make-cell x (- y CELL))
                              (make-cell (+ x CELL) (- y CELL))
                              (make-cell (+ x CELL) (- y (* 2 CELL))))
                             r s c)]))



(@htdd Game)
(define-struct game (block stack record))
;; Game is (make-game Block ListOfCell Integer)
;; interp. the current state of a game including:
;; EX:(Initial state)
(define G0 (make-game (generate-block (+ (+ CELL LEFT) (* CELL (random (- FULLLINE-NUM 4))))
                                      TOP
                                      0
                                      (random (length SHAPE-LIST))
                                      (random (length COLOR-LIST)))
                      empty
                      0))

(define (fn-for-game g)
  (... (fn-for-block (game-block g))
       (fn-for-lob (game-stack g))
       (fn-for-record (game-record g))))




;;===============================================================
;; Function definition
;; Main function
(@htdf main)
(@signature Game -> Game)
;; start with (main G0)
(define (main g)
  (big-bang g                   
    (on-tick next)      
    (to-draw render)    
    (on-key move)))



;; Next function
(@htdf next)
(@signature Game -> Game)
;; produce the next state of game
(define (next g)
  (local [(define blk (game-block g))
          (define stk (game-stack g))
          
          (define (next-block b loc)
            (local [(define new-block
                      (make-block (block-x b) (+ SPEED (block-y b))
                                  (map (λ(c) (make-cell (cell-x c) (+ SPEED (cell-y c))))
                                       (block-cells b))
                                  (block-r b) (block-s b) (block-c b)))]
              
              (if (or (touch-bottom? b) (reach-stack? b loc))
                  (generate-block (+ (+ CELL LEFT) (* CELL (random (- FULLLINE-NUM 4))))
                                  TOP
                                  0
                                  (random (length SHAPE-LIST))
                                  (random (length COLOR-LIST)))
                  (cond [(touch-bottom? new-block) (stay-bottom b)]
                        [(reach-stack? new-block loc) (stay-stack b loc)]
                        [else new-block]))))
          
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




;;!!!
;;reach-stack?
(@signature Block ListOfCell -> Boolean)
(define (reach-stack? b loc)
;  (ormap (λ(p) (ormap (λ(c) (attached? p c)) loc)) (block-cells b)))
  (ormap (λ(p) (ormap (λ(c) (and (= (cell-x c) (cell-x p))
                                 (<= 0 (- (cell-y c) (cell-y p)) CELL)))
                      loc))
         (block-cells b)))
  
;(define (reach-stack? b loc)
;  (ormap (λ(c) (member? c loc)) (block-cells (forward-block b))))

;;attached?
(@signature Cell Cell -> Boolean)
(define (attached? cb cs)
  ;; usually c1 is above(from block) and c2 is below(from stack)
  (or (and (= (cell-x cb) (cell-x cs)) (<= 0 (- (cell-y cs) (cell-y cb)) CELL))
      (and (= (cell-y cb) (cell-y cs)) (<= (abs (- (cell-x cs) (cell-x cb))) CELL))))
  
                  
;;forward-block
(@signature Block -> Block)
(define (forward-block b)
  (make-block (block-x b)
              (+ CELL (block-y b))
              (map (λ(c) (make-cell (cell-x c) (+ CELL (cell-y c)))) (block-cells b))
              (block-r b) (block-s b) (block-c b)))

;;stay-bottom
(@signature Block -> Block)
(define (stay-bottom b)
  (local [(define shape (list-ref SHAPE-LIST (block-s b)))
          (define c-y (block-y b))
          (define half-width (/ (image-width shape) 2))
          (define half-height (/ (image-height shape) 2))
          (define (fn-for-block s)
            (make-block (block-x b) (+ s (block-y b))
                        (map (λ(c) (make-cell (cell-x c) (+ s (cell-y c)))) (block-cells b))
                        (block-r b) (block-s b) (block-c b)))]
    (cond [(horizontal? b)
           (local [(define step1 (- HEIGHT c-y half-height))]
             (fn-for-block step1))]
          [else
           (local [(define step2 (- HEIGHT c-y half-width))]
             (fn-for-block step2))])))



;;stay-stack
;;nested loops(hard to apply)
(@signature Block ListOfCell -> Block)
(define (stay-stack b0 loc0)
  (local [(define (fn-for-loc c loc wl)
            (cond [(empty? loc) wl]
                  [else
                   (local [(define distance (- (cell-y (first loc)) (cell-y c) CELL))]
                     (if (and (= (cell-x (first loc)) (cell-x c)) (>= distance 0))
                         (fn-for-loc c (rest loc) (cons distance wl))
                         (fn-for-loc c (rest loc) wl)))]))

          (define (fn-for-block lop loc wl)
            (cond [(empty? lop) wl]
                  [else
                   (append (fn-for-loc (first lop) loc wl)
                           (fn-for-block (rest lop) loc wl))]))

          (define step
            (local [(define dl (fn-for-block (block-cells b0) loc0 empty))]
              (foldr min (first dl) (rest dl))))]
    
    (make-block (block-x b0) (+ step (block-y b0))
                (map (λ(c) (make-cell (cell-x c) (+ step (cell-y c)))) (block-cells b0))
                (block-r b0) (block-s b0) (block-c b0))))
            
                   


;;!!!
;;check-full-line
(@signature ListOfCell -> ListOfCell)
(define (check-full-line loc)
  (cond [(not (has-full-line? loc)) loc]
        [else
         (check-full-line (trim-line loc))]))

;;!!!
;;has-full-line?
(@signature ListOfCell -> Boolean)
(define (has-full-line? loc)
  (local [(define (fn-for-loc loc rsf)
            (cond [(empty? loc) rsf]
                  [else
                   (if (= (cell-y (first loc)) BOTTOM)
                       (fn-for-loc (rest loc) (add1 rsf))
                       (fn-for-loc (rest loc) rsf))]))]    
    (= FULLLINE-NUM (fn-for-loc loc 0))))


;;!!!
;;trim-line
(@signature ListOfCell -> ListOfCell)
(define (trim-line loc)
  (local [(define (fn-for-loc loc pwl)
            (cond [(empty? loc) pwl]
                  [else
                   (if (not (= (cell-y (first loc)) BOTTOM))
                       (fn-for-loc (rest loc) (cons (first loc) pwl))
                       (fn-for-loc (rest loc) pwl))]))]
    (forward-stack (fn-for-loc loc empty))))


;;forward-stack
(@signature ListOfCell -> ListOfCell)
(define (forward-stack loc)
  (map (λ(c) (make-cell (cell-x c) (+ CELL (cell-y c)))) loc))



;;!!!
(@signature Game -> Boolean)
(define (victory? g) false)

;;!!!
(@signature Game -> Boolean)
(define (lost? g) false)





;; Render function
(@htdf render)
(@signature Game -> Image)
;; place an appropriate image on the previous scene at pos x and y
(define (render g)
  (local [(define blk (game-block g))
          (define stk (game-stack g))
          (define rd (game-record g))
          (define pts (block-cells blk))
          (define angle (block-r blk))
          (define shape (list-ref SHAPE-LIST (block-s blk)))

          
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








;; Move function
(@htdf move)
(@signature Game KeyEvent -> Game)
;; shoot or move left & right when pressing corresponding key
(define (move g ke)
  (local [(define (flip-block b)
            (make-block (block-x b)
                        (block-y b)
                        (rotate-cells b)
                        (+ 90 (block-r b))
                        (block-s b)
                        (block-c b)))

          (define (rotate-cells b)
            (local [(define x0 (block-x b))
                    (define y0 (block-y b))]
              (map (λ(p) (make-cell (- y0 (cell-y p))
                                    (- (cell-x p) x0)))
                   (block-cells b))))



          

          (define (move-left b)
            (if (touch-left? b)
                b
                (make-block (- (block-x b) H-MOVE) (block-y b)
                            (map (λ(p) (make-cell (- (cell-x p) H-MOVE)
                                                  (cell-y p)))
                                 (block-cells b))
                            (block-r b)
                            (block-s b)
                            (block-c b))))

          (define (move-right b)
            (if (touch-right? b)
                b
                (make-block (+ (block-x b) H-MOVE) (block-y b)
                            (map (λ(p) (make-cell (+ (cell-x p) H-MOVE)
                                                  (cell-y p)))
                                 (block-cells b))
                            (block-r b)
                            (block-s b)
                            (block-c b))))

          (define (accelerate b)
            (local [(define new-block (make-block (block-x b) (+ (block-y b) V-MOVE)
                                                  (map (λ(p) (make-cell (cell-x p)
                                                                        (+ (cell-y p) V-MOVE)))
                                                       (block-cells b))
                                                  (block-r b) (block-s b) (block-c b)))]
              (if (or (reach-stack? b (game-stack g))
                      (touch-bottom? b))
                  b
                  (cond [(touch-bottom? new-block) (stay-bottom b)]
                        [(reach-stack? new-block (game-stack g)) (stay-stack b (game-stack g))]
                        [else new-block]))))]

    
    (cond [(key=? ke " ")
           (make-game (flip-block (game-block g)) (game-stack g) (game-record g))]
          [(key=? ke "left")
           (make-game (move-left (game-block g)) (game-stack g) (game-record g))]
          [(key=? ke "right")
           (make-game (move-right (game-block g)) (game-stack g) (game-record g))]
          [(key=? ke "down")
           (make-game (accelerate (game-block g)) (game-stack g) (game-record g))]
          [(key=? ke "r") G0]
          [else g])))


