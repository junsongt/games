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
(define DELTA (/ (image-width UNIT) 2))
(define STEP (image-width UNIT))
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
(define H-MOVE STEP) ;moving speed of the shape along x-axis
(define V-MOVE STEP) ;moving speed of the shape along y-axis
(define SCORE-FONT 10)       ;the font size of score
(define SCORE-COLOR "white") ;the color of score

(define COLOR-LIST (list "red" "yellow" "blue" "purple" "orange" "pink"))
(define SHAPE-LIST (list SHAPE1 SHAPE2 SHAPE3 SHAPE4 SHAPE5 SHAPE6 SHAPE7))


;;=========================================================================
;; Data definition
(@htdd Point)
(define-struct point (x y))
;; Point is (make-point Number Number)
;; Number--x coordinate
;; Number--y coordiante

(@htdd ListOfPoint)

(@htdd Block)
(define-struct block (x y points r s c))
;; Block is (make-block Number Number ListOfPoint Integer Integer Integer)
;;; Number--x coordinate
;;; Number--y coordiante
;;; ListOfPoint--
;;; Integer--angle(0 90 180 270 360)
;;; Integer--shape number
;;; Integer--colour number
;(define (fn-for-block b)
;  (... (block-x b)
;       (block-y b)
;       (fn-for-lop (block-points b))
;       (block-r b)
;       (block-s b)       
;       (block-c b)))



(@htdf generate-block)
(@signature Number Number Number Number -> Block)
(define (generate-block x y r s c)
  (cond [(= s 0) (make-block x y
                             (list
                              (make-point x (- y DELTA))
                              (make-point x (+ y DELTA))
                              (make-point (- x (* 2 DELTA)) (+ y DELTA))
                              (make-point (+ x (* 2 DELTA)) (+ y DELTA)))
                             r s c)]
        
        [(= s 1) (make-block x y
                             (list
                              (make-point (- x (* 2 DELTA)) (- y DELTA))
                              (make-point (- x (* 2 DELTA)) (+ y DELTA))
                              (make-point x (+ y DELTA))
                              (make-point (+ x (* 2 DELTA)) (+ y DELTA)))
                             r s c)]
        
        [(= s 2) (make-block x y
                             (list
                              (make-point (+ x (* 2 DELTA)) (- y DELTA))
                              (make-point (- x (* 2 DELTA)) (+ y DELTA))
                              (make-point x (+ y DELTA))
                              (make-point (+ x (* 2 DELTA)) (+ y DELTA)))
                             r s c)]
        
        [(= s 3) (make-block x y
                             (list
                              (make-point (- x (* 3 DELTA)) y)
                              (make-point (- x DELTA) y)
                              (make-point (+ x DELTA) y)
                              (make-point (+ x (* 3 DELTA)) y))
                             r s c)]
        
        [(= s 4) (make-block x y
                             (list
                              (make-point (- x DELTA) (- y DELTA))
                              (make-point (+ x DELTA) (- y DELTA))
                              (make-point (- x DELTA) (+ y DELTA))
                              (make-point (+ x DELTA) (+ y DELTA)))
                             r s c)]
        
        [(= s 5) (make-block x y
                             (list
                              (make-point (- x DELTA) (- y (* 2 DELTA)))
                              (make-point (- x DELTA) y)
                              (make-point (+ x DELTA) y)
                              (make-point (+ x DELTA) (+ y (* 2 DELTA))))
                             r s c)]
        
        [(= s 6) (make-block x y
                             (list
                              (make-point (+ x DELTA) (- y (* 2 DELTA)))
                              (make-point (+ x DELTA) y)
                              (make-point (- x DELTA) y)
                              (make-point (- x DELTA) (+ y (* 2 DELTA))))
                             r s c)]))



(@htdd Game)
(define-struct game (block stack record))
;; Game is (make-game Block ListOfPoint Integer)
;; interp. the current state of a game including:
;; EX:(Initial state)
(define G0 (make-game (generate-block (/ WIDTH 2)
                                      0
                                      0
                                      (random (length SHAPE-LIST))
                                      (random (length COLOR-LIST)))
                      empty 0))

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
          (define rd (game-record g))
          (define pts (block-points blk))
          (define shape (list-ref SHAPE-LIST (block-s blk)))
          
          (define (next-block b)
            (if (or (touch-bottom? b) (reach-stack? b stk))
                (generate-block (/ WIDTH 2)
                                0
                                0
                                (random (length SHAPE-LIST))
                                (random (length COLOR-LIST)))
                (make-block (block-x b)
                            (+ SPEED (block-y b))
                            (map (λ(p) (make-point (point-x p)
                                                   (+ SPEED (point-y p))))
                                 (block-points b))
                            (block-r b) (block-s b) (block-c b))))

          (define (next-stack lop)
            (if (or (touch-bottom? blk) (reach-stack? blk lop))
                (check-full-line (append lop pts))
                lop))

          (define (next-record lop rsf) rsf)]
    
    (if (or (victory? g) (lost? g))
        g
        (make-game (next-block blk)
                   (next-stack stk)
                   (next-record stk rd)))))



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
(@signature Block ListOfPoint -> Boolean)
(define (reach-stack? b lop)
  (ormap (λ(p) (attached? b p)) lop))


;;!!!
;;attached?
(@signature Block Point -> Boolean)
(define (attached? b pt)
  (ormap (λ(p) (and (= (point-x p) (point-x pt))
                    (= (+ DELTA (point-y p)) (point-y pt))))
         (block-points b))) 
                  




;;check-full-line
(@signature ListOfPoint -> ListOfPoint)
(define (check-full-line lop)
  (cond [(not (has-full-line? lop)) lop]
        [else
         (check-full-line (trim-line lop))]))


;;has-full-line?
(@signature ListOfPoint -> Boolean)
(define (has-full-line? lop)
  (local [(define (fn-for-lop lop rsf)
            (cond [(empty? lop) rsf]
                  [else
                   (if (= (point-y (first lop)) BOTTOM)
                       (fn-for-lop (rest lop) (add1 rsf))
                       (fn-for-lop (rest lop) rsf))]))]
    
    (= FULLLINE-NUM (fn-for-lop lop 0))))



;;trim-line
(@signature ListOfPoint -> ListOfPoint)
(define (trim-line lop)
  (local [(define (fn-for-lop lop pwl)
            (cond [(empty? lop) pwl]
                  [else
                   (if (not (= (point-y (first lop)) BOTTOM))
                       (fn-for-lop (rest lop) (cons (first lop) pwl))
                       (fn-for-lop (rest lop) pwl))]))]

    (forward-stack (fn-for-lop lop empty))))

;;forward-stack
(@signature ListOfPoint -> ListOfPoint)
(define (forward-stack lop)
  (map (λ(p) (+ STEP (point-y p))) lop))



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
          (define pts (block-points blk))
          (define angle (block-r blk))
          (define shape (list-ref SHAPE-LIST (block-s blk)))

          
          (define (render-block b img)
            (local [(define r (block-r b))
                    (define s (list-ref SHAPE-LIST (block-s b)))]
              
              (place-image (rotate r s)
                           (block-x b)
                           (block-y b)
                           img)))


          (define (render-stack lop)
            (cond [(empty? lop) MTS]
                  [else
                   (place-image UNIT
                                (point-x (first lop))
                                (point-y (first lop))
                                (render-stack (rest lop)))]))

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
                        (rotate-points b)
                        (+ 90 (block-r b))
                        (block-s b)
                        (block-c b)))

          (define (rotate-points b)
            (local [(define x0 (block-x b))
                    (define y0 (block-y b))]
              (map (λ(p) (make-point (- y0 (point-y p))
                                     (- (point-x p) x0)))
                   (block-points b))))

          (define (move-left b)
            (if (touch-left? b)
                b
                (make-block (- (block-x b) H-MOVE) (block-y b)
                            (map (λ(p) (make-point (- (point-x p) H-MOVE)
                                                   (point-y p)))
                                 (block-points b))
                            (block-r b)
                            (block-s b)
                            (block-c b))))

          (define (move-right b)
            (if (touch-right? b)
                b
                (make-block (+ (block-x b) H-MOVE) (block-y b)
                            (map (λ(p) (make-point (+ (point-x p) H-MOVE)
                                                   (point-y p)))
                                 (block-points b))
                            (block-r b)
                            (block-s b)
                            (block-c b))))

          (define (accelerate b)
            (if (or (reach-stack? b (game-stack g))
                    (touch-bottom? b))
                b 
                (make-block (block-x b) (+ (block-y b) V-MOVE)
                            (map (λ(p) (make-point (point-x p)
                                                   (+ (point-y p) V-MOVE)))
                                 (block-points b))
                            (block-r b)
                            (block-s b)
                            (block-c b))))]

    
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



;;;flip-block
;(@signature Block -> Block)
;(define (flip-block b)
;  (make-block (block-x b)
;              (block-y b)
;              (rotate-points b)
;              (+ 90 (block-r b))
;              (block-s b)
;              (block-c b)))
;
;;;rotate-points
;(@signature Block -> ListOfPoint)
;(define (rotate-points b)
;  (local [(define x0 (block-x b))
;          (define y0 (block-y b))]
;    (map (λ(p) (make-point (- y0 (point-y p))
;                           (- (point-x p) x0)))
;         (block-points b))))
;          
;
;
;;;move-left
;(@signature Block -> Block)
;(define (move-left b)
;  (if (touch-left? b)
;      b
;      (make-block (- (block-x b) H-MOVE) (block-y b)
;                  (map (λ(p) (make-point (- (point-x p) H-MOVE) (point-y p)))
;                       (block-points b))
;                  (block-r b)
;                  (block-s b)
;                  (block-c b))))
;
;
;;;move-right
;(@signature Block -> Block)
;(define (move-right b)
;  (if (touch-right? b)
;      b
;      (make-block (+ (block-x b) H-MOVE) (block-y b)
;                  (map (λ(p) (make-point (+ (point-x p) H-MOVE) (point-y p)))
;                       (block-points b))
;                  (block-r b)
;                  (block-s b)
;                  (block-c b))))
;
;;;accelerate
;(@signature Game -> Block)
;(define (accelerate g)
;  (local [(define b (game-block g))
;          (define stk (game-stack g))]
;    (if (or (reach-stack? b stk) (touch-bottom? b))
;        b 
;        (make-block (block-x b) (+ (block-y b) V-MOVE)
;                    (map (λ(p) (make-point (point-x p) (+ (point-y p) V-MOVE)))
;                         (block-points b))
;                    (block-r b)
;                    (block-s b)
;                    (block-c b)))))