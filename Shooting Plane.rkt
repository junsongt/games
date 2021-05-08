;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Shooting Plane|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)
(@htdw Game)
;============================================
; Constants
(define WIDTH 500)  ;width of the screen
(define HEIGHT 500) ;height of the screen

(define SHOT (circle 5 "solid" "white"))
(define JET (rectangle 20 10 "solid" "white"))
(define ENEMY (rectangle 20 10 "solid" "red"))
(define MTS (rectangle WIDTH HEIGHT "solid" "black")) ;background
(define MIN-ENEMIES 3)    ;threshold of # enemies to reinforce
(define WINNING-NUM 10)   ;# of enemies shot to win
;(define SPLASH (radial-star 15
;                            (/ (image-width SHOT) 2)
;                            (* (image-width SHOT) 2) "solid" "yellow"))

(define LEFT (+ 0 (/ (image-width JET) 2)))         ;left boundary
(define RIGHT (- WIDTH (/ (image-width JET) 2)))    ;right boundary
;(define TOP (+ 0 (/ (image-height ...) 2)))         ;upper boundary
;(define BOTTOM (- HEIGHT (/ (image-height ...) 2))) ;lower boundary

(define SPEED 5)   ;moving speed of the shot per tick
(define DROP 2)    ;moving speed of the enemy per tick
(define H-MOVE 20) ;moving speed of the jet along x-axis
(define SCORE-FONT 10)       ;the font size of score
(define SCORE-COLOR "white") ;the color of score

;(define COLOR-LIST (list "red" "yellow" "blue" "purple" "orange" "pink"))


;;===============================================================
;; Data definition
(@htdd Shot)
(define-struct shot (x y))
;; Shot is (make-shot Number Number) with
;; Number--the x coordiante
;; Number--the y coordinate
(define (fn-for-shot s)
  (... (shot-x s)
       (shot-y s)))

(@htdd ListOfShot)
;; ListOfShot is one of:
;; - empty
;; - (cons Shot ListOfShot)
;; interp.the list of different world states(compound) of shots
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-shot (first los))
              (fn-for-los (rest los)))]))


(@htdd Enemy)
(define-struct enemy (x y))
;; Enemy is (make-enemy Number Number) with
;; Number--the x coordiante
;; Number--the y coordinate
(define (fn-for-enemy e)
  (... (enemy-x e)
       (enemy-y e)))

(@htdd ListOfEnemy)
;; ListOfEnemy is one of:
;; - empty
;; - (cons Enemy ListOfEnemy)
;; interp.the list of different world states(compound) of enemies
(define (fn-for-loe loe)
  (cond [(empty? loe) (...)]
        [else
         (... (fn-for-enemy (first loe))
              (fn-for-loe (rest loe)))]))




(@htdd Game)
(define-struct game (shots enemies jet record))
;; Game is (make-game ListOfShot ListOfEnemy Number Integer)
;; interp. the current state of a game including:
;;         coordinates of the shots
;;         coordinates of the enemies
;;         x-position of jet
;;         # of enemies been shot
;; EX:(Initial state)
(define G0 (make-game empty (list (make-enemy (random WIDTH) 0)) (/ WIDTH 2) 0))

(define (fn-for-game g)
  (... (fn-for-los (game-shots g))
       (fn-for-loe (game-enemies g))
       (game-jet g)
       (fn-for-record (game-record g))))


;;===============================================================
;; Function definition
;; Main function
(@htdf main)
(@signature Game -> Game)
;; start with (main G0)
(@template htdw-main)
(define (main g)
  (big-bang g                   
    (on-tick next)      
    (to-draw render)    
    (on-key attack)
    ;(stop-when last-world? last-scene)
    ))


;; Next function
(@htdf next)
(@signature Game -> Game)
;; produce the next state of game
(@template Game fn-composition encapsulated)
(define (next g)
  (local [(define (next-shots los)
            (cond [(empty? los) los]
                  [else
                   (if (or (ormap (λ (e) (hit? (first los) e)) (game-enemies g))
                           (<= (shot-y (first los)) (- (/ (image-height SHOT) 2))))
                       (next-shots (rest los))
                       (cons (make-shot (shot-x (first los))
                                        (- (shot-y (first los)) SPEED))
                             (next-shots (rest los))))]))

          
          (define (next-enemies loe)
            (local [(define new-enemy (make-enemy (random WIDTH) 0))]
              (if (<= (length loe) MIN-ENEMIES)
                  (forward-enemies (append loe (list new-enemy)))
                  (forward-enemies loe))))

          (define (forward-enemies loe)           
            (cond [(empty? loe) loe]
                  [else
                   (if (ormap (λ (s) (hit? s (first loe))) (game-shots g))
                       (forward-enemies (rest loe))
                       (cons (make-enemy (enemy-x (first loe))
                                         (+ (enemy-y (first loe)) DROP))
                             (forward-enemies (rest loe))))]))

          
          (define (next-record loe rsf)
            (cond [(empty? loe) rsf]
                  [else
                   (if (ormap (λ (s) (hit? s (first loe))) (game-shots g))
                       (next-record (rest loe) (add1 rsf))
                       (next-record (rest loe) rsf))]))]

    (if (or (victory? g) (conquer? g))
        g
        (make-game (next-shots (game-shots g))
                   (next-enemies (game-enemies g))
                   (game-jet g)
                   (next-record (game-enemies g) (game-record g))))))






;; Render function
(@htdf render)
(@signature Game -> Image)
;; place an appropriate image on the previous scene at pos x and y
(@template Game fn-composition encapsulated)
(define (render g)
  (local [(define (render-shots los img)
            (cond [(empty? los) img]
                  [else
                   (place-image SHOT
                                (shot-x (first los))
                                (shot-y (first los))
                                (render-shots (rest los) img))]))
          
          (define (render-enemies loe)
            (cond [(empty? loe) MTS]
                  [else
                   (place-image ENEMY
                                (enemy-x (first loe))
                                (enemy-y (first loe))
                                (render-enemies (rest loe)))]))
          
          (define score-bd
            (text (string-append "score: " (number->string (game-record g)))
                  SCORE-FONT
                  SCORE-COLOR))

          (define main-image
            (place-image score-bd
                         (image-width score-bd)
                         (image-height score-bd)
                         (place-image JET
                                      (game-jet g)
                                      (- HEIGHT (/ (image-height JET) 2))
                                      (render-shots (game-shots g)
                                                    (render-enemies (game-enemies g))))))]
    

    (cond [(victory? g) (overlay (above (text "VICTORY!" 50 "white")
                                        (text "Press R to restart" 20 "white"))
                                 main-image)]
          [(conquer? g) (overlay (above (text "GAME OVER" 50 "white")
                                        (text "Press R to restart" 20 "white"))
                                 main-image)]
          [else main-image])))




;; Helpers
(@htdf hit?)
(@signature Shot Enemy -> Boolean)
(define (hit? s e)
  (and (<= (- (enemy-y e) (/ (image-height ENEMY) 2))
           (- (shot-y s) (/ (image-height SHOT) 2))
           (+ (enemy-y e) (/ (image-height ENEMY) 2)))
       (<= (- (- (enemy-x e) (/ (image-width ENEMY) 2)) (/ (image-width SHOT) 2))
           (shot-x s)
           (+ (+ (enemy-x e) (/ (image-width ENEMY) 2)) (/ (image-width SHOT) 2)))))


(@htdf victory?)
(@signature Game -> Boolean)
(define (victory? g)
  (>= (game-record g) WINNING-NUM))


(@htdf conquer?)
(@signature Game -> Boolean)
(define (conquer? g)
  (>= (enemy-y (first (game-enemies g)))
      (- HEIGHT (/ (image-height ENEMY) 2))))





;;;Functions for stop-when (one-time game)
;(@htdf last-world?)
;(@signature Game -> Boolean)
;;; whether the game is won or lost
;(define (last-world? g)
;  (or (victory? g) (conquer? g)))
;
;(@htdf last-scene)
;(@signature Game -> Image)
;;; produce the last image of wining or losing
;(define (last-scene g)
;  (if (conquer? g)
;      (overlay (text "GAME OVER" 50 "white") (render g))
;      (overlay (text "VICTORY!" 50 "white") (render g))))




;; Attack function
(@htdf attack)
(@signature Game KeyEvent -> Game)
;; shoot or move left & right when pressing corresponding key
(@template KeyEvent Game)
(define (attack g ke)
  (cond [(key=? ke " ")
         (make-game (cons (make-shot (game-jet g) (- HEIGHT (image-height JET)))
                          (game-shots g))
                    (game-enemies g)
                    (game-jet g)
                    (game-record g))]
        [(key=? ke "left")
         (if (<= (game-jet g) LEFT)
             (make-game (game-shots g)
                        (game-enemies g)
                        LEFT
                        (game-record g))
             (make-game (game-shots g)
                        (game-enemies g)
                        (- (game-jet g) H-MOVE)
                        (game-record g)))]
        [(key=? ke "right")
         (if (>= (game-jet g) RIGHT)
             (make-game (game-shots g)
                        (game-enemies g)
                        RIGHT
                        (game-record g))
             (make-game (game-shots g)
                        (game-enemies g)
                        (+ (game-jet g) H-MOVE)
                        (game-record g)))]
        [(key=? ke "r") G0]
        [else g]))
