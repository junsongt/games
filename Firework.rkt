;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Firework) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)
(@htdw Firework)

;;============================================
;; Constants
(define WIDTH 500)  ;width of the screen
(define HEIGHT 1000) ;height of the screen
(define MTS (rectangle WIDTH HEIGHT "solid" "black")) ;background(night sky)

;(define DOT (circle 5 "solid" "yellow"))
;(define SPLASH (radial-star 15
;                            (/ (image-width DOT) 2)
;                            (* (image-width DOT) 10) "solid" "yellow"))

;(define LEFT (- 0 (/ (image-width CAT-IMAGE) 2)))         ;left boundary
;(define RIGHT (+ WIDTH (/ (image-width CAT-IMAGE) 2)))    ;right boundary
;(define TOP (+ 0 (/ (image-height CAT-IMAGE) 2)))         ;upper boundary
;(define BOTTOM (- HEIGHT (/ (image-height CAT-IMAGE) 2))) ;lower boundary

(define SPEED 5)   ;pixels per tick for moving spark
(define DISTANCE (/ HEIGHT 2))   ;maximum flying distance
(define R 5) ;radius of the spark
(define COLOR-LIST (list "red" "yellow" "blue" "purple" "orange" "pink"))

;;============================================
;; Data definition
(@htdd Firework)
(define-struct fw (x y c d))
;; Firework is (make-fw Number Number String) with
;; Number--the x coordiante
;; Number--the y coordinate
;; String--the color of the fireworks
;; Number--the flying distance so far
(@dd-template-rules compound)
(define (fn-for-fw f)
  (... (fw-x f)
       (fw-y f)
       (fw-c f)
       (fw-d f)))

(@htdd Firework-list)
;; Firework-list is one of:
;; - empty
;; - (cons Firework Firework-list)
;; interp.the list of different world states(compound) of fireworks
(@dd-template-rules one-of
                    compound
                    self-ref)
(define (fn-for-fwl fwl)
  (cond [(empty? fwl) (...)]
        [else
         (... (fn-for-fw (first fwl))
              (fn-for-fwl (rest fwl)))]))

;;============================================
;; Function definition
;; main function
(@htdf main)
(@signature Firework-list -> Firework-list)
;; start with (main empty)
(@template htdw-main)
(define (main fwl)
  (big-bang fwl                     
    (on-tick   next)      
    (to-draw   render)    
    (on-mouse  shoot)))


;; next function
(@htdf next)
(@signature Firework-list -> Firework-listk)
;; produce the next Firework-list
(@template Firework-list)
(define (next fwl)
  (cond [(empty? fwl) fwl]
        [else
         (if (>= (fw-d (first fwl)) (+ (* 2 SPEED) DISTANCE))
             (next (rest fwl))
             (cons (make-fw (fw-x (first fwl))
                            (- (fw-y (first fwl)) SPEED)
                            (fw-c (first fwl))
                            (+ (fw-d (first fwl)) SPEED))
                   (next (rest fwl))))]))
      

;; render function
(@htdf render)
(@signature Firework-list -> Image)
;; place an appropriate image on the previous scene at pos x and y
(@template Firework-list)
(define (render fwl)
  (cond [(empty? fwl) MTS]
        [else
         (place-image (choose-image (first fwl))
                      (fw-x (first fwl))
                      (fw-y (first fwl))
                      (render (rest fwl)))]))


;; helper function: choose-image
(@htdf choose-image)
(@signature Firework -> Image)
;; produce image of firework according to the pos it has
(@template Firework)
(define (choose-image f)
  (local [(define dot (circle R "solid" (fw-c f)))
          (define splash
            (radial-star 25
                         (/ (image-width dot) 2)
                         (* (image-width dot) 20) "solid" (fw-c f)))]
    (if (>= (fw-d f) DISTANCE)
        splash
        dot)))
        



;; shoot function
(@htdf shoot)
(@signature Firework-list Integer Integer MouseEvent -> Firework-list)
;; adding new firework to firework-list when clicking mouse
(@template MouseEvent Firework)
(define (shoot fwl x y me)
  (cond [(mouse=? me "button-down")
         (cons (make-fw x
                        y
                        (list-ref COLOR-LIST
                                  (random (- (length COLOR-LIST) 1)))
                        0)
               fwl)]
        [else fwl]))
