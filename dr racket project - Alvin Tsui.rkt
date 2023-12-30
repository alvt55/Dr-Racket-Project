;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |dr racket project - Alvin Tsui|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/universe)
(require 2htdp/image)
(require spd/tags)


(@htdw Game)
;; project created after taking CPSC 110 at UBC, expanded off of a problem set
;; Changes made:
;; 1. Compound definition for character - can move in all directions
;; 2. Improvements to code - using abstract functions to simplify
;; 3. Win and lose condition - game resets if player hits ball
;;                             WINSCREEN appears if player reaches bottom corner
;; 4. Player bounds - player cannot go out of bounds, prevents a "lost" player
;; 5. Small changes - ex. greater speed range using random func for balls
;; 6. Defaults - after hitting a ball, game resets to G1
;;               which means the addition of more constants and cond conditions
;;               can make the game easier for the user if they fail 


;; Constants
(define WIDTH  605)
(define HEIGHT 535)
(define MTS (rectangle WIDTH HEIGHT "solid" "Medium Gray"))

(define BALL-RADIUS 10)
(define TOP             BALL-RADIUS)
(define BOT (- HEIGHT 1 BALL-RADIUS))
(define LEF             BALL-RADIUS)
(define RIG (- WIDTH  1 BALL-RADIUS))
(define BALL (circle BALL-RADIUS "solid" "white"))

(define PWIDTH 20)
(define PHEIGHT 40)
(define PLAYER (ellipse PWIDTH PHEIGHT "solid" "Light Turquoise"))

(define WINSCREEN 
   (place-image/align (text "YOU WIN" 90 "Light Turquoise")
                      (/ WIDTH 2) (/ HEIGHT 2) "center" "center"
                      MTS))


  
 

;; Data Definitions 
(@htdd Player)
(define-struct player (x y speed))
;; Player is (make-player Natural Natural Integer)
;; interp. x and y are coordinates, speed is pixels moved per key
(define P1 (make-player (+ 5 PWIDTH) (+ 5 PHEIGHT) 10))


(@htdd Ball)
(define-struct ball (x y dx dy))
;; Ball is (make-ball Number Number Number Number)
;; interp. (make-ball x y dx dy) is ball
;;   - position x, y    in screen coordinates
;;   - velocity dx, dy  in pixels/tick
;; CONSTRAINT: x is in [LEF, RIG]; y is in [TOP, BOT]
(define DEADBALL (make-ball 10000 10000 0 0))
(define BMID (make-ball (/ WIDTH 2)
                        (/ HEIGHT 2) (+ 9 (random 13)) (+ 9 (random 13))))
(define BRCORNER (make-ball WIDTH HEIGHT (+ 9 (random 13)) (+ 9 (random 13))))
(define BLCORNER (make-ball 0 HEIGHT (+ 9 (random 13)) (+ 9 (random 13))))
(define BURCORNER (make-ball WIDTH 0 (+ 9 (random 13)) (+ 9 (random 13))))
(define LOB1 (list BMID BURCORNER BRCORNER BLCORNER))


(@htdd Game)
(define-struct game (balls player))
;; Game is (make-game (listof Ball) Player)
;; interp. state of game
(define G0 (make-game (list BMID) P1)) ;; for testing
(define G1 (make-game LOB1 P1)) ;; initialize game with G1 



;; Functions
(@htdf main)
(@signature Game -> Game)
;; main function running game using big bang, call with G1

(@template-origin htdw-main)

(define (main g)
  (big-bang g
    (on-tick next-game) ;; Game -> Game
    (on-draw render-game) ;; Game -> Image 
    (on-key handle-key))) ;; Game KeyEvent -> Game
    

    


(@htdf render-game)
(@signature Game -> Image)
;; renders entire game, using player and ball helper functions

(define (render-game g)
  (if (empty? (game-balls g))
      WINSCREEN
  (render-player g (render-balls g))))




(@htdf render-player)
(@signature Game Image -> Image)
;; renders player onto image created by render-balls 

(define (render-player g img)
  (local [(define p (game-player g))]
    (place-image PLAYER (player-x p) (player-y p) img)))



(@htdf render-balls)
(@signature Game -> Image) 
;; render all balls onto MTS

(define (render-balls g)
  (cond [(empty? (game-balls g)) MTS]
        [else
         (place-ball (first (game-balls g))
                     (render-balls
                      (make-game (rest (game-balls g))(game-player g))))]))




(@htdf place-ball)
(@signature Ball Image -> Image)
;; helper func to place ball in correct place onto a bg img

(define (place-ball b img)
  (place-image BALL (ball-x b) (ball-y b) img))





(@htdf next-game)
(@signature Game -> Game)
;; produces next game, if player touches a ball, reset board 

(define (next-game g)
  (cond [(touch-ball? (game-player g) (game-balls g)) G1] ;reset, lose
        [(touch-rcorner? (game-player g)) (make-game empty P1)] ;win 
        [else
         (make-game
          (next-balls (game-balls g)) (next-player (game-player g)))]))
      



(@htdf next-balls)
(@signature (listof Ball) -> (listof Ball))
;; uses helper next-ball on each ball in the list 

(define (next-balls lob)
  (map next-ball lob))



(@htdf next-ball)
(@signature Ball -> Ball)
;; produce ball at next x,y; checks bounces off top/right/bottom/left wall


(define (next-ball b)
  (cond [(touch-top?    b) (bounce-top b)]
        [(touch-bottom? b) (bounce-bottom b)]
        [(touch-right?  b) (bounce-right b)]
        [(touch-left?   b) (bounce-left b)]
        [else
         (glide b)]))


(@htdf next-player)
(@signature Player -> Player)
;; detects if at edge, if not then just return player

(define (next-player p)
  ;; won't work with problem set ball functions, these are made for player 
  (local [(define (ptouch-left? p)
            (<= (- (player-x p) (/ PWIDTH 2)) 0))
          (define (ptouch-right? p)
            (>= (+ (player-x p) (/ PWIDTH 2)) WIDTH))
          (define (ptouch-top? p)
            (<= (- (player-y p) (/ PHEIGHT 2)) 0))
          (define (ptouch-bot? p)
            (>= (+ (player-y p) (/ PHEIGHT 2)) HEIGHT))]
  (cond [(ptouch-left? p)
         (make-player (add1 (/ PWIDTH 2)) (player-y p) (player-speed p))]
        [(ptouch-right? p)
         (make-player
          (sub1 (- WIDTH (/ PWIDTH 2))) (player-y p) (player-speed p))]
        [(ptouch-top? p)
         (make-player (player-x p) (add1 (/ PHEIGHT 2)) (player-speed p))]
         [(ptouch-bot? p)
         (make-player (player-x p) (sub1 (- HEIGHT PHEIGHT)) (player-speed p))]
        [else p])))


(@htdf handle-key)
(@signature Game KeyEvent -> Game)
;; moves player around - uses player speed 

(@template-origin KeyEvent)

(define (handle-key g ke)
  (local [(define p (game-player g))]
    (cond [(key=? ke "left")
           (make-game
            (game-balls g)
            (make-player (- (player-x p) (player-speed p))
                         (player-y p)
                         (player-speed p)))]
          [(key=? ke "right")
           (make-game
            (game-balls g)
            (make-player (+ (player-x p) (player-speed p))
                         (player-y p)
                         (player-speed p)))]
          [(key=? ke "up")
           (make-game
            (game-balls g)
            (make-player (player-x p) 
                         (- (player-y p) (player-speed p))
                         (player-speed p)))]
          [(key=? ke "down")
           (make-game (game-balls g)
                      (make-player (player-x p) 
                                   (+ (player-y p) (player-speed p))
                                   (player-speed p)))]
          [else g])))
          


(@htdf touch-ball?)
(@signature Player (listof Ball) -> Boolean)
;; true if any balls are touching player
;; add and subtract half the width and height to account for size of player
;; detects if ball is touching ANY part of the player 

(define (touch-ball? p lob)
  (local [(define px (player-x p))
          (define py (player-y p))
          (define (touch? b)
            (and (<= (- px (/ PWIDTH 2))
                     (ball-x b)
                     (+ px (/ PWIDTH 2)))
                 (<= (- py (/ PHEIGHT 2))
                     (ball-y b)
                     (+ py (/ PHEIGHT 2)))))]
    (ormap touch? lob)))
    

(@htdf touch-rcorner?)
(@signature Player -> Boolean)
;; true if player is in lower right corner - win

(define (touch-rcorner? p)
   (local [(define px (player-x p))
          (define py (player-y p))]
     (and (= (sub1 (- WIDTH (/ PWIDTH 2))) px)
          (= (sub1 (- HEIGHT PHEIGHT)) py))))






;; -------- Below this are helper functions from the problem set --------------
(@htdf touch-top?)
(@signature Ball -> Boolean)
;; true if ball is going up and edge will hit top edge of box
(check-expect (touch-top?    (make-ball LEF (+ TOP  5) 3 -4)) false)
(check-expect (touch-top?    (make-ball LEF (+ TOP  4) 3 -4)) true)
(check-expect (touch-top?    (make-ball LEF (+ TOP  1) 3 -2)) true)
(check-expect (touch-top?    (make-ball LEF (+ TOP  0) 3  2)) false)
#;
(define (touch-top? b) false)

(@template-origin Ball)

(@template
 (define (touch-top? b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (touch-top? b)
  (<= (+ (ball-y b) (ball-dy b)) TOP))


(@htdf touch-bottom?)
(@signature Ball -> Boolean)
;; true if ball is going down and edge will hit bottom edge of box
(check-expect (touch-bottom? (make-ball LEF (- BOT 3) 3  2)) false)
(check-expect (touch-bottom? (make-ball LEF (- BOT 2) 3  2)) true)
(check-expect (touch-bottom? (make-ball LEF (- BOT 0) 3  2)) true)
(check-expect (touch-bottom? (make-ball LEF (- BOT 0) 3 -2)) false)
#;
(define (touch-bottom? b) false)

(@template-origin Ball)

(@template
 (define (touch-bottom? b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (touch-bottom? b)
  (>= (+ (ball-y b) (ball-dy b)) BOT))


(@htdf touch-left?)
(@signature Ball -> Boolean)
;; true if ball is going left and edge will hit left  edge of box
(check-expect (touch-left?   (make-ball (+ LEF 6) TOP -5 2)) false)
(check-expect (touch-left?   (make-ball (+ LEF 5) TOP -5 2)) true)
(check-expect (touch-left?   (make-ball (+ LEF 0) TOP -5 2)) true)
(check-expect (touch-left?   (make-ball (+ LEF 0) TOP  3 2)) false)
#;
(define (touch-left? b) false)

(@template-origin Ball)

(@template
 (define (touch-left? b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (touch-left? b)
  (<= (+ (ball-x b) (ball-dx b)) LEF))


(@htdf touch-right?)
(@signature Ball -> Boolean)
;; true if ball is going right and edge will hit right edge of box
(check-expect (touch-right?  (make-ball (- RIG 6) TOP  5 2)) false)
(check-expect (touch-right?  (make-ball (- RIG 5) TOP  5 2)) true)
(check-expect (touch-right?  (make-ball (- RIG 0) TOP  5 2)) true)
(check-expect (touch-right?  (make-ball (- RIG 0) TOP -3 2)) false)
#;
(define (touch-right? b) false)

(@template-origin Ball)

(@template
 (define (touch-right? b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (touch-right? b)
  (>= (+ (ball-x b) (ball-dx b)) RIG))


(@htdf bounce-top)
(@signature Ball -> Ball)
;; produce a ball with top edge 1 pixel off top of box, moving down
;; CONSTRAINT: assume ball is close to top edge and moving up
(check-expect (bounce-top (make-ball (+ LEF 1) (+ TOP 3) 2 -4))
              (make-ball (+ LEF 1) (+ TOP 1) 2  4))
(check-expect (bounce-top (make-ball (+ LEF 2) (+ TOP 6) 3 -7))
              (make-ball (+ LEF 2) (+ TOP 1) 3 7))
#;
(define (bounce-top b) b)

(@template-origin Ball)

(@template
 (define (bounce-top b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (bounce-top b)
  (make-ball (ball-x b) (+ TOP 1) (ball-dx b) (- (ball-dy b))))


(@htdf bounce-bottom)
(@signature Ball -> Ball)
;; produce a ball with bottom edge 1 pixel off bottom of box, moving up
;; CONSTRAINT: assume ball is close to bottom edge and moving down
(check-expect (bounce-bottom (make-ball (+ LEF 1) (- BOT 3) 2 4))
              (make-ball (+ LEF 1) (- BOT 1) 2  -4))
(check-expect (bounce-bottom (make-ball (+ LEF 2) (- BOT 6) 3 7))
              (make-ball (+ LEF 2) (- BOT 1) 3 -7))
#;
(define (bounce-bottom b) b)

(@template-origin Ball)

(@template
 (define (bounce-bottom b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (bounce-bottom b)
  (make-ball (ball-x b) (- BOT 1) (ball-dx b) (- (ball-dy b))))

(@htdf bounce-left)
(@signature Ball -> Ball)
;; produce a ball with left edge 1 pixel off left of box, moving right
;; CONSTRAINT: assume ball is close to left edge and moving left
(check-expect (bounce-left (make-ball (+ LEF 3) (+ TOP 2) -4 4))
              (make-ball (+ LEF 1) (+ TOP 2) 4 4))
(check-expect (bounce-left (make-ball (+ LEF 5) (+ TOP 2) -8 4))
              (make-ball (+ LEF 1) (+ TOP 2) 8 4))
#; 
(define (bounce-left b) b)

(@template-origin Ball)

(@template
 (define (bounce-left b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (bounce-left b)
  (make-ball (+ LEF 1) (ball-y b) (- (ball-dx b)) (ball-dy b) ))


(@htdf bounce-right)
(@signature Ball -> Ball)
;; produce a ball with right edge 1 pixel off right of box, moving left
;; CONSTRAINT: assume ball is close to right edge and moving right
(check-expect (bounce-right (make-ball (- RIG 3) (+ TOP 1) 4 4))
              (make-ball (- RIG 1) (+ TOP 1) -4 4))
(check-expect (bounce-right (make-ball (- RIG 5) (+ TOP 1) 8 4))
              (make-ball (- RIG 1) (+ TOP 1) -8 4))
#;
(define (bounce-right b) b)

(@template-origin Ball)

(@template
 (define (bounce-right b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (bounce-right b)
  (make-ball (- RIG 1) (ball-y b) (- (ball-dx b)) (ball-dy b)))


(@htdf glide)
(@signature Ball -> Ball)
;; move ball by dx dy
;; CONSTRAINT: ball is not touching or about to touch any edge of the box
(check-expect (glide (make-ball 100 200 2 3)) (make-ball 102 203 2 3))
(check-expect (glide (make-ball 50 220 -3 -2)) (make-ball 47 218 -3 -2))
#;
(define (glide b) b)

(@template-origin Ball)

(@template
 (define (glide b)
   (... (ball-x b)
        (ball-y b)
        (ball-dx b)
        (ball-dy b))))

(define (glide b)
  (make-ball (+ (ball-x b) (ball-dx b))
             (+ (ball-y b) (ball-dy b))
             (ball-dx b)
             (ball-dy b)))

       






