;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lesson_10_HW) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 200)
(define MT (empty-scene WIDTH HEIGHT))
(define TANK (rectangle 25 40 "solid" "black")) 25 25
(define TARGET (rectangle 50 16 "solid" "red")) 50 25
(define MISSILE (circle 6 "solid" "yellow")) 7
(define BOTTOM HEIGHT)
(define TOP 8)
(define TARGET-SPEED 3)
(define MISSILE-SPEED 1)
(define TANK-SPEED 1)
(define INIT-TARGET 25)
(define FINAL-TARGET 175)

(define-struct game [target-x tank-x missiles target-direction])
;;(define INIT-GAME (make-game (make-posn (game-target-x game-world) 25 0) (make-posn (game-tank-x game-world) 100 0) (make-posn (game-fired-missile game-world) (game-target-x game-world) 173) #false "right"))
(define INIT-GAME (make-game 25 100 '() "right"))
;; A GameWorld is a (make-game Number Number List-of-Missiles Direction)
;; Interpretation: (make-game target-x tank-x missiles target-direction)
;; where target-x is the x-co of the target
;; Where tank-x is the x-co of the tank
;; A List-of-Missiles is one of
;; - '()
;; - (cons BULLET List-of-Missiles)
;; Where target-direction can be either "left" or "right"

;; A Missile is a (make-posn number number)
;; Interpretation: (make-posn x y) x-co, y-co

;; Image List-of-Missiles -> Image
;; Draw the list of the images
(define (render-missiles img alom)
  (cond
    [(empty? alom) img]
    [else (place-image MISSILE (posn-x (first alom)) (posn-y (first alom)) (render-missiles img (rest alom)))]))


;; GameWorld -> Image
;; Draw the current game-world
(check-expect (render-game INIT-GAME)
               (place-image TARGET 25 TOP
               (place-image TANK 100 BOTTOM MT)))
(define (render-game game-world)
  (place-image TARGET (game-target-x game-world) TOP
               (place-image TANK (game-tank-x game-world) BOTTOM
                            (render-missiles MT (game-missiles game-world)))))
                                                

;; For the on-tick functions

;; List of Missiles -> List of Missiles
;; Make all of the missiles move up
(check-expect (move-missile '()) '())
(check-expect (move-missile (cons (make-posn 100 173) '())) (cons (make-posn 100 172) '()))
(check-expect (move-missile (cons (make-posn 50 120) '())) (cons (make-posn 50 119) '()))
(define (move-missile alom)
  (cond
    [(empty? alom) '()]
    [else (cons (make-posn (posn-x (first alom)) (- (posn-y (first alom)) MISSILE-SPEED)) (move-missile (rest alom)))]))

;; Number Direction -> Number
;; Move target
(check-expect (move-target 100 "right") 103)
(check-expect (move-target 200 "right") 197)
(check-expect (move-target 200 "left") 197)
(check-expect (move-target 0 "left") 3)
(check-expect (move-target 0 "right") 3)
(define (move-target num dir)
  (cond
   [(and (< num FINAL-TARGET) (string=? "right" dir)) (+ TARGET-SPEED num)]
   [(and (>= num FINAL-TARGET) (string=? "right" dir)) (- num TARGET-SPEED)]
   [(and (> num INIT-TARGET) (string=? "left" dir)) (- num TARGET-SPEED)]
   [(and (<= num INIT-TARGET) (string=? "left" dir)) (+ TARGET-SPEED num)]))

;; Number Direction -> Direction
;; Tell the next direction of the target
(check-expect (next-direction 200 "right") "left")
(check-expect (next-direction 100 "right") "right")
(check-expect (next-direction 0 "right") "right")
(check-expect (next-direction 0 "left") "right")
(check-expect (next-direction 100 "left") "left")
(define (next-direction num dir)
  (cond
   [(and (< num FINAL-TARGET) (string=? "right" dir)) "right"]
   [(and (>= num FINAL-TARGET) (string=? "right" dir)) "left"]
   [(and (> num INIT-TARGET) (string=? "left" dir)) "left"]
   [(and (<= num INIT-TARGET) (string=? "left" dir)) "right"]))

;; GameWorld -> GameWorld
;; Move target and missile when the user has fired
(define (move-game game-world)
  (make-game
   (move-target (game-target-x game-world) (game-target-direction game-world))
   (game-tank-x game-world)
   (move-missile (game-missiles game-world))
   (next-direction (game-target-x game-world) (game-target-direction game-world))))


;; For the on-key functions

;; GameState -> GameState
;; Move the tank to the right
(check-expect (move-tank-right INIT-GAME)
  (make-game 25 101 '() "right"))
(define (move-tank-right game-world)
  (make-game
   (game-target-x game-world)
   (+ (game-tank-x game-world) TANK-SPEED)
   (game-missiles game-world)
   (game-target-direction game-world)))


;; GameState -> GameState
;; Move the tank to the left
(check-expect (move-tank-left (make-game 25 100 (cons (make-posn 100 173) '()) "left"))
  (make-game 25 99 (cons (make-posn 100 173) '()) "left"))
(define (move-tank-left game-world)
  (make-game
   (game-target-x game-world)
   (- (game-tank-x game-world) TANK-SPEED)
   (game-missiles game-world)
   (game-target-direction game-world)))

;; GameWorld -> GameWorld
;; Render a new missile when space key is clicked
(define (make-missile game-world)
  (make-game
   (game-target-x game-world)
   (game-tank-x game-world)
   (cons (make-posn (game-tank-x game-world) 173) (game-missiles game-world))
   (game-target-direction game-world)))

;; gameState keyEvent -> gameState
;; Do action depending on what the user inputs
(check-expect (move-tank-missile INIT-GAME "right")
              (make-game 25 101 '() "right"))
(define (move-tank-missile game-world key)
  (cond
    [(key=? "right" key) (move-tank-right game-world)]
    [(key=? "left" key) (move-tank-left game-world)]
    [(key=? " " key) (make-missile game-world)]
    [else game-world]))


;; Missile Number -> Boolean
;; End the game when one of the missiles hits the target
(check-expect (first-hit-target? (make-posn 100 100) 100) #false)
(check-expect (first-hit-target? (make-posn 0 60) 30) #false)
(check-expect (first-hit-target? (make-posn 100 15) 100) #true)
(define (first-hit-target? missile target-x)
  (cond
    [(empty? missile) #false]
    [else (and
           (<= (posn-x missile) (+ target-x (/ (image-width TARGET) 2)))
           (>= (posn-x missile) (- (/ (image-width TARGET) 2) target-x))
           (<= (posn-y missile) (image-height TARGET))
           (>= (posn-y missile) 0))]))


;; List-of-missile Number -> Boolean
;; Check if the the rest, excluding the first missile, hit the target
(check-expect (hit-target? '() 100) #false)
(check-expect (hit-target? (cons (make-posn 100 15) '()) 100) #true)
(define (hit-target? alom target-x)
  (cond
    [(empty? alom) #false]
    [else (or (first-hit-target? (first alom) target-x)
                      (hit-target? (rest alom) target-x))]))

;; Boolean -> GameWorld
;; End the game when the boolean result is true
(define (game-over? game-world)
   (hit-target? (game-missiles game-world) (game-target-x game-world)))


(big-bang INIT-GAME
  [to-draw render-game] ;; Render the image of the game
  [on-tick move-game] ;; Move the missile and the target on tick
  [on-key move-tank-missile] ;; Move the tank and trigger the missile when the key is pressed
  [stop-when game-over?]) ;; Stop the missile when it hits the target or when it reaches the end of the screen
               
