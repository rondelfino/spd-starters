;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MAX-INVADER-COUNT 5)
(define INVADER-SPAWN-Y -5)


;; =================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; =================
;; Functions:

;; Game -> Game
;; start space invaders with (main G0)
;; 
(define (main s)
  (big-bang s                    ; Game
    (on-tick   update-game)      ; Game -> Game
    (to-draw   render-game)      ; Game -> Image
    (on-release handle-attacks)  ; Game KeyEvent -> Game
    (stop-when game-over)        ; Game-> Boolean
    (on-key    handle-movement))); Game KeyEvent -> Game




;; Game -> Game
;; update the game state including:
;;   - tank x position and direction
;(check-expect (update-game (make-game empty empty (make-tank 0 0))) (make-game (spawn-invaders empty MAX-INVADER-COUNT) empty (make-tank 0 0)))

;(define (update-game s) (make-game empty empty (make-tank 0 0)))   ;stub

;; <use template from Game>

(define (update-game s)
  (make-game (spawn-invaders (update-invaders (destroy-invaders (game-invaders s) (game-missiles s))) MAX-INVADER-COUNT)
             (update-missiles (destroy-missiles (game-missiles s) (game-invaders s)))
             (move-tank (game-tank s))))




;; ListOfInvader ListOfMissile -> ListOfInvader
;; destroy invaders that collide with missiles
(check-expect (destroy-invaders empty (list M1)) empty)
(check-expect (destroy-invaders empty empty) empty)
(check-expect (destroy-invaders (list I1) empty) (list I1))
(check-expect (destroy-invaders (list I1 I2) (list M1 M2)) (list I2))

;(define (destroy-invaders loi lom) loi)   ;stub

(define (destroy-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (invader-collides? (first loi) lom)
             (destroy-invaders (rest loi) lom)
             (cons (first loi) (destroy-invaders (rest loi) lom)))]))




;; ListOfMissile ListOfInvader -> ListOfMissile
;; destroy missiles that are out of bounds or collide with invaders
(check-expect (destroy-missiles empty empty) empty)
(check-expect (destroy-missiles empty (list I1)) empty)
(check-expect (destroy-missiles (list M1) empty) (list M1))
(check-expect (destroy-missiles (list M1 M2) (list I1 I2)) (list M1))

;(define (destroy-missiles lom loi) lom)   ;stub

(define (destroy-missiles lom loi)
  (cond [(empty? lom) empty]
        [(empty? loi) lom]
        [else
         (if (or (missile-collides? (first lom) loi) (oob? (missile-y (first lom)) MISSILE))
             (destroy-missiles (rest lom) loi)
             (cons (first lom) (destroy-missiles (rest lom) loi)))]))



;; Missile -> Boolean
;; produce true if missile is out of bounds
(check-expect (oob? 0 MISSILE) false)
(check-expect (oob? -15 MISSILE) (< -15 (- 0 (image-height MISSILE))))

;(define (oob? y img) false)   ;stub

(define (oob? y img)
  (or (< y (- 0 (image-height MISSILE)))
      (> y (+ HEIGHT (image-height img)))))



;; Invader ListOfMissile -> Boolean
;; produces true if the given invader collides with any missile in the list
(check-expect (invader-collides? I1 (list M1 M2 M3)) true)
(check-expect (invader-collides? I1 (list M1)) false)

;(define (invader-collides? invader lom) false)   ;stub

;; <use tempalte from Invader with additional one of argument>

(define (invader-collides? invader lom)
  (cond [(empty? lom) false]
        [else
         (if (collides? invader (first lom))
             #t
             (invader-collides? invader (rest lom)))]))



;; Missile ListOfInvader -> Boolean
;; produces true if the given missile collides with any invader in the list
(check-expect (missile-collides? M1 (list I1 I2 I3)) false)
(check-expect (missile-collides? M2 (list I1 I2 I3)) true)

;(define (missile-collides? missile loi) false)   ;stub

(define (missile-collides? missile loi)
  (cond [(empty? loi) false]
        [else
         (if (collides? (first loi) missile)
             #t
             (missile-collides? missile (rest loi)))]))


                           
;; Missile Invader -> Boolean
;; produces true when a missile is within the hit range of an invader
;; within hit range if the distance between the two is <= HIT-RANGE
(check-expect (collides? I1 M1) false)
(check-expect (collides? I1 (make-missile (invader-x I1) (+ (invader-y I1) 10))) true)  ; within hit range
(check-expect (collides? I1 (make-missile 50 (invader-y I1))) false)
(check-expect (collides? I1 (make-missile (invader-x I1) (+ (invader-y I1) 5))) true)
(check-expect (collides? I1 (make-missile (invader-x I1) (- (invader-y I1) 10))) true)
(check-expect (collides? I1 (make-missile (invader-x I1) (+ (invader-y I1) 11))) false)

;(define (collides? invader missile) false)   ;stub

;; <use template from Invader with additional Missile argument>

(define (collides? invader missile)
  (<= (distance (invader-x invader) (invader-y invader)
                (missile-x missile) (missile-y missile))
      HIT-RANGE)) 



;; Number Number Number Number -> Number
;; calculate the distance between (x1, y1) and (x2, y2)
(check-expect (distance 0 0 0 0) 0)
(check-expect (distance 50 25 30 25) (sqrt (+ (sqr (- 30 50)) (sqr (- 25 25)))))

;(define (distance x1 y1 x2 y2) 0)   ;stub

(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1))
           (sqr (- y2 y1)))))



;; ListOfInvader Natural -> ListOfInvader
;; spawn an invader at a random x position and direction
;; the number of invaders on screen is limited by the given natural
;(check-expect (spawn-invaders empty 5) (list (make-invader 50 0 -1)))              ; invader-dx should be a random direction
;(check-expect (spawn-invaders (list I1 I1 I1 I2 I3) 5) (list I1 I1 I1 I2 I3))      ; list length = max invader count

;(define (spawn-invaders loi max) empty)   ;stub

(define (spawn-invaders loi max)
  (cond [(>= (length loi) max) loi]
        [(< (random 100) INVADE-RATE)
         (cons (make-invader (random WIDTH)
                             INVADER-SPAWN-Y
                             (list-ref (list -1 1) (random 2)))
               loi)]
        [else loi]))



;; ListOfInvaders -> ListOfInvaders
;; update the position
;; change direction if it touches an edge
;; increment x and y by INVADER-SPEED
(check-expect (update-invaders empty) empty)
(check-expect (update-invaders (list I1)) (list (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 12)))
(check-expect (update-invaders (list I1 I2 I3)) (list (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 12)
                                                      (make-invader (+ 150 (* -10 INVADER-X-SPEED)) (+ HEIGHT INVADER-Y-SPEED) -10)
                                                      (make-invader (+ 150 (* 10 INVADER-X-SPEED)) (+ (+ HEIGHT 10) INVADER-Y-SPEED) 10)))
(check-expect (update-invaders (list (make-invader (- 0 1) 100 -10))) (list (make-invader (+ (- 0 1) (* 10 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) (* -1 -10))))  ; invader hit left edge
(check-expect (update-invaders (list (make-invader (+ WIDTH 1) 100 10))) (list (make-invader (+ (+ WIDTH 1) (* -10 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) (* -1 10)))) ; invader hit right edge

;(define (update-invaders loi) empty)   ;stub

(define (update-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (update-invader (first loi))
                    (update-invaders (rest loi)))]))



;; Invader -> Invader
;; update the position of the invader

;(define (update-invader invader) (make-invader 0 0 0))   ;stub

;; <use template from Invader>

(define (update-invader invader)
  (cond [(or (< (invader-x invader) 0)
             (> (invader-x invader) WIDTH))
         (make-invader (+ (invader-x invader) (* (* -1 (invader-dx invader))
                                                 INVADER-X-SPEED))
                       (+ (invader-y invader) INVADER-Y-SPEED)
                       (* -1 (invader-dx invader)))]
        [else
         (make-invader (+ (invader-x invader) (* (invader-dx invader)
                                                 INVADER-X-SPEED))
                       (+ (invader-y invader) INVADER-Y-SPEED)
                       (invader-dx invader))]))



;; ListOfMissile -> ListOfMissile
;; update the y pos of the spawned missiles
;; missiles with y pos < (- 0 (image-height MISSILE)) should be removed from the list
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles (cons (make-missile 150 300) empty)) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))

;(define (update-missiles lom) empty)   ;stub

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (update-missile (first lom))
               (update-missiles (rest lom)))]))



;; Missile -> Missile
;; update the y pos of a missile by (- ypos MISSILE-SPEED)
(check-expect (update-missile (make-missile 150 300)) (make-missile 150 (- 300 MISSILE-SPEED)))

;(define (update-missile m) (make-missile 0 0))   ;stub

(define (update-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Tank -> Tank
;; update velocity of tank
(check-expect (move-tank T1) (make-tank (+ 50 (* 1 TANK-SPEED)) 1))
(check-expect (move-tank T2) (make-tank (+ 50 (* -1 TANK-SPEED)) -1))

;(define (move-tank t) (make-tank 0 0))  ;stub

;; <use template from Tank>

(define (move-tank t)
  (make-tank
   (+ (tank-x t) (* (tank-dir t) TANK-SPEED))
   (tank-dir t)))



;; Game -> Image
;; render the game objects (invaders, missles, tank)
;; Tank tests:
(check-expect (render-game (make-game empty empty (make-tank 0 0))) (place-image TANK 0 TANK-Y BACKGROUND))
(check-expect (render-game (make-game empty empty T1)) (place-image TANK 50 TANK-Y BACKGROUND))

;; Missile tests:
(check-expect (render-game (make-game empty empty T1)) (place-image empty-image 0 0 (render-tank T1 BACKGROUND)))
(check-expect (render-game (make-game empty (list M1) T1)) (place-image MISSILE 150 300 (render-tank T1 BACKGROUND)))

;; Invader tests:
(check-expect (render-game (make-game empty empty T1)) (place-image empty-image 0 0 (render-missiles empty (render-tank T1 BACKGROUND))))
(check-expect (render-game (make-game (list I1) empty T1)) (place-image INVADER 150 100 (render-missiles empty (render-tank T1 BACKGROUND))))

;(define (render-game s) empty-image)   ;stub

;; <use template from Game>

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-tank (game-tank s)
                                (render-missiles (game-missiles s) BACKGROUND))))



;; ListOfInvader Image -> Image
;; render invaders onto the scene
(check-expect (render-invaders empty (render-missiles empty (render-tank T1 BACKGROUND))) (render-missiles empty (render-tank T1 BACKGROUND)))
(check-expect (render-invaders (list I1) (render-missiles empty (render-tank T1 BACKGROUND))) (place-image INVADER 150 100 (render-missiles empty (render-tank T1 BACKGROUND))))

;(define (render-invaders loi scene) empty-image)   ;stub

(define (render-invaders loi scene)
  (cond [(empty? loi) scene]
        [else (render-invader-on (first loi)
                                 (render-invaders (rest loi) scene))]))



;; Invader Image -> Image
;; render invader on image

;(define (render-invader-on invader img) empty-image)   ;stub

(define (render-invader-on invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img))



;; ListOfMissiles Image -> Image
;; render missiles onto the scene
(check-expect (render-missiles empty (render-tank T1 BACKGROUND)) (render-tank T1 BACKGROUND))
(check-expect (render-missiles (list M1) (render-tank T1 BACKGROUND)) (place-image MISSILE 150 300 (render-tank T1 BACKGROUND)))

;(define (render-missiles lom scene) empty-image)   ;stub

(define (render-missiles lom scene)
  (cond [(empty? lom) scene]
        [else
         (render-missile-on (first lom)
                            (render-missiles (rest lom) scene))]))



;; Missile Image -> Image
;; render missile on image
(check-expect (render-missile-on (make-missile 150 300) BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))

;(define (render-missile-on m img) empty-image)   ;stub

(define (render-missile-on m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))



;; Tank Image -> Image
;; render tank onto scene
(check-expect (render-tank (make-tank 0 0) BACKGROUND) (place-image TANK 0 TANK-Y BACKGROUND))
(check-expect (render-tank T1 empty-image) (place-image TANK 50 TANK-Y empty-image))

;(define (render-tank t img) empty-image)   ;stub

;; <use template from Tank>

(define (render-tank t img)
  (place-image TANK
               (tank-x t)
               TANK-Y img))



;; Game KeyEvent -> Game
;; move tank left when the left arrow key is down
;; move tank right when right arrow key is down
;; shoot missiles when space is down

;; Tank movement tests:
(check-expect (handle-movement (make-game empty empty (make-tank 0 0)) "a") (make-game empty empty (make-tank 0 0)))
(check-expect (handle-movement (make-game empty empty T1) "left") (make-game empty empty (make-tank 50 -1))); tank moving left
(check-expect (handle-movement (make-game empty empty T2) "right") (make-game empty empty (make-tank 50 1))) ; tank moving right

;; Missile tests:
(check-expect (handle-attacks (make-game empty empty T1) " ") (make-game empty (cons (make-missile 50 HEIGHT) empty) T1))
(check-expect (handle-attacks (make-game empty empty T1) "a") (make-game empty empty T1))
(check-expect (handle-attacks (make-game empty (cons (make-missile 50 (/ HEIGHT 2)) empty) T1) " ") (make-game empty (cons (make-missile 50 HEIGHT)
                                                                                                                           (cons (make-missile 50 (/ HEIGHT 2)) empty))
                                                                                                               T1))
(define (handle-movement s ke)
  (cond [(key=? ke "left") (make-game (game-invaders s) (game-missiles s) (update-direction (game-tank s) -1))]
        [(key=? ke "right") (make-game (game-invaders s) (game-missiles s) (update-direction (game-tank s) 1))]
        [else s]))

(define (handle-attacks s ke)
  (cond [(key=? ke " ") (make-game (game-invaders s) (spawn-missile (game-tank s) (game-missiles s)) (game-tank s))]
        [else s]))



;; Tank Integer[-1, 1] -> Tank
;; update the the direction of the tank
(check-expect (update-direction T1 -1) (make-tank 50 -1)); tank moving left
(check-expect (update-direction T2 1) (make-tank 50 1))   ; tank moving left ; tank moving right

;(define (update-direction t d) (make-tank 0 0))  ;stub

;; <use template from Tank with additional inverval argument>

(define (update-direction t d)
  (make-tank (tank-x t) d))



;; Tank ListOfMissile -> ListOfMissile
;; add a new missile at the given tank x position to the list of missiles
(check-expect (spawn-missile (make-tank 50 1) empty) (cons (make-missile 50 HEIGHT) empty))
(check-expect (spawn-missile (make-tank 25 -1) (cons (make-missile 50 (/ HEIGHT 2)) empty)) (cons (make-missile 25 HEIGHT)
                                                                                                  (cons (make-missile 50 (/ HEIGHT 2)) empty)))
;(define (spawn-missile t lom) empty)   ;stub

(define (spawn-missile t lom)
  (cons (make-missile (tank-x t) HEIGHT) lom))



;; Game -> Boolean
;; produce true if game over criteria is met
(define (game-over s)
  (landed? (game-invaders s)))



;; ListOfInvader -> Boolean
;; produce true if an invader reaches the bottom of the screen
(check-expect (landed? empty) false)
(check-expect (landed? (list (make-invader 0 (/ HEIGHT 2) -1))) false)
(check-expect (landed? (list (make-invader 0 (/ HEIGHT 2) -1)
                             (make-invader 25 (+ HEIGHT 1) 1))) false)
(check-expect (landed? (list (make-invader 0 (/ HEIGHT 2) -1)
                             (make-invader 25 (+ HEIGHT (image-height INVADER) 1) 1))) true)

;(define (landed? loi) false)   ;stub

;; <use template from ListOfInvader>

(define (landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (oob? (invader-y (first loi)) INVADER)
             #t
             (landed? (rest loi)))]))