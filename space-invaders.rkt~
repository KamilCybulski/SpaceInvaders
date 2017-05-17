;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ==========================================================================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INV_X_SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INV_Y_SPEED 2)
(define TANK_SPEED 5)
(define MISSILE_SPEED 10)

(define HIT_RANGE 10)

(define INVADE_RATE 28)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define BLANK (rectangle WIDTH HEIGHT "outline" "WHITE"))

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

(define TANK_HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; ==========================================================================
;; Data Definitions:

(define-struct game (invaders missiles tank t))
;; Game is (make-game  (listof Invader) (listof Missile) Tank Natural)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position
;;         t represents the time for which game has been running (in ticks)

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK_SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dir))
;; Invader is (make-invader Number Number Integer[-1, 1])
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves left if dir is -1 and right if dir is 1

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right
(define I4 (make-invader 300 50 1))            ;not landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dir invader)))


;; listOfInvaders is one of:
;; - empty
;; -(cons Invader listOfInvaders)
(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I1 I2 I4))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 30 -1))

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; listOfMissiles is one of:
;; -empty
;; -(cons Missile listOfMissiles)
(define LOM0 empty)
(define LOM1 (list M1))
(define LOM2 (list M1 M2 M3))
(define LOM3 (list M1 M2 M3 M4))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (first lom)
              (fn-for-lom (rest lom)))]))
 

(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 10))
(define G2 (make-game (list I1) (list M1) T1 500))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 1256))
(define G4 (make-game (list I1 I4) (list M1 M2) T1 1000))
(define G5 (make-game (list I1 I3 I4) (list M1 M3 M4) T1 2011))

  
;; ==========================================================================
;; Functions

;; main
;; Game -> Game
;; Updates the state of the game and renders it

(define (main g)
  (big-bang g                                 ; game
            (on-tick updateGame)              ; game -> game  
            (to-draw renderObjects)           ; game -> image
            (on-release handleKey)            ; game keyEvent -> game
            (stop-when didInvaderLand?) ))    ; game -> boolean



;; updateGame
;; Game -> Game
;; Produces a new game with all the objects updated (position / existance)
(check-expect (updateGame G0)
              (make-game empty empty T0 1))    ; One tick after start, nothing moved

(check-expect (updateGame G1)
              (make-game empty empty T1 11))   ; Eleven ticks after start, nothing moved

(check-expect (updateGame G2)
              (make-game (list (make-invader (+ 150 INV_X_SPEED) (+ 100 INV_Y_SPEED) 1))
                         (list (make-missile 150 (- 300 MISSILE_SPEED)))
                         T1
                         501))
 
(check-expect (updateGame G4)
              (make-game (list (make-invader (- 300 INV_X_SPEED) (+ 50 INV_Y_SPEED) -1)) 
                         (list (make-missile 150 (- 300 MISSILE_SPEED)))
                         T1
                         1001))

 
(define (updateGame g)
  (invade (moveObjects (missilesOnScreenOnly (removeCollisions (updateTimer g))))))


 
;; updateTimer
;; Game -> Game
;; updates game timer by 1 tick
(check-expect (updateTimer G0)
              (make-game empty empty T0 1))
(check-expect (updateTimer G2)
              (make-game (list I1) (list M1) T1 501))

(define (updateTimer g)
  (make-game (game-invaders g) (game-missiles g) (game-tank g) (+ (game-t g) 1) ))

  

;; removeCollisions
;; Game -> Game
;; removes invaders and missiles that collide
(check-expect (removeCollisions G0)
              (make-game empty empty T0 0))

(check-expect (removeCollisions G2)
              (make-game (list I1) (list M1) T1 500))

(check-expect (removeCollisions G4)
              (make-game (list I4) (list M1) T1 1000))

(define (removeCollisions g)
  (make-game
   (filterInvaders (game-invaders g) (game-missiles g))
   (filterMissiles (game-missiles g) (game-invaders g))
   (game-tank g)
   (game-t g)))

 
;; filterInvaders
;; listOfInvaders listOfMissiles -> listOfInvaders
;; checks every Invader in the listOfInvaders for collision with any Missile in the listOfMissiles and removes those invaders from the list
(check-expect (filterInvaders empty empty) empty)
(check-expect (filterInvaders LOI1 empty) LOI1)
(check-expect (filterInvaders LOI2 LOM1) LOI2)
(check-expect (filterInvaders LOI2 LOM2) (list I2 I4))

(define (filterInvaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (cond [(invCollides? (first loi) lom) (filterInvaders (rest loi) lom)]
               [else
                (cons (first loi) (filterInvaders (rest loi) lom))])]))
 
;; invCollides?
;; Invader listOfMissiles -> Boolean
;; produces true if given Invader collides with any of the Missiles in the list
(check-expect (invCollides? I1 LOM0) false)
(check-expect (invCollides? I1 LOM1) false)
(check-expect (invCollides? I1 LOM2) true)
(check-expect (invCollides? I2 LOM2) false)

(define (invCollides? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (abs (- (missile-x (first lom)) (invader-x i))) HIT_RANGE)  (<= (abs (- (missile-y (first lom)) (invader-y i))) HIT_RANGE) )
             true
             (invCollides? i (rest lom)))]))
 
 
;; filterMissiles
;; listOfMissiles listOfInvaders -> listOfMissiles
;; analogically to filterInvaders, but produces a list of missiles
(check-expect (filterMissiles empty empty) empty)
(check-expect (filterMissiles LOM1 empty) LOM1)
(check-expect (filterMissiles LOM1 LOI2) LOM1)
(check-expect (filterMissiles LOM2 LOI1) (list M1))
(check-expect (filterMissiles LOM2 LOI2) (list M1))

(define (filterMissiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (cond [(mslCollides? (first lom) loi) (filterMissiles (rest lom) loi)]
               [else
                (cons (first lom) (filterMissiles (rest lom) loi))])]))
 

;; mslCollides?
;; Missile listOfInvaders -> Boolean
;; produces true if given Missile collides with any of the Invaders in the list
(check-expect (mslCollides? M1 empty) false)
(check-expect (mslCollides? M1 LOI2) false)
(check-expect (mslCollides? M2 LOI1) true)
(check-expect (mslCollides? M2 LOI2) true)

(define (mslCollides? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (<= (abs (- (invader-x (first loi)) (missile-x m))) HIT_RANGE) (<= (abs (- (missile-y m) (invader-y (first loi)))) HIT_RANGE))
             true
             (mslCollides? m (rest loi)))]))
 
 
;; missilesOnScreenOnly
;; Game -> Game
;; removes all the missiles that have moved beyond upper edge of BACKGROUND from the game
(check-expect (missilesOnScreenOnly G0) G0)
(check-expect (missilesOnScreenOnly G3) G3)
(check-expect (missilesOnScreenOnly G5)
              (make-game (list I1 I3 I4) (list M1 M3) T1 2011))

(define (missilesOnScreenOnly g)
  (make-game (game-invaders g) (clearMissiles (game-missiles g)) (game-tank g) (game-t g) ))


;; clearMissiles
;; listOfMissiles -> listOfMissiles
;; removes missiles with y < 0
(check-expect (clearMissiles empty) empty)
(check-expect (clearMissiles LOM1) LOM1)
(check-expect (clearMissiles LOM2) LOM2)
(check-expect (clearMissiles LOM3) (list M1 M2 M3))

(define (clearMissiles lom)
  (cond [(empty? lom) empty]
        [else
         (cond [(< (missile-y (first lom)) 0) (clearMissiles (rest lom))]
               [else
                (cons (first lom) (clearMissiles (rest lom)))])]))


 
;; moveObjects
;; Game -> Game
;; moves all the invaders in the Game by their y and x speed (in the proper direction!)
;; if an invader reaches right or left edge of the BACKGROUND, the direction is switched
;; moves all the missiles in the game towards upper edge by MISSILE_SPEED
(check-expect (moveObjects G0)
              (make-game empty empty T0 0))
(check-expect (moveObjects G2)
              (make-game (list (make-invader (+ 150 INV_X_SPEED) (+ 100 INV_Y_SPEED) 1))
                         (list (make-missile 150 (- 300 MISSILE_SPEED) ))
                         T1
                         500 ))
(check-expect (moveObjects G5)
              (make-game (list (make-invader (+ 150 INV_X_SPEED) (+ 100 INV_Y_SPEED)  1)
                               (make-invader (+ 150 INV_X_SPEED) (+ 510 INV_Y_SPEED)  1)
                               (make-invader (- 300 INV_X_SPEED) (+  50 INV_Y_SPEED) -1))
                         (list (make-missile 150 (- 300 MISSILE_SPEED) )
                               (make-missile 150 (- 105 MISSILE_SPEED) )
                               (make-missile  30 (-  -1 MISSILE_SPEED) ))
                         T1
                         2011))
 
(define (moveObjects g)
  (make-game (moveInvaders (game-invaders g))
             (moveMissiles (game-missiles g))
             (game-tank g)
             (game-t g)))
   
 
;; moveInvaders
;; listOfInvaders -> listOfInvaders
;; moves each invaders in the list on the x and y axis by their speed
(check-expect (moveInvaders LOI0) LOI0)
(check-expect (moveInvaders LOI1) (list (make-invader (+ 150 INV_X_SPEED) (+ 100 INV_Y_SPEED) 1)))
(check-expect (moveInvaders LOI2) (list
                                   (make-invader (+ 150 INV_X_SPEED) (+ 100 INV_Y_SPEED)  1)
                                   (make-invader (- 150 INV_X_SPEED) (+ 500 INV_Y_SPEED) -1)
                                   (make-invader (- 300 INV_X_SPEED) (+  50 INV_Y_SPEED) -1) ))

(define (moveInvaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (moveInvader (changeDir (first loi)))
               (moveInvaders (rest loi)))]))


;; moveInvader
;; Invader -> Invader
;; updates invaders x and y coordinates
(check-expect (moveInvader I1) (make-invader (+ 150 INV_X_SPEED) (+ 100 INV_Y_SPEED)  1))
(check-expect (moveInvader I2) (make-invader (- 150 INV_X_SPEED) (+ 500 INV_Y_SPEED) -1))

(define (moveInvader i)
  (make-invader  (+ (invader-x i) (* INV_X_SPEED (invader-dir i)))  (+ (invader-y i) INV_Y_SPEED) (invader-dir i)))
 

;; changeDir 
;; Invader -> Invader
;; changes the direction of the invader, if it touches the right or left edge of the BACKGROUND
(check-expect (changeDir I1) I1)
(check-expect (changeDir I2) I2)
(check-expect (changeDir I4) (make-invader 300 50 -1))
(check-expect (changeDir (make-invader 0 100 -1)) (make-invader 0 100 1))

(define (changeDir i)
  (cond [(<= (invader-x i) 0) (make-invader (invader-x i) (invader-y i) 1)]
        [(>= (invader-x i) WIDTH) (make-invader (invader-x i) (invader-y i) -1)]
        [else i]))

 


;; moveMissiles
;; listOfMissiles -> listOfMissiles
;; moves each missile in the list towards upper edge of the BACKGROUND by MISSILE_SPEED
(check-expect (moveMissiles LOM0) LOM0)
(check-expect (moveMissiles LOM1) (list (make-missile 150 (- 300 MISSILE_SPEED))))
(check-expect (moveMissiles LOM2) (list
                                   (make-missile 150 (- 300 MISSILE_SPEED))
                                   (make-missile 150 (- 110 MISSILE_SPEED))
                                   (make-missile 150 (- 105 MISSILE_SPEED)) )) 

(define (moveMissiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE_SPEED)) (moveMissiles (rest lom)))]))



;; invade
;; Game -> Game
;; adds a new invader every INVADE_RATE ticks
(check-random (invade G0) (make-game (list (make-invader (random WIDTH) 0 1)) empty T0 0))
(check-random (invade (make-game empty empty T0 28)) (make-game (list (make-invader (random WIDTH) 0 1)) empty T0 28))
(check-random (invade G3) G3)
(check-random (invade (make-game empty empty T0 84)) (make-game (list (make-invader (random WIDTH) 0 1)) empty T0 84))

(define (invade g)
  (if (integer? (/ (game-t g) INVADE_RATE))
      (make-game (cons (make-invader (random WIDTH) 0 1) (game-invaders g)) (game-missiles g) (game-tank g) (game-t g))
      g ))

  
;; renderObjects
;; Game -> Image
;; draws current state of the game on the BACKGROUND
(check-expect (renderObjects G1)
       (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                    (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                                 (place-image TANK (tank-x T1) HEIGHT
                                              BACKGROUND))))
  
(check-expect (renderObjects G2)
              (place-image INVADER (invader-x I1) (invader-y I1)
                    (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                           (place-image MISSILE (missile-x M1) (missile-y M1)
                                   (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                                        (place-image TANK (tank-x T1) HEIGHT
                                                     BACKGROUND))))))

(check-expect (renderObjects G3)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image INVADER (invader-x I2) (invader-y I2)
                                   (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                                        (place-image MISSILE (missile-x M1) (missile-y M1)
                                                     (place-image MISSILE (missile-x M2) (missile-y M2)
                                                            (place-image BLANK (/ WIDTH 2) (/ HEIGHT 2)
                                                              (place-image TANK (tank-x T1) HEIGHT
                                                                    BACKGROUND))))))))


(define (renderObjects g)
  (place-image (renderInvaders (game-invaders g)) (/ WIDTH 2) (/ HEIGHT 2)
                               (place-image (renderMissiles (game-missiles g)) (/ WIDTH 2) (/ HEIGHT 2)
                                            (place-image TANK (tank-x (game-tank g)) HEIGHT
                                                         BACKGROUND))))
;; renderInvaders
;; listOfInvaders -> Image
;; renders all the invaders in the list
;; tests from renderObjects apply

(define (renderInvaders loi)
  (cond [(empty? loi) BLANK]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (renderInvaders (rest loi)))]))


;; renderMissiles
;; listOfMissiles -> Image
;; renders all the missiles in the list
;; tests from renderObjects apply

(define (renderMissiles lom)
  (cond [(empty? lom) BLANK]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (renderMissiles (rest lom)))]))
   

 
;; handleKey
;; Game keyEvent -> game
;; moves the tank when arrow keys are pressed and shoot a missle when space key is pressed
(check-expect (handleKey G1 "left")
              (make-game (game-invaders G1) (game-missiles G1) (make-tank (- (tank-x (game-tank G1))  TANK_SPEED) -1) (game-t G1)))
(check-expect (handleKey G1 "right")
              (make-game (game-invaders G1) (game-missiles G1) (make-tank (+ (tank-x (game-tank G1)) TANK_SPEED) 1) (game-t G1)))
(check-expect (handleKey G1 " ")
              (make-game (game-invaders G1) (cons (make-missile (tank-x (game-tank G1)) HEIGHT) (game-missiles G1)) (game-tank G1) (game-t G1)))

(define (handleKey g ke)
  (cond [(or (key=? ke "right")(key=? ke "left")) (moveTank g ke)]
        [(key=? ke " ") (shoot g)] ))


;; moveTank
;; Game -> Game
;; moves tank in the proper direction if the <- or -> key is pressed
;; tests from handleKey apply

(define (moveTank g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (make-tank (- (tank-x (game-tank g)) TANK_SPEED) -1) (game-t g))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (+ (tank-x (game-tank g)) TANK_SPEED) 1) (game-t g))]))


;; shoot
;; Game -> Game
;; shoots a missiles if space key is pressed
;; tests from handleKey apply
(define (shoot g)
  (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) HEIGHT) (game-missiles g)) (game-tank g) (game-t g) ))


;; didInvaderLand?
;; Game -> Boolean
;; produces true if the invader has reachen the bottom edge of the BACKGROUND
(check-expect (didInvaderLand? G0) false)
(check-expect (didInvaderLand? G1) false)
(check-expect (didInvaderLand? G2) false)
(check-expect (didInvaderLand? G3) true)
(check-expect (didInvaderLand? G5) true)

(define (didInvaderLand? g)
  (checkInvader(game-invaders g)))


;; checkInvader
;; listOfInvaders -> Boolean
;; checks if any of the invaders in the list have reached the bottom edge
;; tests from didInvaderLand? apply

(define (checkInvader loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (checkInvader (rest loi)) )]))

 



 
 
























