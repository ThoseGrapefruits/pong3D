#lang typed/racket

(require
  math/flonum
  pict3d
  pict3d/universe
  typed/racket/base
  typed/racket/gui
  typed-compose)

;
;                                  ═══
;
;     -                               
;    /                             ╭─╮
;   x                              ╰─╯
;  /                                                                        +
; +                                                                         |
;                               ╓───────╖                                   z 
;                               ╙───────╜                                   |
;                             - ——— y ——— +                                 -

;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: BALL-ACCELERATION-PADDLE : Flonum)
(define BALL-ACCELERATION-PADDLE 1.04)

(: BALL-ACCELERATION-WALL : Flonum)
(define BALL-ACCELERATION-WALL 1.01)

(: BALL-RADIUS : Flonum)
(define BALL-RADIUS 0.024)

(: BALL-SPEED : Flonum)
(define BALL-SPEED 0.001)

(: CAMERA-SPACE-DISTANCE : Flonum)
(define CAMERA-SPACE-DISTANCE 0.05)

(: BUMPER-SCALE : Dir)
(define BUMPER-SCALE (dir 0.015 0.11 0.03))

(: BUMPER-CONTACT-WIDTH : Flonum)
(define BUMPER-CONTACT-WIDTH (+ (dir-dy BUMPER-SCALE) BALL-RADIUS))

(: BOUNDS-BUMPER-GAP : Flonum)
(define BOUNDS-BUMPER-GAP (* 10 (dir-dx BUMPER-SCALE)))

(: CONTACT-BUFFER : Flonum)
(define CONTACT-BUFFER (+ BALL-RADIUS (dir-dx BUMPER-SCALE)))

(: FRAME-DELAY-MILLIS : Positive-Real)
(define FRAME-DELAY-MILLIS (max 1.0 (abs (/ 1000.0 59.9))))

(: OPPONENT-SPEED : Flonum)
(define OPPONENT-SPEED 0.001)

(: OPPONENT-X : Flonum)
(define OPPONENT-X -1.0)

(: OPPONENT-BOUNDS : Flonum)
(define OPPONENT-BOUNDS (- OPPONENT-X BOUNDS-BUMPER-GAP))

(: PLAYER-X : Flonum)
(define PLAYER-X 1.0)

(: PLAYER-BOUNDS : Flonum)
(define PLAYER-BOUNDS (+ PLAYER-X BOUNDS-BUMPER-GAP))

(: REDIRECT-FACTOR : Flonum)
(define REDIRECT-FACTOR 40.0)

(: SCREEN-WIDTH : Positive-Integer)
(define SCREEN-WIDTH 1200)

(: SCREEN-WIDTH-INEXACT : Flonum)
(define SCREEN-WIDTH-INEXACT (exact->inexact SCREEN-WIDTH))

(: SCREEN-HEIGHT : Positive-Integer)
(define SCREEN-HEIGHT 1080)

(: SCREEN-HEIGHT-INEXACT : Flonum)
(define SCREEN-HEIGHT-INEXACT (exact->inexact SCREEN-HEIGHT))

(: WALL-Y : Flonum)
(define WALL-Y 0.8)

(: aspect-ratio : -> Flonum)
(define (aspect-ratio) (/ SCREEN-WIDTH-INEXACT
                          SCREEN-HEIGHT-INEXACT))

(: CAMERA-LOOK-AT : Pos)
(define CAMERA-LOOK-AT origin)

;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Ball
  ([dir : Dir]
   [pos : Pos]))

(struct Opponent
  ([y : Flonum ]))

(struct Player
  ([lives : Nonnegative-Integer]
   [score : Nonnegative-Integer]
   [score-multiplier : Nonnegative-Flonum]
   [y : Flonum]))

(struct State
  ([dt   : Flonum]
   [n    : Natural]
   [pressed : (Setof String)]
   [t    : Flonum])
  #:transparent)

(struct State-Game-Over State
  ([end-state : State-Play])
  #:transparent)

(struct State-Main-Menu State
  ()
  #:transparent)

(struct State-Paused State
  ; hold a copy of the last play state for when the game resumes
  ([resume-state : State-Play])
  #:transparent)

(struct State-Play State
  ([ball : Ball]
   [opponent : Opponent]
   [player : Player]
   [start-t : Flonum])
  #:transparent)

;; STATE UPDATERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; [distant, maniacal laughter]
; https://stackoverflow.com/a/77208704/2883258
;
; This horrible macro expansion thing lets us get around the fact that
; struct-copy doesn't have much runtime flexibility with choosing what child
; struct to use when copying, while avoiding repeating the same cond statement
; in a bunch of different update methods.
(define-syntax-rule (base-state-update s [t #:parent State v] ...)
  (cond [(State-Game-Over? s)
         (struct-copy State-Game-Over s [t #:parent State v] ...)]
        [(State-Main-Menu? s)
         (struct-copy State-Main-Menu s [t #:parent State v] ...)]
        [(State-Paused? s)
         (struct-copy State-Paused s [t #:parent State v] ...)]
        [(State-Play? s)
         (struct-copy State-Play s [t #:parent State v] ...)]
        [else (error (~s "Invalid state: " s))]))

; This other horrible macro syntax expansion thing lets us transition between
; states more easily, copying the fields stored on the base State struct.
(define-syntax-rule (state-transition S s field ...)
  (S (State-dt s)
     (State-n s)
     (State-pressed s)
     (State-t s)
     field ...))

; Update game timers in state
(: update-counters : State Natural Flonum -> State)
(define (update-counters s n t)
  (base-state-update s
                     [dt #:parent State (get-dt s t) ]
                     [n #:parent State n]
                     [t #:parent State t]))

(: get-dt : (State Flonum -> Flonum))
(define (get-dt s t)
  (- t (State-t s)))

(: update-key-pressed : State String Boolean -> State)
(define (update-key-pressed s key pressed?)
  (cond
    [(string-prefix? key "wheel-") s]
    [else
     (base-state-update
      s
      [pressed #:parent State
               (cond
                 [pressed? (set-add (State-pressed s) key)]
                 [else (set-remove (State-pressed s) key)])])]))

(: set-player-position : State-Play Flonum -> State-Play)
(define (set-player-position s y)
  (struct-copy
   State-Play s
   [player (struct-copy
            Player (State-Play-player s)
            [y (clamp-bumper-y y)])]))

;; CALCULATION UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: clamp : Flonum Flonum Flonum -> Flonum)
(define (clamp x low high)
  (cond
    [(< high low) (error "low must be less than high")]
    [else (max low (min high x))]))

(: clamp-bumper-y : Flonum -> Flonum)
(define (clamp-bumper-y y)
  (clamp y
         (+ (- 0.0 WALL-Y) (* 1.0 (dir-dy BUMPER-SCALE)))
         (- WALL-Y (* 1.0 (dir-dy BUMPER-SCALE)))))

; Get the y-offset between the center a bumper and a ball
(: get-contact-offset-y : State-Play Flonum -> Flonum)
(define (get-contact-offset-y s y)
  (- (pos-y (Ball-pos (State-Play-ball s))) y))

(: get-new-player-score : Player Nonnegative-Integer -> Nonnegative-Integer)
(define (get-new-player-score player n)
  (+ (Player-score player)
     (max 0 (fl->exact-integer (ceiling (* (exact->inexact n)
                                 (Player-score-multiplier player)))))))

(: get-number-place : Integer Integer Integer -> Integer)
(define (get-number-place n over under)
  (exact-floor (/ (exact->inexact (- (modulo n over) (modulo n under)))
                  (exact->inexact under))))

(: digits : (->* (Nonnegative-Integer) (Nonnegative-Integer) (Listof Nonnegative-Integer)))
(define (digits n [radix 10])
  (define-values (q r) (quotient/remainder n radix))
  (cons r (if (= 0 q)
              null
              (digits q radix))))

(: random-0-1 : -> Flonum)
(define (random-0-1)
  (let ([result (/ (exact->inexact (random 4294967087)) 4294967086.0)])
    (cond [(flonum? result) result]
          [else (error "random did not return flonum")])))

(: within? : Flonum Flonum Flonum -> Boolean)
(define (within? x low high)
  (cond
    [(< high low) (error "low must be less than high")]
    [else (and (<= x high)
               (>= x low))]))

;; EVENT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: on-mouse : State Natural Flonum Integer Integer String -> State)
(define (on-mouse s n t x y e)
  (cond [State-Play? s (on-mouse-game-play s n t x y e)]
        [else s]))
  
(: on-mouse-game-play : State Natural Flonum Integer Integer String -> State)
(define (on-mouse-game-play s n t x y e)
  (cond [(State-Play? s)
         (set-player-position
          s
          (- (* 1.6 (/ (exact->inexact x) SCREEN-WIDTH)) 0.8))]
        [else s]))

; Game is never hard-paused, we manage pause state separately and just don't run
; the simulation based on the game field of State.
(: pause-state? : State Natural Flonum -> Boolean)
(define (pause-state? s n t) #f)

;; EVENT HANDLERS — ON-FRAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: on-frame : State Natural Flonum -> State)
(define (on-frame s n t)
  ((compose-n
    (λ ([s : State]) (update-counters s n t))
    on-frame-game-play)
   s))

;; EVENT HANDLERS — ON-FRAME-GAME-PAUSED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: on-frame-game-paused : State -> State)
(define (on-frame-game-paused s)
  (cond [(State-Paused? s) s]
        [else s]))

;; EVENT HANDLERS — ON-FRAME-GAME-PLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: on-frame-game-play : State -> State)
(define (on-frame-game-play s)
  (cond [(State-Play? s)
          (on-frame-game-play-endgame
           ((compose-n
             on-frame-game-play-ball
             on-frame-game-play-opponent
             on-frame-game-play-player-position
             on-frame-game-play-lives)
            s))]
        [else s]))

(: on-frame-game-play-ball : State-Play -> State-Play)
(define (on-frame-game-play-ball s)
  ((compose1
    on-frame-game-play-ball-position
    on-frame-game-play-ball-direction) s))

(: on-frame-game-play-ball-collision-bumper : State-Play -> State-Play)
(define (on-frame-game-play-ball-collision-bumper s)
  (let ([player (State-Play-player s)]
        [opponent (State-Play-opponent s)]
        [ball (State-Play-ball s)])
    (cond
      ; opponent collision
      [(and (negative? (dir-dx (Ball-dir ball)))
            (within? (+ (pos-x (Ball-pos ball)) CONTACT-BUFFER)
                     (+ OPPONENT-X CONTACT-BUFFER)
                     (+ OPPONENT-X (* CONTACT-BUFFER 2)))
            (within? (get-contact-offset-y s (Opponent-y opponent))
                     (- (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        State-Play s
        [ball (struct-copy
               Ball ball
               [dir
                (dir-scale
                 (dir-reflect (Ball-dir ball)
                              (angles->dir
                               (* (- 180 REDIRECT-FACTOR)
                                  (get-contact-offset-y s (Opponent-y opponent)))
                               0))
                 BALL-ACCELERATION-PADDLE)])])]
      ; player collision
      [(and (positive? (dir-dx (Ball-dir ball)))
            (within? (+ (pos-x (Ball-pos ball)) CONTACT-BUFFER)
                     (- PLAYER-X CONTACT-BUFFER)
                     PLAYER-X)
            (within? (get-contact-offset-y s (Player-y player))
                     (- (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        State-Play s
        [ball
         (struct-copy
          Ball ball
          [dir (dir-scale (dir-reflect
                           (Ball-dir ball)
                           (angles->dir
                            (* (- REDIRECT-FACTOR)
                               (get-contact-offset-y s (Player-y player)))
                            0))
                          BALL-ACCELERATION-PADDLE)])]
        [player
         (struct-copy
          Player player
          [score (get-new-player-score player 1)])])]
      [else s])))

(: on-frame-game-play-ball-collision-wall : State-Play -> State-Play)
(define (on-frame-game-play-ball-collision-wall s)
  (let ([plr (State-Play-player s)]
        [ball (State-Play-ball s)])
    (cond
      ; left wall collision
      [(and (negative? (dir-dy (Ball-dir ball)))
            (< (pos-y (Ball-pos ball)) (- (- WALL-Y BALL-RADIUS))))
       (struct-copy
        State-Play s
        [ball (struct-copy
               Ball ball
               [dir (dir-scale
                     (dir-reflect (Ball-dir ball) +y)
                     BALL-ACCELERATION-WALL)])])]
      ; right wall collision
      [(and (positive? (dir-dy (Ball-dir ball)))
            (> (pos-y (Ball-pos ball)) (- WALL-Y BALL-RADIUS)))
       (struct-copy
        State-Play s
        [ball (struct-copy
               Ball ball
               [dir (dir-scale
                     (dir-reflect (Ball-dir ball) +y)
                     BALL-ACCELERATION-WALL)])])]
      [else s])))

(: on-frame-game-play-ball-direction : State-Play -> State-Play)
(define (on-frame-game-play-ball-direction s)
  ((compose1
    on-frame-game-play-ball-collision-bumper
    on-frame-game-play-ball-collision-wall) s))

(: on-frame-game-play-ball-position : State-Play -> State-Play)
(define (on-frame-game-play-ball-position s)
  (let ([ball (State-Play-ball s)])
    (struct-copy
     State-Play s
     [ball (struct-copy
            Ball ball
            [pos
             (pos+ (Ball-pos ball)
                   (dir-scale (Ball-dir ball)
                              (* (State-dt s) BALL-SPEED)))])])))

(: on-frame-game-play-endgame : State-Play -> (U State-Game-Over State-Play))
(define (on-frame-game-play-endgame s)
  (cond [(= 0 (Player-lives (State-Play-player s)))
         (state-transition State-Game-Over s s)]
        [else s]))

(: on-frame-game-play-opponent : State-Play -> State-Play)
(define (on-frame-game-play-opponent s)
  (let ([ball (State-Play-ball s)]
        [opponent (State-Play-opponent s)])
    (let ([aim-buffer (/ BUMPER-CONTACT-WIDTH 8)]
          [pos-desired (pos-y (Ball-pos ball))])
      (let ([pos-diff (- pos-desired (Opponent-y opponent))])
        (struct-copy
         State-Play s
         [opponent (struct-copy
                    Opponent opponent
                    [y (clamp-bumper-y (+ (Opponent-y opponent)
                          (cond
                            [(within?
                              (pos-y (Ball-pos ball))
                              (- (Opponent-y opponent) aim-buffer)
                              (+ (Opponent-y opponent) aim-buffer))
                             (* OPPONENT-SPEED
                                (State-dt s)
                                pos-diff)]
                            [else
                             (* OPPONENT-SPEED
                                (State-dt s)
                                (flsgn pos-diff))])))])])))))

(: on-frame-game-play-player-position : State-Play -> State-Play)
(define (on-frame-game-play-player-position s)
  (let ([player (State-Play-player s)]
        [pressed (State-pressed s)])
    (cond
      [(set-member? pressed "left")
       (set-player-position
        s
        (+ (Player-y player) (* (State-dt s) -1/512)))]
      [(set-member? pressed "right")
       (set-player-position
        s
        (+ (Player-y player) (* (State-dt s)  1/512)))]
      [else s])))

(: on-frame-game-play-lives : State-Play -> State-Play)
(define (on-frame-game-play-lives s)
  (let ([ball (State-Play-ball s)]
        [player (State-Play-player s)])
    (cond [(< (pos-x (Ball-pos ball)) OPPONENT-BOUNDS)
           (struct-copy
            State-Play s
            [ball (state-start-game-play-ball)]
            [player
             (struct-copy
              Player player
              [score (get-new-player-score player 10)])])]
          [(> (pos-x (Ball-pos ball)) PLAYER-BOUNDS)
           (struct-copy
            State-Play s
            [ball (state-start-game-play-ball)]
            [player
             (struct-copy
              Player player
              [lives (max 0 (sub1 (Player-lives player)))])])]
          [else s])))

;; EVENT HANDLERS — ON-KEY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: on-key : State Natural Flonum String -> State)
(define (on-key s n t k)
  (update-key-pressed
   (on-key-immediate s n t k)
   k #t))

(: on-key-immediate : State Natural Flonum String -> State)
; Immediate reactions to keypresses
(define (on-key-immediate s n t k)
  (cond
    [(string=? k "Escape")
     (cond [(State-Play? s) (struct-copy State-Paused s)]
           [(State-Paused? s) (State-Paused-resume-state s)]
           [else s])]
    [(string=? k "r")
     (state-reset s n t)]
    [else s]))

(: on-release : State Natural Flonum String -> State)
(define (on-release s n t k)
  (update-key-pressed s k #f))

;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; GLOBAL SETTINGS

(current-material (material #:ambient 0
                            #:diffuse 0
                            #:specular 0
                            #:roughness 0.3))

;; RENDER — CONSTANTS

(define COLOR-OPPONENT-EMITTED (emitted 100 60 10 0.03))
(define COLOR-PLAYER-EMITTED (emitted "plum" 2))

(: arena-bumper Pict3D)
(define arena-bumper
  (combine (with-emitted (emitted "oldlace" 2) (cylinder origin 1))
           (for/list : (Listof Pict3D) ([z (in-range 0 10)])
             (light (pos 0.0 0.0 (/ (exact->inexact z) 9.0))
                    (emitted "oldlace" 4.0)))))

(: axes Pict3D)
(define axes
  (combine (with-emitted (emitted "cyan" 2)
             (arrow origin -x))
           (with-emitted (emitted "magenta" 2)
             (arrow origin -y))
           (with-emitted (emitted "yellow" 2)
             (arrow origin -z))))

;; RENDER — CAMERA

(: camera : State -> Pict3D)
(define (camera s)
  (basis 'camera (camera-transform s)))

(: camera-dir : -> Dir)
(define (camera-dir)
  (define normalized (dir-normalize (pos- origin (camera-pos))))
  (cond [normalized normalized]
        [else (error "normalized not normal")]))

(: camera-point-at : -> Affine)
(define (camera-point-at) (point-at (camera-pos) CAMERA-LOOK-AT))

(: camera-pos : -> Pos)
(define (camera-pos)
  (define ar (aspect-ratio))
  (pos (+ 2.0 (* ar -0.3))   ; distance
       0.0                   ; side-to-side
       (+ 1.0 (* ar -0.2)))) ; elevation

(: camera-transform : State -> Affine)
(define (camera-transform s)
  (camera-point-at))

; RENDER — ON-DRAW

(: on-draw : State Natural Flonum -> Pict3D)
(define (on-draw s n t)
  (combine (render-game-play s)
           (render-game-over s)))

; RENDER FUNCTIONS

; Scale a [-1,1] value up to a range of the given width.
(: scale--1-1 : Flonum Integer -> Flonum)
(define (scale--1-1 n width)
  (let ([half-width (/ (exact->inexact width) 2.0)])
  (+ (* n half-width) half-width)))

; wrap-within
(: wrap-within : Flonum Flonum -> Flonum)
(define (wrap-within n width)
  (cond [(negative? width) (error "negative width")]
        [(or (> n width) (< n (- width))) (error "n outside screen bounds")]
        [(negative? n) (+ width n)]
        [else n]))

; Get a transformation that moves an object from the origin to the given (x,y,z)
; coordinate in screen-space, where (0,0,1) is the top-left of the camera
; viewport and (SCREEN-WIDTH,SCREEN-HEIGHT,1) is the bottom-right, placed in a
; z-plane 1 unit away from the camera.
(: position-screen-space-pixels : (->* (State Flonum Flonum) (Flonum) Affine))
(define (position-screen-space-pixels s x y [z 1.0])
  (define cam-t (camera-transform s))
  (define z-near (* z CAMERA-SPACE-DISTANCE))
  (define dir ((camera-ray-dir cam-t
                               #:width SCREEN-WIDTH
                               #:height SCREEN-HEIGHT
                               #:z-near z-near)
               (wrap-within x SCREEN-WIDTH-INEXACT)
               (wrap-within y SCREEN-HEIGHT-INEXACT)))
  (affine-compose (move dir)
                  cam-t
                  (scale z-near)))

; Get a transformation that moves an object from the origin to the given (x,y,z)
; coordinate in screen-space, where (-1,-1,1) is the top-left of the camera
; viewport, and (1,1,1) is the bottom-right, placed in a z-plane 1 unit away
; from the camera.
(: position-screen-space-relative : (->* (State Flonum Flonum) (Flonum) Affine))
(define (position-screen-space-relative s x y [z 1.0])
  (position-screen-space-pixels
   s
   (scale--1-1 x SCREEN-WIDTH)
   (scale--1-1 y SCREEN-HEIGHT)
   z))

; RENDER FUNCTIONS — GAME OVER

(: render-game-over : State -> Pict3D)
(define (render-game-over s)
  (cond
    [(State-Game-Over? s)
     (combine
      (camera s)
      (render-game-over-message s)
      (render-game-over-score s))]
    [else empty-pict3d]))

(: render-game-over-message : State-Game-Over -> Pict3D)
(define (render-game-over-message s)
  (transform (with-emitted (emitted "red" 2.0)
                         (rotate-z (move-z (cube origin 0.5) 1.0) 45))
             (position-screen-space-relative s 0.0 0.0)))

(: render-game-over-score : State-Game-Over -> Pict3D)
(define (render-game-over-score s)
  (transform (render-player-score
              (Player-score
               (State-Play-player
                (State-Game-Over-end-state s))))
             (position-screen-space-relative s 0.0 0.0)))

; RENDER FUNCTIONS — GAME-PLAY

(: render-game-play : State -> Pict3D)
(define (render-game-play s)
  (cond
    [(State-Play? s)
     (combine
      (render-game-play-opponent s)
      (render-game-play-player s)
      (render-game-play-ball s)
      (render-game-play-hud s)
      (render-game-play-lights+camera s)
      (render-game-play-arena s)
      (render-game-play-arena-bumpers s))]
    [else empty-pict3d]))

(: render-game-play-arena : State-Play -> Pict3D)
(define (render-game-play-arena s)
  (parameterize
      ([current-material (material #:ambient 0.01
                                   #:diffuse 0.15
                                   #:specular 0.3
                                   #:roughness 0.3)]
       [current-color (rgba 0.6 0.6 0.6)])
    (transform
     ; tunnel
     (combine (cylinder origin 1
                        #:arc (arc 180 360)
                        #:inside? #t
                        #:top-cap? #f
                        #:bottom-cap? #f)
              ; end of the tunnel
              (transform
               (light origin (emitted "goldenrod" 0.0001))
               (affine-compose
                (move-y -0.5)
                (move-z -0.99)
                (scale 40)))
              (with-emitted
                  (emitted 0 0 0.01 20)
                (cylinder origin 1
                          #:arc (arc 180 360)
                          #:inside? #t
                          #:top-cap? #f
                          #:bottom-cap? #t
                          #:start-cap? #f
                          #:end-cap? #f
                          #:outer-wall? #f)))
     (affine-compose (scale-x 10)
                     (move-z -0.1)
                     (rotate-x -90)
                     (rotate-y 90)))))

(: render-game-play-arena-bumpers : State-Play -> Pict3D)
(define (render-game-play-arena-bumpers s)
  (combine
   (transform arena-bumper
              (affine-compose
               (move-y (+ WALL-Y BALL-RADIUS))
               (scale (dir 10 1/256 1/256))))
   (transform arena-bumper
              (affine-compose
               (move-y (- 0.0 WALL-Y BALL-RADIUS))
               (scale (dir 10 1/256 1/256))))))

(: render-game-play-ball : State-Play -> Pict3D)
(define (render-game-play-ball s)
  (let ([ball (State-Play-ball s)])
    (combine
     (light
      (Ball-pos ball)
      (emitted "oldlace" 0.1)
      #:range 1)
     (with-emitted (emitted "oldlace" 1.5)
       (sphere (Ball-pos ball) BALL-RADIUS)))))

(: render-game-play-hud : State-Play -> Pict3D)
(define (render-game-play-hud s)
  (define player (State-Play-player s))
  ; player score
  (combine
   (transform (render-player-score (Player-score player))
              (position-screen-space-pixels s 100.0 100.0))
   ; player lives
   (transform
    (parameterize ([current-emitted COLOR-PLAYER-EMITTED])
      (combine
       (for/list : (Listof Pict3D)
         ([n (range 0 (Player-lives player))])
         (cube (pos (* (exact->inexact n) -0.08) 0 0) 0.02))))
    (position-screen-space-pixels s -100.0 100.0))))

(: render-game-play-opponent : State-Play -> Pict3D)
(define (render-game-play-opponent s)
  (parameterize
      ([current-material (material #:ambient 0
                                   #:diffuse 0
                                   #:specular 0
                                   #:roughness 0.3)]
       [current-emitted COLOR-OPPONENT-EMITTED])
    (rectangle (pos OPPONENT-X (Opponent-y (State-Play-opponent s)) 0)
               BUMPER-SCALE)))

(: render-game-play-player : State-Play -> Pict3D)
(define (render-game-play-player s)
  (parameterize
      ([current-material (material #:ambient 0
                                   #:diffuse 0
                                   #:specular 0.6
                                   #:roughness 0.3)]
       [current-emitted COLOR-PLAYER-EMITTED])
    (rectangle (pos PLAYER-X (Player-y (State-Play-player s)) 0)
               BUMPER-SCALE)))

(: render-game-play-lights+camera : State-Play -> Pict3D)
(define (render-game-play-lights+camera s)
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (camera s)))

(: render-player-score : Nonnegative-Integer -> Pict3D)
(define (render-player-score score)
  (combine
   ; ones
   (parameterize ([current-emitted (emitted 1 1 1 2)])
     (combine
      (for/list : (Listof Pict3D)
        ([n (range 0 (get-number-place score 10 1))])
        (sphere (pos (* n 0.03) 0.0 0.0) 0.01))))
   ; tens
   (parameterize ([current-emitted (emitted 0.5 0.7 1 2)])
     (combine
      (for/list : (Listof Pict3D)
        ([n (range 0.0 (get-number-place score 100 10))])
        (sphere (pos (* n 0.03) 0.03 0.0) 0.01))))
   ; hundreds
   (parameterize ([current-emitted (emitted 1 0.8 0 1.5)])
     (combine
      (for/list : (Listof Pict3D)
        ([n (range 0.0 (get-number-place score 1000 100))])
        (sphere (pos (* n 0.03) 0.06 0.0) 0.01))))
   ; thousands
   (parameterize ([current-emitted (emitted 0.6 0 0.8 1.5)])
     (combine
      (for/list : (Listof Pict3D)
        ([n (range 0.0 (get-number-place score 10000 1000))])
        (sphere (pos (* n 0.03) 0.09 0.0) 0.01))))
   ))

;; VALIDATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: valid-state? : State Natural Flonum -> Boolean)
(define (valid-state? s n t)
  (or (State-Game-Over? s)
      (State-Main-Menu? s)
      (State-Paused? s)
      (State-Play? s)))

;; BANGIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: state-reset : State Natural Flonum -> State-Play)
(define (state-reset s n t)
  (struct-copy
   State-Play (state-start t)
   [dt #:parent State (State-dt s)]
   [n  #:parent State (State-n s)]
   [t  #:parent State (State-t s)]))

(: state-start : (->* () (Flonum) State))
(define (state-start [t 0.0])
  (State-Play
   ; State
   0.0   ; dt
   0     ; n
   (set) ; pressed
   t     ; t

   ; State-Play
   (state-start-game-play-ball) ; ball
   (Opponent 0.0) ; y           ; opponent
   (Player 3    ; lives         ; player
           0    ; score
           1.0  ; score-multiplier
           0.0) ; y
   t))   ; start-t

(define (state-start-game-play-ball)
  (Ball (dir -1.0 (* 1.5 (- 0.5 (random-0-1))) 0.0)
        (pos 0.0 0.0 0.0)))

(big-bang3d
 (state-start)
 #:frame-delay FRAME-DELAY-MILLIS
 #:name "Pong3D — Racket"
 #:on-draw on-draw
 #:on-frame on-frame
 #:on-key on-key
 #:on-mouse on-mouse
 #:on-release on-release
 #:pause-state? pause-state?
 #:valid-state? valid-state?
 #:width SCREEN-WIDTH
 #:height SCREEN-HEIGHT)

;; WINDOWING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
