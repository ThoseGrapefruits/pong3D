#lang typed/racket

(require
  pict3d
  pict3d/universe
  racket/set
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

(define BALL-ACCELERATION 0.02)
(define BALL-RADIUS 3/128)
(define BALL-SPEED 0.001)
(define BUMPER-SCALE (dir 1/64 7/64 1/32))
(define BUMPER-CONTACT-WIDTH (+ (dir-dy BUMPER-SCALE) BALL-RADIUS))
(define BOUNDS-BUMPER-GAP (* 10 (dir-dx BUMPER-SCALE)))
(define CONTACT-BUFFER (+ BALL-RADIUS (dir-dx BUMPER-SCALE)))
(define FOV 120)
(define OPPONENT-SPEED 0.001)
(define OPPONENT-X -1.0)
(define OPPONENT-BOUNDS (- OPPONENT-X BOUNDS-BUMPER-GAP))
(define PLAYER-X 1.0)
(define PLAYER-BOUNDS (+ PLAYER-X BOUNDS-BUMPER-GAP))
(define SCREEN-WIDTH 1200)
(define SCREEN-HEIGHT 1080)
(define SPIN-FACTOR 40.0)
(define WALL-Y 0.8)
(define ASPECT-RATIO (/ SCREEN-WIDTH SCREEN-HEIGHT))
(define CAMERA-X (+ 0.5 (/ 1.0 ASPECT-RATIO)))
(define CAMERA-Z (/ (+ 0.5 (/ 1.0 ASPECT-RATIO)) 2.0))
(define CAMERA-LOOK-AT origin)
(define CAMERA-POS (pos CAMERA-X 0 CAMERA-Z))

;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Ball
  ([dir : Dir]
   [pos : Pos]))

(struct Opponent
  ([lives : Nonnegative-Integer]
   [y : Flonum ]))

(struct Player
  ([lives : Nonnegative-Integer]
   [y : Flonum]))

(struct State
  ([dt   : Flonum]
   [n    : Natural]
   [pressed : (Setof String)]
   [t    : Flonum])
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
   [player : Player])
  #:transparent)

;; STATE UPDATERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: get-state-constructor (All (S) (-> (∩ S State) Any)))
(define (get-state-constructor s)
  (cond [(State-Main-Menu? s) State-Main-Menu]
        [(State-Paused? s) State-Paused]
        [(State-Play? s) State-Play]
        [else (error (~s "Invalid state" s))]))

;;
;;
;; nope
;;
;; can't do what I want because of methods like the below, unless I have a switch
;; for every type of state
;;
;;

(: update-counters : State Natural Flonum -> State)
(define (update-counters s n t)
  (cond [(State-Main-Menu? s)
         (struct-copy
          State-Main-Menu s
          [dt #:parent State (get-dt s n t) ]
          [n #:parent State n]
          [t #:parent State t])]
        [(State-Paused? s)
         (struct-copy
          State-Paused s
          [dt #:parent State (get-dt s n t)]
          [n #:parent State n]
          [t #:parent State t])]
        [(State-Play? s)
         (struct-copy
          State-Play s
          [dt #:parent State (get-dt s n t)]
          [n #:parent State n]
          [t #:parent State t])]
        [else (error (~s "Invalid state" s))]))

(: get-dt : (State Natural Flonum -> Flonum))
(define (get-dt s n t)
  (- t (State-t s)))

(: update-key-pressed : State String Boolean -> State)
(define (update-key-pressed s key pressed?)
  (struct-copy
   State s
   [pressed
    (cond
      [pressed? (set-add (State-pressed s) key)]
      [else (set-remove (State-pressed s) key)])]))

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
         (+ (- 0 WALL-Y) (* 1.0 (dir-dy BUMPER-SCALE)))
         (- WALL-Y (* 1.0 (dir-dy BUMPER-SCALE)))))

; Get the y-offset between the center a bumper and a ball
(: get-contact-offset-y : State-Play Flonum -> Flonum)
(define (get-contact-offset-y s y)
  (- (pos-y (Ball-pos (State-Play-ball s))) y))

(: get-ball-acceleration : State-Play -> Flonum)
(define (get-ball-acceleration s)
  (+ 1.0 BALL-ACCELERATION))

(: sign : Flonum -> (U -1.0 1.0))
(define (sign n)
  (cond [(< n 0.0) -1.0]
        [else 1.0]))

(: within? : Flonum Flonum Flonum -> Boolean)
(define (within? x low high)
  (cond
    [(< high low) (error "low must be less than high")]
    [else (and (<= x high)
               (>= x low))]))

;; EVENT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: on-mouse : State Natural Flonum Integer Integer Any -> State)
(define (on-mouse s n t x y e)
  (cond [State-Play? s (on-mouse-game-play s n t x y e)]
        [else s]))
  
(: on-mouse-game-play : State Natural Flonum Integer Integer Any -> State)
(define (on-mouse-game-play s n t x y e)
  (cond [(State-Play? s) (set-player-position s (- (* 1.6 (/ x SCREEN-WIDTH)) 0.8))]
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
         ((compose-n
           on-frame-game-play-lives
           on-frame-game-play-endgame
           on-frame-game-play-ball
           on-frame-game-play-opponent
           on-frame-game-play-player-position)
          s)]
        [else s]))

(: on-frame-game-play-ball : State-Play -> State-Play)
(define (on-frame-game-play-ball s)
  ((compose1
    on-frame-game-play-ball-direction
    on-frame-game-play-ball-position) s))

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
                               (* (- 180 SPIN-FACTOR)
                                  (get-contact-offset-y s (Opponent-y opponent)))
                               0))
                 (get-ball-acceleration s))])])]
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
                            (* (- SPIN-FACTOR)
                               (get-contact-offset-y s (Player-y player)))
                            0))
                          (get-ball-acceleration s))])])]
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
                     (get-ball-acceleration s))])])]
      ; right wall collision
      [(and (positive? (dir-dy (Ball-dir ball)))
            (> (pos-y (Ball-pos ball)) (- WALL-Y BALL-RADIUS)))
       (struct-copy
        State-Play s
        [ball (struct-copy
               Ball ball
               [dir (dir-scale
                     (dir-reflect (Ball-dir ball) +y)
                     (get-ball-acceleration s))])])]
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

(: on-frame-game-play-endgame : State-Play -> State-Play)
(define (on-frame-game-play-endgame s) s)

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
                    [y (+ (Opponent-y opponent)
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
                                (sign pos-diff))]))])])))))

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
        [opponent (State-Play-opponent s)])
    (cond [(< (pos-x (Ball-pos ball)) OPPONENT-BOUNDS)
           (struct-copy
            State-Play s
            [ball (state-start-game-play-ball)]
            [opponent
             (struct-copy
              Opponent opponent
              [lives (max 0 (sub1 (Opponent-lives (State-Play-opponent s))))])])]
          [(> (pos-x (Ball-pos ball)) PLAYER-BOUNDS)
           (struct-copy
            State-Play s
            [ball (state-start-game-play-ball)]
            [player
             (struct-copy
              Player (State-Play-player s)
              [lives (max 0 (sub1 (Player-lives (State-Play-player s))))])])]
          [else s])))

;; EVENT HANDLERS — ON-KEY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: on-key : State Natural Flonum String -> State)
(define (on-key s n t k)
  (update-key-pressed
   (on-key-immediate s k)
   k #t))

(: on-key-immediate : State String -> State)
; Immediate reactions to keypresses
(define (on-key-immediate s k)
  (cond
    [(string=? k "Escape")
     (cond [(State-Play? s) (struct-copy State-Paused s)]
           [(State-Paused? s) (State-Paused-resume-state s)]
           [else s])]
    [(string=? k "r")
     (state-reset s)]
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

; CONSTANTS

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

; ON-DRAW

(: on-draw : State Natural Flonum -> Pict3D)
(define (on-draw s n t)
  (combine (render-game-play s)))

; RENDER FUNCTIONS

; Get a transformation that moves an object from the origin to the given (x,y,z)
; coordinate in screen-space, where (0,0,1) is the top-left of the camera
; viewport, placed in a z-plane 1 unit away from the camera.

;; WIP
(: position-screen-space : Flonum Flonum Flonum -> Affine)
(define (position-screen-space x y z)
 (affine-compose (move (pos- CAMERA-POS origin))))

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
                                  #:diffuse 0.1
                                  #:specular 0.6
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
  (let ([opponent (State-Play-opponent s)]
        [player (State-Play-player s)])
    (combine
     ; opponent
     (parameterize ([current-emitted COLOR-OPPONENT-EMITTED])
       (combine
        (for/list : (Listof Pict3D)
          ([n (range 0 (Opponent-lives opponent))])
          (cube (pos 0.5 (+ 0.5 (* (exact->inexact n) 0.08)) 0.4) 0.02))))
     ; player
     (parameterize ([current-emitted COLOR-PLAYER-EMITTED])
       (combine
        (for/list : (Listof Pict3D)
          ([n (range 0 (Player-lives player))])
          (cube (pos 0.5 (+ 0.5 (* (exact->inexact n) 0.08)) 0.2) 0.02)))))))

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
           (basis 'camera (point-at CAMERA-POS CAMERA-LOOK-AT))))

;; VALIDATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: valid-state? : State Natural Flonum -> Boolean)
(define (valid-state? s n t)
  (or (State-Main-Menu? s)
      (State-Paused? s)
      (State-Play? s)))

;; BANGIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: random-0-1 : -> Flonum)
(define (random-0-1)
  (let ([result (/ (exact->inexact (random 4294967087)) 4294967086.0)])
    (cond [(flonum? result) result]
          [else (error "random did not return flonum")])))

(: state-reset : State -> State)
(define (state-reset s)
  (struct-copy
   State-Play (state-start)
   [dt #:parent State (State-dt s)]
   [n  #:parent State (State-n s)]
   [t  #:parent State (State-t s)]))

(: state-start : -> State)
(define (state-start)
  (State-Play
   ; State
   0.0   ; dt
   0     ; n
   (set) ; pressed
   0.0   ; t

   ; State-Play
   (state-start-game-play-ball) ; ball
   (Opponent 3 0.0)             ; opponent
   (Player 3 0.0)))             ; player

(define (state-start-game-play-ball)
  (Ball (dir -1.0 (* 1.5 (- 0.5 (random-0-1))) 0.0)
               (pos 0.0 0.0 0.0)))

(big-bang3d
   (state-start)
   #:frame-delay (max 1.0 (abs (/ 1000.0 59.9))) ; take that type checker lol
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

;; TASKS & IDEAS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STATE MACHINE
;; Turn the majority of the State object into a (U ...) of possible states (e.g.
;; playing vs pause menu vs main menu, etc