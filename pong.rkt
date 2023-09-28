#lang typed/racket

(require
  racket/flonum
  pict3d
  pict3d/universe
  racket/set
  typed-compose)

;
;                                  ═══
;
;     +                               
;    /                             ╭─╮
;   x                              ╰─╯
;  /                                                                        +
; -                                                                         |
;                               ╓───────╖                                   z 
;                               ╙───────╜                                   |
;                             - ——— y ——— +                                 -

;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BALL-ACCELERATION 0.02)
(define BALL-RADIUS 3/128)
(define BALL-SPEED 0.001)
(define BUMPER-SCALE (dir 1/64 7/64 1/32))
(define BUMPER-CONTACT-WIDTH (+ (dir-dy BUMPER-SCALE) BALL-RADIUS))
(define CONTACT-BUFFER (+ BALL-RADIUS (dir-dx BUMPER-SCALE)))
(define OPPONENT-SPEED 0.001)
(define OPPONENT-X -1.0)
(define PLAYER-X 1.0)
(define SCREEN-WIDTH 1200)
(define SCREEN-HEIGHT 1080)
(define SPIN-FACTOR 40.0)
(define WALL-Y 0.8)
(define ASPECT-RATIO (/ SCREEN-WIDTH SCREEN-HEIGHT))
(define CAMERA-X (+ 0.5 (/ 1.0 ASPECT-RATIO)))
(define CAMERA-Y (/ (+ 0.5 (/ 1.0 ASPECT-RATIO)) 2.0))

;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct Ball ([direction : Dir] [position : Pos]))

(struct Opponent ([y : Flonum ]))

(struct Player ([pressed : (Setof String)] [y : Flonum]))

(struct State ([ball : Ball]
               [dt : Flonum]
               [n : Natural]
               [opponent : Opponent]
               [pause-next-frame? : Boolean]
               [paused? : Boolean]
               [player : Player]
               [t : Flonum]))

;; STATE UPDATERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: update-counters : State Natural Flonum -> State)
(define (update-counters s n t)
  (struct-copy State (update-dt s n t) [n n] [t t]))

(: update-dt : State Natural Flonum -> State)
(define (update-dt s n t)
  (struct-copy State s [dt (- t (State-t s))]))

(: update-key-pressed : State String Boolean -> State)
(define (update-key-pressed s key pressed?)
  (struct-copy
   State s
   [player
    (struct-copy
     Player (State-player s)
     [pressed
      (cond
        [pressed? (set-add (Player-pressed (State-player s)) key)]
        [else (set-remove (Player-pressed (State-player s)) key)])])]))

(: set-player-position : State Flonum -> State)
(define (set-player-position s y)
  (struct-copy
   State s
   [player (struct-copy
            Player (State-player s)
            [y (clamp-bumper-y y)])]))

;; CALCULATION UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: clamp : Flonum Flonum Flonum -> Flonum)
(define (clamp x low high)
  (cond
    [(< high low) (error "low must be less than high")]
    [else (max low (min high x))]))

(: clamp-bumper-y : Flonum -> Flonum)
(define (clamp-bumper-y y)
  (clamp y (+ -1.0 (* 3.0 (dir-dy BUMPER-SCALE))) (- 1.0 (* 3.0 (dir-dy BUMPER-SCALE)))))

; Get the y-offset between the center a bumper and a ball
(: get-contact-offset-y : State Flonum -> Flonum)
(define (get-contact-offset-y s y)
    (- (pos-y (Ball-position (State-ball s))) y))

(: get-ball-acceleration : State -> Flonum)
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

(: on-frame : State Natural Flonum -> State)
(define (on-frame s n t)
  ((compose-n
    (λ ([s : State]) (update-counters s n t))
    on-frame-pause
    on-frame-ball
    on-frame-opponent
    on-frame-player-position)
   s))

(: on-mouse : State Natural Flonum Integer Integer Any -> State)
(define (on-mouse s n t x y e)
  (set-player-position s (- (* 1.6 (/ x SCREEN-WIDTH)) 0.8)))

(: pause-state? : State Natural Flonum -> Boolean)
(define (pause-state? s n t)
  (State-paused? s))

;; EVENT HANDLERS — ON-FRAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: on-frame-ball : State -> State)
(define (on-frame-ball s)
  ((compose1
    on-frame-ball-direction
    on-frame-ball-position) s))

(: on-frame-ball-collision-bumper : State -> State)
(define (on-frame-ball-collision-bumper s)
  (let ([plr (State-player s)]
        [opp (State-opponent s)]
        [bll (State-ball s)])
    (cond
      ; opponent collision
      [(and (< (dir-dx (Ball-direction bll)) 0)
            (within? (+ (pos-x (Ball-position bll)) CONTACT-BUFFER)
                     (+ OPPONENT-X CONTACT-BUFFER)
                     (+ OPPONENT-X (* CONTACT-BUFFER 2)))
            (within? (get-contact-offset-y s (Opponent-y opp))
                     (- 0 (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        State s
        [ball (struct-copy
               Ball (State-ball s)
               [direction
                (dir-scale
                 (dir-reflect (Ball-direction bll)
                              (angles->dir
                               (* (- 180 SPIN-FACTOR)
                                  (get-contact-offset-y s (Opponent-y opp)))
                               0))
                 (get-ball-acceleration s))])])]
      ; player collision
      [(and (> (dir-dx (Ball-direction bll)) 0)
            (within? (+ (pos-x (Ball-position bll)) CONTACT-BUFFER)
                     (- PLAYER-X CONTACT-BUFFER)
                     PLAYER-X)
            (within? (get-contact-offset-y s (Player-y plr))
                     (- 0 (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        State s
        [ball
         (struct-copy
          Ball (State-ball s)
          [direction
           (dir-scale
            (dir-reflect
             (Ball-direction bll)
             (angles->dir
              (* (- SPIN-FACTOR) (get-contact-offset-y s (Player-y plr)))
              0))
            (get-ball-acceleration s))])])]
        
      [else s])))

(: on-frame-ball-collision-wall : State -> State)
(define (on-frame-ball-collision-wall s)
  (let ([plr (State-player s)]
        [opp (State-opponent s)]
        [bll (State-ball s)])
    (cond
      ; left wall collision
      [(and (< (dir-dy (Ball-direction bll)) 0)
            (< (pos-y (Ball-position bll)) (- 0 (- WALL-Y BALL-RADIUS))))
       (struct-copy
        State s
        [ball (struct-copy
               Ball (State-ball s)
               [direction (dir-scale
                           (dir-reflect (Ball-direction bll) +y)
                           (get-ball-acceleration s))])])]
      ; right wall collision
      [(and (> (dir-dy (Ball-direction bll)) 0)
            (> (pos-y (Ball-position bll)) (- WALL-Y BALL-RADIUS)))
       (struct-copy
        State s
        [ball (struct-copy
               Ball (State-ball s)
               [direction (dir-scale
                           (dir-reflect (Ball-direction bll) +y)
                           (get-ball-acceleration s))])])]
      [else s])))

(: on-frame-ball-direction : State -> State)
(define (on-frame-ball-direction s)
  ((compose1
    on-frame-ball-collision-bumper
    on-frame-ball-collision-wall) s))


(: on-frame-ball-position : State -> State)
(define (on-frame-ball-position s)
    (struct-copy
     State s
     [ball (struct-copy
            Ball (State-ball s)
            [position
             (pos+ (Ball-position (State-ball s))
                   (dir-scale (Ball-direction (State-ball s))
                              (* (State-dt s) BALL-SPEED)))])]))

(: on-frame-opponent : State -> State)
(define (on-frame-opponent s)
  (let ([aim-buffer (/ BUMPER-CONTACT-WIDTH 8)]
        [position-desired (pos-y (Ball-position (State-ball s)))])
    (let ([position-diff (- position-desired (Opponent-y (State-opponent s)))])
      (struct-copy
       State s
       [opponent (struct-copy
                  Opponent (State-opponent s)
                  [y (+ (Opponent-y (State-opponent s))
                        (cond
                          [(within? (pos-y (Ball-position (State-ball s)))
                                   (- (Opponent-y (State-opponent s)) aim-buffer)
                                   (+ (Opponent-y (State-opponent s)) aim-buffer))
                           (* OPPONENT-SPEED
                              (State-dt s)
                              position-diff)]
                          [else
                           (* OPPONENT-SPEED
                              (State-dt s)
                              (sign position-diff))]))])]))))

(: on-frame-pause : State -> State)
(define (on-frame-pause s)
  (cond [(State-pause-next-frame? s) (struct-copy State s [paused? #t] [pause-next-frame? #f])]
        [else s]))

(: on-frame-player-position : State -> State)
(define (on-frame-player-position s)
  (let ([pressed (Player-pressed (State-player s))])
    (cond
      [(set-member? pressed "left")
       (set-player-position
        s
        (max (- 0 WALL-Y) (+ (Player-y (State-player s)) (* (State-dt s) -1/512))))]
      [(set-member? pressed "right")
       (set-player-position
        s
        (min WALL-Y       (+ (Player-y (State-player s)) (* (State-dt s)  1/512))))]
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
    [(string=? k " ") (struct-copy State s [paused? (not (State-paused? s))])]
    [(string=? k "r") (struct-copy State (state-reset s)
                                   [pause-next-frame? #t])]
    [else s]))

(: on-release : State Natural Flonum String -> State)
(define (on-release s n t k)
  (update-key-pressed s k #f))

;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; CONSTANTS

(current-material (material #:ambient 0
                            #:diffuse 0
                            #:specular 0
                            #:roughness 0.3))
(: arena-bumper Pict3D)
(define arena-bumper
  (combine (with-emitted (emitted "oldlace" 2) (cylinder origin 1))
           (for/list : (Listof Pict3D) ([z  (in-range 0 10)])
             (light (pos 0 0 (- (* 2 (/ z 9)) 1))
                    (emitted "oldlace" 1)))))

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
  (combine (render-opponent s)
           (render-player s)
           (render-ball s)
           (render-lights+camera s)
           (render-arena s)
           (render-arena-bumpers s)))

; RENDER FUNCTIONS

(: render-arena : State -> Pict3D)
(define (render-arena s)
  (parameterize
      ([current-material (material #:ambient 0.01
                                  #:diffuse 0.1
                                  #:specular 0.6
                                  #:roughness 0.3)]
       [current-color (rgba 0.6 0.6 0.6)])
    (transform 
     (cylinder origin 1
               #:arc (arc 180 360)
               #:inside? #t
               #:top-cap? #f
               #:bottom-cap? #f)
     (affine-compose (scale-x 10)
                     (move-z -0.1)
                     (rotate-x -90)
                     (rotate-y 90)))))

(: render-arena-bumpers : State -> Pict3D)
(define (render-arena-bumpers s)
  (combine
    (transform arena-bumper
      (affine-compose
       (move-y (+ WALL-Y BALL-RADIUS))
       (scale (dir 10 1/256 1/256))))
    (transform arena-bumper
      (affine-compose
       (move-y (- 0 WALL-Y BALL-RADIUS))
       (scale (dir 10 1/256 1/256))))))

(: render-ball : State -> Pict3D)
(define (render-ball s)
  (combine
   (light
    (Ball-position (State-ball s))
    (emitted "oldlace" 0.1)
    #:range 1)
   (with-emitted (emitted "oldlace" 1.5)
     (sphere (Ball-position (State-ball s)) BALL-RADIUS))))

(: render-opponent : State -> Pict3D)
(define (render-opponent s)
  (parameterize
      ([current-material (material #:ambient 0
                                  #:diffuse 0
                                  #:specular 0
                                  #:roughness 0.3)]
       [current-emitted (emitted 100 60 10 0.03)])
    (rectangle (pos OPPONENT-X (Opponent-y (State-opponent s)) 0)
               BUMPER-SCALE)))

(: render-player : State -> Pict3D)
(define (render-player s)
  (parameterize
      ([current-material (material #:ambient 0
                                  #:diffuse 0
                                  #:specular 0.6
                                  #:roughness 0.3)]
       [current-emitted (emitted "plum" 2)])
    (rectangle (pos PLAYER-X (Player-y (State-player s)) 0)
               BUMPER-SCALE)))

(: render-lights+camera : State -> Pict3D)
(define (render-lights+camera s)
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (basis 'camera (point-at (pos CAMERA-X 0 CAMERA-Y) origin))))

;; VALIDATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: valid-state? : State Natural Flonum -> Boolean)
(define (valid-state? s n t)
  (not (and (State-paused? s) (State-pause-next-frame? s))))

;; BANGIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: random-0-1 : -> Flonum)
(define (random-0-1)
  (let ([result (/ (random 4294967087) 4294967086.0)])
    (cond [(flonum? result) result]
          [else (error "random did not return flonum")])))

(: state-reset : State -> State)
(define (state-reset s)
  (struct-copy State (state-start)
               [dt (State-dt s)]
               [n  (State-n s)]
               [t  (State-t s)]))

(: state-start : -> State)
(define (state-start)
  (State (Ball (dir -1.0 (* 1.5 (- 0.5 (random-0-1))) 0.0) ; ball
               (pos 0.0 0.0 0.0))
         0.0                 ; dt
         0                   ; n
         (Opponent 0.0)      ; opponent
         #f                  ; pause-next-frame?
         #f                  ; paused?
         (Player (set) 0.0)  ; player
         0.0))               ; t

(big-bang3d
   (struct-copy State (state-start) [paused? #t])
   #:frame-delay (+ 0.00001 (abs (/ 1000.0 59.9))) ; take that type checker lol
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
