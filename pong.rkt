#lang racket

(require
  pict3d
  pict3d/universe
  racket/set)

;
;                                  ═══
;
;     ^                               
;    /                             ╭─╮
;   x                              ╰─╯
;  /                                                                        ^
; v                                                                         |
;                               ╓───────╖                                   z 
;                               ╙───────╜                                   |
;                             <=====y=====>                                 v

;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BALL-ACCELERATION 0.00015)
(define BALL-RADIUS 3/128)
(define BALL-SPEED 0.001)
(define BUMPER-SCALE (dir 1/64 3/32 1/32))
(define BUMPER-CONTACT-WIDTH (+ (dir-dy BUMPER-SCALE) BALL-RADIUS))
(define CONTACT-BUFFER (+ BALL-RADIUS (dir-dx BUMPER-SCALE)))
(define OPPONENT-X -3/4)
(define PLAYER-X 3/4)
(define SCREEN-WIDTH 1920)
(define SCREEN-HEIGHT 1080)
(define WALL-Y 1)

;; TODO: collision by raytrace

;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct ball (direction position))

(struct opponent (y))

(struct player (pressed y))

(struct state (ball dt n opponent player t))

;; STATE UPDATERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-counters s n t)
  (struct-copy state s
               [dt (- t (state-t s))]
               [n n]
               [t t]))

(define (update-key-pressed s key is-pressed)
  (struct-copy
   state s
   [player
    (struct-copy
     player (state-player s)
     [pressed
      (cond
        [is-pressed (set-add (player-pressed (state-player s)) key)]
        [else (set-remove (player-pressed (state-player s)) key)])])]))

(define (update-last s n t) (last n t))

(define (set-player-position s y)
  (struct-copy state s [player (struct-copy player (state-player s) [y y])]))

;; CALCULATION UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Get the y-offset between the center a bumper and a ball
(define (contact-offset-y bll y)
    (- (pos-y (ball-position bll)) y))

(define (get-ball-acceleration s)
  (+ 1 (* BALL-ACCELERATION (state-dt s))))

(define (within? x low high)
  (cond
    [(< high low) error("low must be less than high")]
    [else (and (<= x high)
               (>= x low))]))

;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(current-material (material #:ambient 0.1
                            #:diffuse 3
                            #:specular 0.3
                            #:roughness 0.3))

(define render-axes
  (combine (with-emitted (emitted "cyan" 2)
             (arrow origin -x))
           (with-emitted (emitted "magenta" 2)
             (arrow origin -y))
           (with-emitted (emitted "yellow" 2)
             (arrow origin -z))))

(define (render-ball s n dt)
  (with-emitted (emitted "white" 1.5)
    (sphere (ball-position (state-ball s)) BALL-RADIUS)))0

(define (render-opponent s n dt)
  (with-emitted (emitted 100 60 10 0.03)
    (rectangle (pos OPPONENT-X (opponent-y (state-opponent s)) 0)
               BUMPER-SCALE)))

(define (render-player s n dt)
  (with-emitted (emitted "cyan" 1.5)
    (rectangle (pos PLAYER-X (player-y (state-player s)) 0)
               BUMPER-SCALE)))

(define (render-lights+camera s n dt)
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (basis 'camera (point-at (pos 1 0 1/2) origin))))

;; EVENT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-draw s n t)
  (combine (render-opponent s n t)
           (render-player s n t)
           (render-ball s n t)
           (render-lights+camera s n t)))

(define (on-frame s n t)
  ((compose1
    (λ (s) (update-counters s n t))
    on-frame-ball
    on-frame-opponent
    on-frame-player-position)
   s))

(define (on-key s n t k)
  (update-key-pressed s k #t))

(define (on-mouse s n t x y e)
  (set-player-position s (- (* 1.6 (/ x SCREEN-WIDTH)) 0.8)))

(define (on-release s n t k)
  (update-key-pressed s k #f))

(define (stop-state? s n t)
  (set-member? (player-pressed (state-player s)) "escape"))

;; EVENT HANDLERS — ON-FRAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-frame-ball s)
  ((compose1
    on-frame-ball-direction
    on-frame-ball-position) s))

(define (on-frame-ball-collision-bumper s)
  (let ([plr (state-player s)]
        [opp (state-opponent s)]
        [bll (state-ball s)])
    (cond
      ; opponent collision
      [(and (< (dir-dx (ball-direction bll)) 0)
            (< (- (pos-x (ball-position bll)) CONTACT-BUFFER) OPPONENT-X)
            (within? (contact-offset-y (state-ball s)
                                       (opponent-y opp))
                     (- 0 (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        state s
        [ball (struct-copy
               ball (state-ball s)
               [direction (dir-scale (dir-reflect (ball-direction bll) +x)
                                     (get-ball-acceleration s))])])]

      ; player collision
      [(and (> (dir-dx (ball-direction bll)) 0)
            (within? (+ (pos-x (ball-position bll)) CONTACT-BUFFER)
                     (- PLAYER-X CONTACT-BUFFER)
                     PLAYER-X)
            (within? (contact-offset-y (state-ball s)
                                       (player-y plr))
                     (- 0 (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        state s
        [ball (struct-copy
               ball (state-ball s)
               [direction (dir-scale (dir-reflect (ball-direction bll) +x)
                                     (get-ball-acceleration s))])])]
        
      [else s])))


(define (on-frame-ball-collision-wall s)
  (let ([plr (state-player s)]
        [opp (state-opponent s)]
        [bll (state-ball s)])
    (cond
      ; left wall collision
      [(and (< (dir-dy (ball-direction bll)) 0)
            (< (pos-y (ball-position bll)) (- 0 (- WALL-Y BALL-RADIUS))))
       (struct-copy
        state s
        [ball (struct-copy
               ball (state-ball s)
               [direction (dir-reflect (ball-direction bll) +y)])])]
         
      ; right wall collision
      [(and (> (dir-dy (ball-direction bll)) 0)
            (> (pos-y (ball-position bll)) (- WALL-Y BALL-RADIUS)))
       (struct-copy
        state s
        [ball (struct-copy
               ball (state-ball s)
               [direction (dir-reflect (ball-direction bll) +y)])])]
      [else s])))

(define (on-frame-ball-direction s)
  ((compose1
    on-frame-ball-collision-bumper
    on-frame-ball-collision-wall) s))

(define (on-frame-ball-position s)
    (struct-copy
     state s
     [ball (struct-copy
            ball (state-ball s)
            [position
             (pos+ (ball-position (state-ball s))
                   (dir-scale (ball-direction (state-ball s))
                              (* (state-dt s) BALL-SPEED)))])]))

(define (on-frame-opponent s) s)

(define (on-frame-player-position s)
  (let ([pressed (player-pressed (state-player s))])
    (cond
      [(set-member? pressed "left")
       (set-player-position
        s
        (max -1/2 (- (* (state-dt s) 1/512) (player-y (state-player s)))))]
      [(set-member? pressed "right")
       (set-player-position
        s
        (min 1/2  (+ (* (state-dt s) 1/512) (player-y (state-player s)))))]
      [else s])))

;; BANGIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang3d
   (state (ball (dir 1 0 0) ; ball
                (pos 0 0 0))
          0                 ; dt
          0                 ; n
          (opponent 0)      ; opponent
          (player empty 0)  ; player
          0)                ; t
   #:frame-delay (/ 1000 59.9)
   #:name "Pong3D — Racket"
   #:on-draw on-draw
   #:on-frame on-frame
   #:on-key on-key
   #:on-mouse on-mouse
   #:on-release on-release
   #:stop-state? stop-state?
   #:width SCREEN-WIDTH
   #:height SCREEN-HEIGHT)
