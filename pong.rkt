#lang racket

(require
  pict3d
  pict3d/universe
  racket/set)

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

(define BALL-ACCELERATION 0.0002)
(define BALL-RADIUS 3/128)
(define BALL-SPEED 0.001)
(define BUMPER-SCALE (dir 1/64 7/64 1/32))
(define BUMPER-CONTACT-WIDTH (+ (dir-dy BUMPER-SCALE) BALL-RADIUS))
(define CONTACT-BUFFER (+ BALL-RADIUS (dir-dx BUMPER-SCALE)))
(define OPPONENT-SPEED 0.0010)
(define OPPONENT-X -3/4)
(define PLAYER-X 3/4)
(define SCREEN-WIDTH 1200)
(define SCREEN-HEIGHT 1080)
(define SPIN-FACTOR 40)
(define WALL-Y 0.8)
(define ASPECT-RATIO (/ SCREEN-WIDTH SCREEN-HEIGHT))
(define CAMERA-X (+ 0.5 (/ 1 ASPECT-RATIO)))
(define CAMERA-Y (/ (+ 0.5 (/ 1 ASPECT-RATIO)) 2))

;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct ball (direction position))

(struct opponent (y))

(struct player (pressed y))

(struct state (ball dt n opponent pause-next-frame? paused? player t))

;; STATE UPDATERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-counters s n t)
  (struct-copy state (update-dt s n t) [n n] [t t]))

(define (update-dt s n t)
  (struct-copy state s [dt (- t (state-t s))]))

(define (update-key-pressed s key pressed?)
  (struct-copy
   state s
   [player
    (struct-copy
     player (state-player s)
     [pressed
      (cond
        [pressed? (set-add (player-pressed (state-player s)) key)]
        [else (set-remove (player-pressed (state-player s)) key)])])]))

(define (update-last s n t) (last n t))

(define (set-player-position s y)
  (struct-copy
   state s
   [player (struct-copy
            player (state-player s)
            [y (clamp-bumper-y y)])]))

;; CALCULATION UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clamp x low high)
  (cond
    [(< high low) error("low must be less than high")]
    [else (max low (min high x))]))

(define (clamp-bumper-y y)
  (clamp y (+ -1 (* 3 (dir-dy BUMPER-SCALE))) (- 1 (* 3 (dir-dy BUMPER-SCALE)))))

; Get the y-offset between the center a bumper and a ball
(define (get-contact-offset-y s y)
    (- (pos-y (ball-position (state-ball s))) y))

(define (get-ball-acceleration s)
  (+ 1 BALL-ACCELERATION))

(define (sign n)
  (cond [(< n 0) -1]
        [else 1]))

(define (within? x low high)
  (cond
    [(< high low) error("low must be less than high")]
    [else (and (<= x high)
               (>= x low))]))

;; EVENT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-frame s n t)
  ((compose1
    (λ (s) (update-counters s n t))
    on-frame-pause
    on-frame-ball
    on-frame-opponent
    on-frame-player-position)
   s))

(define (on-mouse s n t x y e)
  (set-player-position s (- (* 1.6 (/ x SCREEN-WIDTH)) 0.8)))

(define (pause-state? s n t)
  (state-paused? s))

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
            (within? (+ (pos-x (ball-position bll)) CONTACT-BUFFER)
                     (+ OPPONENT-X CONTACT-BUFFER)
                     (+ OPPONENT-X (* CONTACT-BUFFER 2)))
            (within? (get-contact-offset-y s (opponent-y opp))
                     (- 0 (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        state s
        [ball (struct-copy
               ball (state-ball s)
               [direction
                (dir-scale
                 (dir-reflect (ball-direction bll)
                              (angles->dir
                               (* (- 180 SPIN-FACTOR)
                                  (get-contact-offset-y s (opponent-y opp)))
                               0))
                 (get-ball-acceleration s))])])]
      ; player collision
      [(and (> (dir-dx (ball-direction bll)) 0)
            (within? (+ (pos-x (ball-position bll)) CONTACT-BUFFER)
                     (- PLAYER-X CONTACT-BUFFER)
                     PLAYER-X)
            (within? (get-contact-offset-y s (player-y plr))
                     (- 0 (dir-dy BUMPER-SCALE))
                     (dir-dy BUMPER-SCALE)))
       (struct-copy
        state s
        [ball
         (struct-copy
          ball (state-ball s)
          [direction
           (dir-scale
            (dir-reflect
             (ball-direction bll)
             (angles->dir
              (* (- SPIN-FACTOR) (get-contact-offset-y s (player-y plr)))
              0))
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
               [direction (dir-scale
                           (dir-reflect (ball-direction bll) +y)
                           (get-ball-acceleration s))])])]
      ; right wall collision
      [(and (> (dir-dy (ball-direction bll)) 0)
            (> (pos-y (ball-position bll)) (- WALL-Y BALL-RADIUS)))
       (struct-copy
        state s
        [ball (struct-copy
               ball (state-ball s)
               [direction (dir-scale
                           (dir-reflect (ball-direction bll) +y)
                           (get-ball-acceleration s))])])]
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

(define (on-frame-opponent s)
  (let ([aim-buffer (/ BUMPER-CONTACT-WIDTH 8)]
        [position-desired (pos-y (ball-position (state-ball s)))])
    (let ([position-diff (- position-desired (opponent-y (state-opponent s)))])
      (struct-copy
       state s
       [opponent (struct-copy
                  opponent (state-opponent s)
                  [y (+ (opponent-y (state-opponent s))
                        (cond
                          [(within? (pos-y (ball-position (state-ball s)))
                                   (- (opponent-y (state-opponent s)) aim-buffer)
                                   (+ (opponent-y (state-opponent s)) aim-buffer))
                           (* OPPONENT-SPEED
                              (state-dt s)
                              position-diff)]
                          [else
                           (* OPPONENT-SPEED
                              (state-dt s)
                              (sign position-diff))]))])]))))

(define (on-frame-pause s)
  (cond [(state-pause-next-frame? s) (struct-copy state s [paused? #t] [pause-next-frame? #f])]
        [else s]))

(define (on-frame-player-position s)
  (let ([pressed (player-pressed (state-player s))])
    (cond
      [(set-member? pressed "left")
       (set-player-position
        s
        (max (- 0 WALL-Y) (+ (player-y (state-player s)) (* (state-dt s) -1/512))))]
      [(set-member? pressed "right")
       (set-player-position
        s
        (min WALL-Y       (+ (player-y (state-player s)) (* (state-dt s)  1/512))))]
      [else s])))

;; EVENT HANDLERS — ON-KEY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-key s n t k)
  (update-key-pressed
   (on-key-immediate s k)
   k #t))

; Immediate reactions to keypresses
(define (on-key-immediate s k)
  (cond
    [(string=? k " ") (struct-copy state s [paused? (not (state-paused? s))])]
    [(string=? k "r") (struct-copy state (state-reset s)
                                   [pause-next-frame? #t])]
    [else s]))

(define (on-release s n t k)
  (update-key-pressed s k #f))

;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-draw s n t)
  (combine (render-opponent s n t)
           (render-player s n t)
           (render-ball s n t)
           (render-lights+camera s n t)
           (render-arena s)
           (render-arena-bumpers s)))


(current-material (material #:ambient 0
                            #:diffuse 0
                            #:specular 0
                            #:roughness 0.3))

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

(define arena-bumper
  (combine (with-emitted (emitted "oldlace" 2) (cylinder origin 1))
           (for/list ([z  (in-range 0 10)])
             (light (pos 0 0 (- (* 2 (/ z 9)) 1))
                    (emitted "oldlace" 1)))))

(define render-axes
  (combine (with-emitted (emitted "cyan" 2)
             (arrow origin -x))
           (with-emitted (emitted "magenta" 2)
             (arrow origin -y))
           (with-emitted (emitted "yellow" 2)
             (arrow origin -z))))

(define (render-ball s n dt)
  (combine
   (light
    (ball-position (state-ball s))
    (emitted "oldlace" 0.1)
    #:range 1)
   (with-emitted (emitted "oldlace" 1.5)
     (sphere (ball-position (state-ball s)) BALL-RADIUS))))

(define (render-opponent s n dt)
  (parameterize
      ([current-material (material #:ambient 0.01
                                  #:diffuse 0.1
                                  #:specular 0.6
                                  #:roughness 0.3)]
       [current-emitted (emitted 100 60 10 0.03)])
    (rectangle (pos OPPONENT-X (opponent-y (state-opponent s)) 0)
               BUMPER-SCALE)))

(define (render-player s n dt)
  (parameterize
      ([current-material (material #:ambient 0.01
                                  #:diffuse 0.1
                                  #:specular 0.6
                                  #:roughness 0.3)]
       [current-emitted (emitted "plum" 1.5)])
    (rectangle (pos PLAYER-X (player-y (state-player s)) 0)
               BUMPER-SCALE)))

(define (render-lights+camera s n dt)
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (basis 'camera (point-at (pos CAMERA-X 0 CAMERA-Y) origin))))

;; VALIDATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (valid-state? s n t)
  (not (and (state-paused? s) (state-pause-next-frame? s))))

;; BANGIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (state-reset s)
  (struct-copy state state-start
               [dt (state-dt s)]
               [n  (state-n s)]
               [t  (state-t s)]))

(define state-start
  (state (ball (dir 1 0.75 0) ; ball
               (pos 0 0 0))
         0                 ; dt
         0                 ; n
         (opponent 0)      ; opponent
         #f                ; pause-next-frame?
         #f                ; paused?
         (player empty 0)  ; player
         0))               ; t

(big-bang3d
   (struct-copy state state-start [paused? #t])
   #:frame-delay (/ 1000 59.9)
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
