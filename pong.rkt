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
(define PLAYER-X 3/4)
(define OPPONENT-X -3/4)

;; TODO: collision by raytrace

;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct ball (direction position))

(struct opponent (y))

(struct player (pressed y))

(struct state (ball dt n opponent player t))

;; STATE UPDATERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-ball s)
  (let ([b (state-ball s)]
        [dt (state-dt s)])
    (let ([ball-dir (ball-direction b)])
      (let ([ball-pos-prediction (update-ball-position s)])
        ((compose1
          update-ball-direction
          update-ball-position) s)))))

(define (update-ball-direction s)
  (let ([plr (state-player s)]
        [opp (state-opponent s)]
        [ball-dir (ball-direction (state-ball s))])
    (let ([ball-pos-prediction (ball-position (state-ball (update-ball-position s)))])
      (cond
        ; opponent collision
        [(and (< (dir-dx ball-dir) 0)
              (< (- (pos-x ball-pos-prediction) CONTACT-BUFFER) OPPONENT-X)
              (within? (contact-offset-y (state-ball s)
                                         (opponent-y opp))
                       (- 0 (dir-dy BUMPER-SCALE))
                       (dir-dy BUMPER-SCALE)))
         (struct-copy
          state s
          [ball (struct-copy
                 ball (state-ball s)
                 [direction (dir-scale (dir-reflect ball-dir +x)
                                       (get-ball-acceleration s))])])]

        ; player collision
        [(and (> (dir-dx ball-dir) 0)
              (within? (+ (pos-x ball-pos-prediction) CONTACT-BUFFER)
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
                 [direction (dir-scale (dir-reflect ball-dir +x)
                                       (get-ball-acceleration s))])])]
        
        [else s]))))

(define (update-ball-position s)
    (struct-copy
     state s
     [ball (struct-copy
            ball (state-ball s)
            [position
             (pos+ (ball-position (state-ball s))
                   (dir-scale (ball-direction (state-ball s))
                              (* (state-dt s) BALL-SPEED)))])]))

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

(define (update-opponent s) s)

(define (update-player-position s)
  (let ([pressed (player-pressed (state-player s))])
    (cond
      [(set-member? pressed "left")
       (struct-copy
        state s
        [player (struct-copy
                player (state-player s)
                [y (max -1/2 (+ (* (state-dt s) -1/512) (player-y (state-player s))))])])]
      [(set-member? pressed "right")
       (struct-copy
        state s
        [player (struct-copy
                 player (state-player s)
                [y (min 1/2 (+ (* (state-dt s) 1/512) (player-y (state-player s))))])])]
      [else s])))

(define (update-last s n t) (last n t))

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
    update-ball
    update-opponent
    update-player-position)
   s))

(define (on-frame-after s n t)
  (state
   (state-ball s)
   (update-last s n t)
   (state-opponent s)
   (state-player s)))

; on-key runs after on-frame, so we should respond immediately to any user input
(define (on-key s n t k)
  (update-key-pressed s k #t))

(define (on-release s n t k)
  (update-key-pressed s k #f))

;; BANGIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang3d
   (state (ball (dir 1 0 0) ; ball
                (pos 0 0 0))
          0                 ; dt
          0                 ; n
          (opponent 0)      ; opponent
          (player empty 0)  ; player
          0)                ; t
   #:frame-delay (/ 1000 119)
   #:on-draw on-draw
   #:on-frame on-frame
   #:on-key on-key
   #:on-release on-release
   #:width 1920
   #:height 1080)