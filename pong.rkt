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

(define PLAYER-X 3/4)
(define OPPONENT-X -3/4)

;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct ball (direction position))

(struct last [n t])

(struct opponent (y))

(struct player (pressed y))

(define (player-update-key-pressed p key is-pressed)
  (player (cond [is-pressed (set-add (player-pressed p) key)]
                [else (set-remove (player-pressed p) key)])
          (player-y p)))

(struct state (ball last opponent player))

;; STATE ACCESSORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; state -> Number
(define (state-last-t s) (last-t (state-last s)))

; state Number -> Number
(define (state-last-dt s t) (- t (state-last-t s)))

;; STATE UPDATERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-ball s n t)
  (let ([b (state-ball s)])
    (let ([ball-dir (ball-direction b)])
      (ball ball-dir
            (pos+ (ball-position b)
                  (dir-scale ball-dir (* t 0.00001)))))))

(define (update-opponent s n t) (state-opponent s))

(define (update-player s n t)
  (update-player-position s n t (state-player s)))

(define (update-player-position s n t pl)
  (let ([dt (state-last-dt s t)])
    (let ([pressed (player-pressed (state-player s))])
      (cond
        [(set-member? pressed "left")
         (player pressed
                 (max -1/2 (+ (* dt -1/512) (player-y pl))))]
        [(set-member? pressed "right")
         (player pressed
                 (min 1/2 (+ (* dt 1/512) (player-y pl))))]
        [else pl]))))

(define (update-last s n t) (last n t))

;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(current-material (material #:ambient 0.01
                            #:diffuse 0.5
                            #:specular 0.8
                            #:roughness 0.2))

(define render-axes
  (combine (with-emitted (emitted "cyan" 2)
             (arrow origin -x))
           (with-emitted (emitted "magenta" 2)
             (arrow origin -y))
           (with-emitted (emitted "yellow" 2)
             (arrow origin -z))))

(define (render-ball s n dt)
  (with-emitted (emitted "white" 1.5)
    (sphere (ball-position (state-ball s)) 3/128)))0

(define (render-opponent s n dt)
  (with-emitted (emitted 100 60 10 0.03)
    (rectangle (pos OPPONENT-X (opponent-y (state-opponent s)) 0)
               (dir 1/64 3/32 1/32))))

(define (render-player s n dt)
  (with-emitted (emitted "cyan" 1.5)
    (rectangle (pos PLAYER-X (player-y (state-player s)) 0)
               (dir 1/64 3/32 1/32))))

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
  (on-frame-after (state
   (update-ball s n t)
   (state-last s)
   (update-opponent s n t)
   (update-player s n t)) n t))

(define (on-frame-after s n t)
  (state
   (state-ball s)
   (update-last s n t)
   (state-opponent s)
   (state-player s)))

; on-key runs after on-frame, so we should respond immediately to any user input
(define (on-key s n t k)
  (state (state-ball s)
         (state-last s)
         (state-opponent s)
         (update-player-position
          s n t
          (player-update-key-pressed (state-player s) k #t))))

(define (on-release s n t k)
    (state (state-ball s)
           (state-last s)
           (state-opponent s)
           (player-update-key-pressed (state-player s) k #f)))

;; BANGIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang3d
   (state (ball (dir 1 0 0)
                (pos 0 0 0))
          (last 0 0)
          (opponent 0)
          (player empty 0))
   #:frame-delay (/ 1000 120)
   #:on-draw on-draw
   #:on-frame on-frame
   #:on-key on-key
   #:on-release on-release)