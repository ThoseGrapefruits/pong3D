#lang racket

;; to do next: update ball position

(require
  pict3d
  pict3d/universe
  racket/set)

;
;
;
;     ^                               
;    /                             ╭─╮
;   x                              ╰─╯
;  /                                                                        ^
; v                                                                         |
;                                                                           z 
;                                                                           |
;                             <=====y=====>                                 v

;; DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct ball (direction position))

(struct last (n t))

(struct player (pressed y))

(define (player-update-key-pressed p key is-pressed)
  (player (cond [is-pressed (set-add (player-pressed p) key)]
                [else (set-remove (player-pressed p) key)])
          (player-y p)))

(struct state (ball player last))

;; STATE ACCESSORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (state-last-t s) (last-t (state-last s)))

(define (state-last-dt s t) (- t (state-last-t s)))

;; STATE UPDATERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-player-position s n t)
  (let ([pl (state-player s)]
        [dt (state-last-dt s t)])
    (let ([pressed (player-pressed (state-player s))])
      (cond
        [(set-member? pressed "left")
         (state (state-ball s)
                (player pressed (+ (* dt -1/512) (player-y pl)))
                (state-last s))]
        [(set-member? pressed "right")
         (state (state-ball s)
                (player pressed (+ (* dt 1/512) (player-y pl)))
                (state-last s))]
        [else s]))))

(define (update-tickers s n t)
  (state (state-ball s)
         (state-player s)
         (last n t)))

;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(current-material (material #:ambient 0.01
                            #:diffuse 0.5
                            #:specular 0.8
                            #:roughness 0.2))

(define (render-ball s n dt)
  (sphere (ball-position (state-ball s)) 1/16))

(define (render-player s n dt)
  (rectangle (pos 3/4 (player-y (state-player s)) 0)
             (dir 1/64 3/32 1/32)))

(define (render-lights+camera s n dt)
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (basis 'camera (point-at (pos 1 0 1/2) origin))))

;; EVENT HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-draw s n t)
  (combine (render-player s n t)
           (render-ball s n t)
           (render-lights+camera s n t)))

(define (on-frame s n t)
  (update-tickers
   (update-player-position s n t)
   n t))

; on-key runs after on-frame, so we should respond immediately to any user input
(define (on-key s n t k)
    (update-player-position ; respond immediately to input
     (state (state-ball s)
            (player-update-key-pressed (state-player s) k #t)
            (state-last s))
     n t))

(define (on-release s n t k)
    (state (state-ball s)
           (player-update-key-pressed (state-player s) k #f)
           (state-last s)))

;; BANGIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang3d
   (state (ball (dir 1 0 0)
                (pos 0 0 0))
          (player empty 0)
          (last 0 0))
   #:frame-delay (/ 1000 60)
   #:on-draw on-draw
   #:on-frame on-frame
   #:on-key on-key
   #:on-release on-release)