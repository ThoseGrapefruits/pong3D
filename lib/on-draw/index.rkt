#lang typed/racket

(require
  pict3d
  "../config.rkt"
  "../state/state.rkt"
  "../util/number/index.rkt")

(provide on-draw)

; GLOBAL SETTINGS

(current-pict3d-fov FOV)
(current-material (material #:ambient 0
                            #:diffuse 0
                            #:specular 0
                            #:roughness 0.3))

; RENDER — ON-DRAW

(: on-draw : State Natural Flonum -> Pict3D)
(define (on-draw s n t)
  (combine (render-game-play s)
           (render-game-over s)))

(: aspect-ratio : -> Flonum)
(define (aspect-ratio) (/ SCREEN-WIDTH-INEXACT
                          SCREEN-HEIGHT-INEXACT))

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
  (define fov (exact->inexact FOV))
  (pos (+ 2.0 (* ar -0.3) (* fov 0.01))    ; distance
       0.0                                 ; side-to-side
       (+ 1.0 (* ar -0.2) (* fov 0.001)))) ; elevation

(: camera-transform : State -> Affine)
(define (camera-transform s)
  (camera-point-at))


; RENDER FUNCTIONS

; Scale a [-1,1] value up to a range of the given width.
(: scale--1-1 : Flonum Integer -> Flonum)
(define (scale--1-1 n width)
  (define half-width (/ (exact->inexact width) 2.0))
  (+ (* n half-width) half-width))

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
      (render-game-over-background s)
      (render-game-over-score s))]
    [else empty-pict3d]))

(: render-game-over-background : State-Game-Over -> Pict3D)
(define (render-game-over-background s)
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
              ; sun
              (transform
               (light origin (emitted "goldenrod" 0.0001))
               (affine-compose
                (rotate-z (- 135.0 (* 0.003 (State-t s))))
                (move-y -0.5)
                (move-z -0.99)
                (scale 40)))
              ; sky
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
  (define ball (State-Play-ball s))
  (combine
   ; predicted position
   ; (for/list : (Listof Pict3D)
   ;   ([predicted-pos (State-Play-ball-predicted-pos-ends s)])
   ;   (define is-player (positive? (pos-x predicted-pos)))
   ;   (cond [(null? predicted-pos) empty-pict3d]
   ;         [else (with-emitted
   ;                   (emitted "oldlace" 0.3)
   ;                 (transform
   ;                  (sphere origin BALL-RADIUS)
   ;                  (affine-compose
   ;                   (move (pos- predicted-pos origin))
   ;                   (rotate-y (if is-player 90.0 -90.0)))))]))
   (light
    (Ball-pos ball)
    (emitted "oldlace" 0.1)
    #:range 1)
   (with-emitted (emitted "oldlace" 1.5)
     (sphere (Ball-pos ball) BALL-RADIUS))))

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
         (cube (pos (* (exact->inexact n) -0.08) 0.0 0.0) 0.02))))
    (position-screen-space-pixels s -100.0 100.0))))

(: render-game-play-opponent : State-Play -> Pict3D)
(define (render-game-play-opponent s)
  (parameterize
      ([current-material (material #:ambient 0.0
                                   #:diffuse 0.0
                                   #:specular 0.0
                                   #:roughness 0.3)]
       [current-emitted COLOR-OPPONENT-EMITTED])
    (rectangle (pos OPPONENT-X (Opponent-y (State-Play-opponent s)) 0.0)
               BUMPER-SCALE)))

(: render-game-play-player : State-Play -> Pict3D)
(define (render-game-play-player s)
  (parameterize
      ([current-material (material #:ambient 0.0
                                   #:diffuse 0.0
                                   #:specular 0.6
                                   #:roughness 0.3)]
       [current-emitted COLOR-PLAYER-EMITTED])
    (rectangle (pos PLAYER-X (Player-y (State-Play-player s)) 0.0)
               BUMPER-SCALE)))

(: render-game-play-lights+camera : State-Play -> Pict3D)
(define (render-game-play-lights+camera s)
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (camera s)))

(define-struct Score-Section
  ([color-emitted : Emitted]
   [y : Flonum]
   [place-low : Integer]
   [place-high : Integer]))

(define SCORE-SECTIONS : (Listof Score-Section)
  (list (Score-Section (emitted 1.0 1.0 1.0 2.0) 0.0  10       1)   ; ones
        (Score-Section (emitted 0.5 0.7 1.0 2.0) 0.03 100     10)   ; tens
        (Score-Section (emitted 1.0 0.8 0.0 1.5) 0.06 1000   100)   ; hundreds
        (Score-Section (emitted 0.6 0.0 0.8 1.5) 0.09 10000 1000))) ; thousands

(: render-player-score : Nonnegative-Integer -> Pict3D)
(define (render-player-score score)
  (combine
   (for/list : (Listof Pict3D)
     ([ score-section SCORE-SECTIONS ])
     (match-define (Score-Section color-emitted y place-low place-high) score-section)
     (parameterize ([current-emitted color-emitted])
       (combine
        (for/list : (Listof Pict3D)
          ([ n (range 0.0 10) ])
          (pipe (pos (* n 0.03) y 0.0) (dir 0.01 0.01 0.001)
                #:top-radii (interval 6/10 1)
                #:bottom-radii (interval 6/10 1)))
        (for/list : (Listof Pict3D)
          ([n (range 0.0 (get-number-place score place-low place-high))])
          (sphere (pos (* n 0.03) y 0.0) 0.01)))))))
