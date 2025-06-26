#lang typed/racket/base

(require (only-in pict3d Dir)
         (only-in pict3d
                  dir
                  dir-dx
                  dir-dy
                  dir-dz))

(provide (all-defined-out))

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

(: PADDLE-SCALE : Dir)
(define PADDLE-SCALE (dir 0.015 0.11 0.03))

(: PADDLE-CONTACT-WIDTH : Flonum)
(define PADDLE-CONTACT-WIDTH (+ (dir-dy PADDLE-SCALE) BALL-RADIUS))

(: BOUNDS-BUMPER-GAP : Flonum)
(define BOUNDS-BUMPER-GAP (* 10 (dir-dx PADDLE-SCALE)))

(: BUMPER-CONTACT-BUFFER : Flonum)
(define BUMPER-CONTACT-BUFFER (+ BALL-RADIUS (dir-dx PADDLE-SCALE)))

(: FOV : Positive-Real)
(define FOV #i60)

(: FPS : Flonum)
(define FPS #i60)

(: FRAME-DELAY-MILLIS : Positive-Real)
(define FRAME-DELAY-MILLIS (max #i1 (abs (/ #i1000 FPS))))

(: OPPONENT-SPEED : Flonum)
(define OPPONENT-SPEED 0.0035)

(: OPPONENT-X : Flonum)
(define OPPONENT-X #i-1)

(: OPPONENT-X-COLLISION : Flonum)
(define OPPONENT-X-COLLISION (+ OPPONENT-X BUMPER-CONTACT-BUFFER))

(: OPPONENT-BOUNDS : Flonum)
(define OPPONENT-BOUNDS (- OPPONENT-X BOUNDS-BUMPER-GAP))

(: PLAYER-X : Flonum)
(define PLAYER-X #i1)

(: PLAYER-X-COLLISION : Flonum)
(define PLAYER-X-COLLISION   (- PLAYER-X   BUMPER-CONTACT-BUFFER))

(: PLAYER-BOUNDS : Flonum)
(define PLAYER-BOUNDS (+ PLAYER-X BOUNDS-BUMPER-GAP))

(: PLAYER-SPEED : Flonum)
(define PLAYER-SPEED 0.05)

(: REDIRECT-FACTOR : Flonum)
(define REDIRECT-FACTOR #i40)

(: SCREEN-WIDTH-INIT : Positive-Integer)
(define SCREEN-WIDTH-INIT 1200)

(: SCREEN-HEIGHT-INIT : Positive-Integer)
(define SCREEN-HEIGHT-INIT 1080)

(: WALL-Y : Flonum)
(define WALL-Y 0.8)

(: BALL-MAX-Y : Flonum)
(define BALL-MAX-Y (- WALL-Y BALL-RADIUS))