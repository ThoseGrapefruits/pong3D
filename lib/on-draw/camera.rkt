#lang typed/racket

(require pict3d
         "../config.rkt"
         "../state/state.rkt")

(provide aspect-ratio
         camera
         camera-dir
         camera-point-at
         camera-pos
         camera-transform-pong)

;; RENDER — CAMERA

(: aspect-ratio : -> Flonum)
(define (aspect-ratio) (/ SCREEN-WIDTH-INEXACT
                          SCREEN-HEIGHT-INEXACT))

(: camera : State -> Pict3D)
(define (camera s)
  (basis 'camera (camera-transform-pong s)))

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

(: camera-transform-pong : State -> Affine)
(define (camera-transform-pong s)
  (camera-point-at))
