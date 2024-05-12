#lang typed/racket/base

(require pict3d
         "../config.rkt"
         "../state/state.rkt")

(provide aspect-ratio
         camera
         CAMERA-DIR
         CAMERA-POINT-AT
         CAMERA-POS
         camera-transform-pong)

;; RENDER — CAMERA

(: aspect-ratio : -> Flonum)
(define (aspect-ratio) (/ SCREEN-WIDTH-INEXACT
                          SCREEN-HEIGHT-INEXACT))

(: camera : State -> Pict3D)
(define (camera s)
  (basis 'camera (camera-transform-pong s)))

(: CAMERA-POS : Pos)
(define CAMERA-POS
  ((λ ()
     (define ar (aspect-ratio))
     (define fov (exact->inexact FOV))
     (pos (+ 2.0 (* ar -0.3) (* fov 0.01))      ; distance
          0.0                                   ; side-to-side
          (+ 1.0 (* ar -0.2) (* fov 0.001)))))) ; elevation

(: CAMERA-DIR : Dir)
(define CAMERA-DIR
  ((λ ()
     (define normalized (dir-normalize (pos- CAMERA-LOOK-AT CAMERA-POS)))
     (cond [normalized normalized]
           [else (error "normalized not normal")]))))

(: CAMERA-POINT-AT : Affine)
(define CAMERA-POINT-AT (point-at CAMERA-POS CAMERA-LOOK-AT))

(: camera-transform-pong : State -> Affine)
(define (camera-transform-pong s)
  CAMERA-POINT-AT)
