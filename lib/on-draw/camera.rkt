#lang typed/racket/base

(require
  (only-in pict3d Affine Dir Pict3D Pos)
  (only-in pict3d
           affine-compose
           basis
           dir
           dir-normalize
           pos
           pos+
           pos-
           origin
           point-at
           +x)
  (only-in racket/match match-define)
  "../config.rkt"
  "../state/state.rkt")

(provide aspect-ratio
         camera
         camera-dir
         camera-point-at
         camera-pos
         camera-transform-pong)

;; RENDER — CAMERA

(: aspect-ratio : State -> Flonum)
(define (aspect-ratio s)
  (match-define (cons width height) (State-window-dims s))
  (/ (exact->inexact width)
     (exact->inexact height)))

(: camera : State -> Pict3D)
(define (camera s)
  (basis 'camera (camera-transform-pong s)))

(: camera-pos : State -> Pos)
(define (camera-pos s)
  (define ar (aspect-ratio s))
  (define fov (exact->inexact FOV))
  (pos (+ 2.0 (* ar -0.3) (* fov 0.01))      ; distance
       0.0                                   ; side-to-side
       (+ 1.0 (* ar -0.2) (* fov 0.001))))   ; elevation

(: camera-dir : State -> Dir)
(define (camera-dir s)
  (define normalized (dir-normalize (pos- (camera-look-at s)
                                          (camera-pos s))))
  (or normalized
      (error "camera-dir not normalizable")))

(: camera-look-at : State -> Pos)
(define (camera-look-at s) (pos+ origin +x 0.5))

(: camera-point-at : State -> Affine)
(define (camera-point-at s)
  (point-at (camera-pos s)
            (camera-look-at s)))

(: camera-transform-pong : State -> Affine)
(define (camera-transform-pong s)
  (camera-point-at s))
