#lang typed/racket/base

(require pict3d
         racket/match
         "./camera.rkt"
         "./util.rkt"
         "../config.rkt"
         "../state/state.rkt")

(provide position-screen-space-pixels
position-screen-space-relative)

; Get a transformation that moves an object from the origin to the given (x,y,z)
; coordinate in screen-space, where (0,0,1) is the top-left of the camera
; viewport and (width,height,1) is the bottom-right, placed in a z-plane 1 unit
; away from the camera.
(: position-screen-space-pixels : State Flonum Flonum Flonum [#:wrap? Boolean] -> Affine)
(define (position-screen-space-pixels s x y [z 1.0] #:wrap? [wrap? #f])
  (match-define (cons width height) (State-window-dims s))
  (define cam-t (camera-transform-pong s))
  (define z-near (* z CAMERA-SPACE-DISTANCE))
  (define dir ((camera-ray-dir cam-t
                               #:width width
                               #:height height
                               #:z-near z-near)
               (if wrap? (wrap-within x (exact->inexact width)) x)
               (if wrap? (wrap-within y (exact->inexact height)) y)))
  (affine-compose (move dir)
                  cam-t
                  (scale z-near)))

; Get a transformation that moves an object from the origin to the given (x,y,z)
; coordinate in screen-space, where (-1,-1,1) is the top-left of the camera
; viewport, and (1,1,1) is the bottom-right, placed in a z-plane 1 unit away
; from the camera.
(: position-screen-space-relative : State Flonum Flonum Flonum [#:wrap? Boolean] -> Affine)
(define (position-screen-space-relative s x y z #:wrap? [wrap? #f])
  (match-define (cons width height) (State-window-dims s))
  (position-screen-space-pixels
   s
   (scale--1-1 x width)
   (scale--1-1 y height)
   z
   #:wrap? wrap?))