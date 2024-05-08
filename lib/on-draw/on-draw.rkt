#lang typed/racket/base

(require
  pict3d
  "./camera.rkt"
  "./game-score.rkt"
  "./on-char-jiggle.rkt"
  "./on-draw-game-over.rkt"
  "./on-draw-game-play.rkt"
  "./position-screen-space.rkt"
  "./text.rkt"
  "../config.rkt"
  "../state/state.rkt"
  "../util/number/index.rkt")

(module srfi racket/base
  (require srfi/13)
  (provide string-pad))

(require/typed 'srfi
  [string-pad (->* (String Integer) (Char Integer Integer) String)])

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
  (combine (on-draw-game-play s)
           (on-draw-game-over s)))

; RENDER — CONSTANTS

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
