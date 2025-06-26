#lang typed/racket/base

(require
  (only-in pict3d Dir Pict3D Pos)
  (only-in pict3d
           camera-ray-dir
           trace/data)
  (only-in racket/match match-define)
  "../on-draw/camera.rkt"
  "./state.rkt"
  "./syntax.rkt")

(provide (all-defined-out))

(: State-update-counters : State-Any Natural Flonum -> State-Any)
(define (State-update-counters s n t)
  (define dt (- t (unbox (State-t s))))
  (box-cas! (State-dt s) (unbox (State-dt s)) dt)
  (box-cas! (State-n s)  (unbox (State-n s))  n)
  (box-cas! (State-t s)  (unbox (State-t s))  t)
  s)

(: State-update-trace-mouse : State-Any Integer Integer -> State-Any)
(define (State-update-trace-mouse s x y)
  (match-define (cons width height) (State-window-dims s))
  (define direction   ((camera-ray-dir (camera-transform-pong s)
                                       #:width width
                                       #:height height
                                       #:z-near 0.01)
                       x y))
  (define traced-data (and direction
                           (trace/data (unbox (State-pict-last s))
                                       (camera-pos s)
                                       direction)))
  (define mouse-pos   (cons x y))

  ; Only update trace-mouse/last if we have a successful trace
  (cond [traced-data
         (State-update-parent s
                              [mouse-pos-last   #:parent State mouse-pos]
                              [trace-mouse      #:parent State traced-data]
                              [trace-mouse/last #:parent State traced-data])]
        [else
         (State-update-parent s
                              [mouse-pos-last #:parent State mouse-pos]
                              [trace-mouse    #:parent State traced-data])]))
