#lang typed/racket/base

(require pict3d
         racket/math
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/memoize.rkt"
         "../util/cirque.rkt"
         "../util/quad-thicc.rkt")

(provide (all-defined-out))

(define char:a
  (make-Char-3D-memoized
   #\a
   WIDTH-EM-1/2
   (λ () (combine
          ; loop
          (cirque-x-3/8 #:basis 'x)
          ; ascender
          (rectangle (pos+ LINE/MID-X/START +x (- WIDTH-EM-3/8 WIDTH-STROKE-1/2))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))))))

(define char:b
  (make-Char-3D-memoized
   #\b
   WIDTH-EM-1/2
   (λ () (combine
          ; loop
          (cirque-x-3/8 #:basis 'x)
          ; ascender
          (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                     (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))))))

(define char:c
  (make-Char-3D-memoized
   #\c
   WIDTH-EM-1/2
   (λ () (cirque-x-3/8 #:arc (arc 40.0 -40.0)))))

(define char:d
  (make-Char-3D-memoized
   #\d
   WIDTH-EM-1/2
   (λ () (combine
          (cirque-x-3/8 #:basis 'x)
          (rectangle (pos+ LINE/MID/START +x (- WIDTH-EM-3/8 WIDTH-STROKE-1/2))
                     (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))))))

(define char:e
  (make-Char-3D-memoized
   #\e
   WIDTH-EM-1/2
   (λ ()
     (define extender-radius-x (- WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/4))
     (combine
      ; curve left
      (cirque-x-3/8 #:arc (arc 90.0 0.0) #:basis 'y)
      ; cross
      (rectangle LINE/MID-X/CENTER-3/8
                 (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
      ; bottom extender
      (rectangle (pos+ LINE/BASE/CENTER-3/8 (dir extender-radius-x (- WIDTH-STROKE-1/2) 0.0))
                 (dir extender-radius-x WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:f
  (make-Char-3D-memoized
   #\f
   WIDTH-EM-5/16
   (λ () (combine
          ; curve
          (cirque-y-3/8 #:arc (arc -180.0 -60.0) #:basis 'x)
          ; ascender upper
          (rectangle (pos+ LINE/MEAN/START
                           (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
          ; cross
          (rectangle (pos+ LINE/MEAN/START (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 0.0))
                     (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; ascender lower
          (rectangle (pos+ LINE/MID-X/START +x WIDTH-STROKE-1/2)
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))))))

(define char:g
  (make-Char-3D-memoized
   #\g
   WIDTH-EM-1/2
   (λ () (combine
          ; top accent
          (rectangle (pos+ LINE/MEAN/CENTER-1/2
                           (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 0.0))
                     (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; curve x
          (cirque-x-3/8 #:basis 'y)
          ; connector
          (cirque-1/2 LINE/BASE/START HEIGHT-DESC-3/8 #:arc (arc 125.0 225.0) #:basis #f)
          ; curve desc
          (cirque-3/8 LINE/MID-DESC/START HEIGHT-DESC-5/16 #:arc circle-arc #:basis #f)))))

(define char:h
  (make-Char-3D-memoized
   #\h
   WIDTH-EM-1/2
   (λ () (combine
          ; ascender
          (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                     (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
          ; curve
          (cirque-x-3/8 #:arc (arc 180.0 0) #:basis 'x)
          ; end ascender
          (rectangle (pos+ LINE/BASE/END-3/8
                           (dir (- 0.0 WIDTH-STROKE-1/2)
                                (- HEIGHT-X-1/4)
                                0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))))))

(define char:i
  (make-Char-3D-memoized
   #\i
   WIDTH-EM-1/2
   (λ () (combine
          ; dot
          (rectangle (pos+ LINE/MEAN/CENTER-1/2 -y (* WIDTH-STROKE 1.5))
                     (dir WIDTH-STROKE-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; cap
          (rectangle (pos+ LINE/MEAN/CENTER-1/2
                           (dir (- WIDTH-STROKE-1/2 WIDTH-BASE/NARROW-1/2)
                                WIDTH-STROKE-1/2
                                0.0))
                     (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; ascender
          (rectangle LINE/MID-X/CENTER-1/2
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))
          ; base
          (rectangle (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-1/2)
                     (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:j
  (make-Char-3D-memoized
   #\j
   WIDTH-EM-1/2
   (λ () (combine
          ; dot
          (rectangle (pos+ LINE/MEAN/CENTER-1/2 -y (* WIDTH-STROKE 1.5))
                     (dir WIDTH-STROKE-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; cap
          (rectangle (pos+ LINE/MEAN/CENTER-1/2
                           (dir (- WIDTH-STROKE-1/2 WIDTH-BASE/NARROW-1/2)
                                WIDTH-STROKE-1/2
                                0.0))
                     (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; ascender
          (rectangle (pos+ LINE/MID-X/START +x (+ WIDTH-BASE/NARROW WIDTH-STROKE-1/2))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))
          (rectangle (pos+ LINE/BASE/START +x (+ WIDTH-BASE/NARROW WIDTH-STROKE-1/2))
                     (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/2 DEPTH-Z-1/2))
          ; descender
          (move-x (cirque-desc WIDTH-BASE/NARROW #:arc (arc 0.0 90.0) #:basis 'x)
                  WIDTH-STROKE)))))

(define char:k
  (make-Char-3D-memoized
   #\k
   WIDTH-EM-3/8
   (λ () (combine
          ; ascender
          (rectangle (pos+ LINE/MID/START +x WIDTH-STROKE-1/2)
                     (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
          ; diagonal upper
          (quad-thicc (pos+ LINE/MID-X/START
                            (dir WIDTH-STROKE (- WIDTH-STROKE-1/2) 0.0))
                      (pos+ LINE/MID-X/START
                            (dir WIDTH-STROKE (- WIDTH-DIAGONAL-BASE WIDTH-STROKE-1/2) 0.0))
                      (pos+ LINE/MEAN/START
                            (dir (+ WIDTH-STROKE WIDTH-EM-3/16) WIDTH-DIAGONAL-BASE 0.0))
                      (pos+ LINE/MEAN/START
                            (dir (+ WIDTH-STROKE WIDTH-EM-3/16) 0.0 0.0)))
          (move-x
           (combine
            ; dialogal lower
            (move-y (cirque-x-3/8 #:arc (arc -90.0 0.0) #:basis 'y) HEIGHT-X-1/2))
           (- WIDTH-STROKE WIDTH-EM-3/16))))))

(define char:l
  (make-Char-3D-memoized
   #\l
   WIDTH-EM-9/16
   (λ () (combine
          ; cap
          (rectangle (pos+ LINE/CAP/CENTER-1/2 -x
                           (- WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2))
                     (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; ascender
          (rectangle LINE/MID/CENTER-1/2
                     (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
          ; base
          (rectangle (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-1/2)
                     (dir WIDTH-BASE/NARROW WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define char:m
  (make-Char-3D-memoized
   #\m
   WIDTH-EM-13/16
   (λ () (combine
          ; ascender left
          (rectangle (pos+ LINE/MID-X/START
                           (dir WIDTH-STROKE-1/2 0.0 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))
          ; curve left
          (cirque-x-3/8 #:arc (arc 180.0 0) #:basis 'x)
          ; ascender middle
          (rectangle (pos+ LINE/BASE/START
                           (dir (- WIDTH-EM-3/8 WIDTH-STROKE-1/2) (- HEIGHT-X-1/4) 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
          ; curve right
          (move-x (cirque-x-3/8 #:arc (arc 180.0 0) #:basis 'x)
                  (- WIDTH-EM-3/8 WIDTH-STROKE))
          ; ascender end
          (rectangle (pos+ LINE/BASE/START
                           (dir (- (+ WIDTH-EM-3/8 WIDTH-EM-3/8) WIDTH-STROKE WIDTH-STROKE-1/2)
                                (- HEIGHT-X-1/4)
                                0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))))))

(define char:n
  (make-Char-3D-memoized
   #\n
   WIDTH-EM-1/2
   (λ () (combine
          ; ascender left
          (rectangle (pos+ LINE/MID-X/START
                           (dir WIDTH-STROKE-1/2 0.0 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))
          ; curve
          (cirque-x-3/8 #:arc (arc 180.0 0) #:basis 'x)
          ; ascender end
          (rectangle (pos+ LINE/BASE/END-3/8
                           (dir (- WIDTH-STROKE-1/2)
                                (- HEIGHT-X-1/4)
                                0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))))))

(define char:o
  (make-Char-3D-memoized
   #\o
   WIDTH-EM-1/2
   (λ () (cirque-x-3/8 #:basis 'x))))

(define char:p
  (make-Char-3D-memoized
   #\p
   WIDTH-EM-1/2
   (λ () (combine (cirque-x-3/8 #:basis 'x)
                  (rectangle (pos+ LINE/BASE/START +x WIDTH-STROKE-1/2)
                             (dir WIDTH-STROKE-1/2 HEIGHT-X DEPTH-Z-1/2))))))

(define char:q
  (make-Char-3D-memoized
   #\q
   WIDTH-EM-1/2
   (λ ()
     (combine
      (cirque-x-3/8 #:basis 'x)
      (rectangle (pos+ LINE/BASE/END-3/8 -x WIDTH-STROKE-1/2)
                 (dir WIDTH-STROKE-1/2 HEIGHT-X DEPTH-Z-1/2))))))

(define char:r
  (make-Char-3D-memoized
   #\r ;blah
   WIDTH-EM-3/8
   (λ ()
     (combine
      (rectangle (pos+ LINE/MID-X/START +x WIDTH-STROKE-1/2)
                 (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))
      (cirque (pos+ LINE/BASE/START +x WIDTH-EM-1/4)
              WIDTH-EM-1/4
              HEIGHT-X
              #:arc (arc -180.0 -90.0)
              #:basis 'x)))))

(define char:s
  (make-Char-3D-memoized
   #\s
   WIDTH-EM-7/16
   (λ ()
     (define position-angle      : Flonum  115.0)
     (define position-angle-r (degrees->radians position-angle))
     (define connector-angle-bot : Flonum -45.0)
     (define connector-angle-bot-r (degrees->radians connector-angle-bot))
     (define connector-angle-top (+ 180.0 connector-angle-bot))
     (define connector-angle-top-r (degrees->radians connector-angle-top))
     (define distance-offset (- WIDTH-STROKE-1/4))
     (define radius-x WIDTH-EM-3/16)
     (define radius-y HEIGHT-X-1/4)
     (define radius-position (cirque-radius position-angle-r radius-x radius-y))
     (define radius-connector (cirque-radius connector-angle-bot-r radius-x radius-y))


     (define connector-angle-rendered-r (cirque-angle-rendered connector-angle-bot-r radius-x radius-y))
     (define connector-angle-rendered (radians->degrees connector-angle-rendered-r))

     ; TODO: This is still slightly off. There is probably a good reason having
     ; to do with the squashed pipe not _really_ having a width of WIDTH-STROKE
     ; at the connector angle. It's ok at normal rendering sizes so I'm not
     ; gonna worry about trying to fix it for now.
     (define radius-connector-inner (- radius-connector WIDTH-STROKE))

     (define offset-x (+ distance-offset (* radius-position (abs (cos position-angle-r)))))
     (define offset-y (+ distance-offset (* radius-position (abs (sin position-angle-r)))))

     (define top-left (pos+ LINE/MID-X/START
                            (dir (* 2.0 offset-x) (- offset-y) 0.0)))
     (define top-center (pos+ top-left +x radius-x))
     (define bot-left (pos+ LINE/MID-X/START
                            (dir 0.0              offset-y     0.0)))
     (define bot-center (pos+ bot-left +x radius-x))

     (combine
      ; top curve
      (cirque top-center
              radius-x
              radius-y
              #:arc (arc connector-angle-top 330.0)
              #:basis connector-angle-top-r)
      ; connector
      (quad-thicc (pos+ top-center (dir (* -1.0 radius-connector-inner (cos connector-angle-rendered-r))
                                        (* -1.0 radius-connector-inner (sin connector-angle-rendered-r))
                                        0.0))
                  (pos+ top-center (dir (* radius-x (cos connector-angle-top-r))
                                        (* radius-y (sin connector-angle-top-r))
                                        0.0))
                  (pos+ bot-center (dir (* radius-connector-inner (cos connector-angle-rendered-r))
                                        (* radius-connector-inner (sin connector-angle-rendered-r))
                                        0.0))
                  (pos+ bot-center (dir (* radius-x (cos connector-angle-bot-r))
                                        (* radius-y (sin connector-angle-bot-r))
                                        0.0)))
      ; bottom curve
      (cirque bot-center
              radius-x
              radius-y
              #:arc (arc connector-angle-bot 150.0)
              #:basis connector-angle-bot-r)))))

(define char:t
  (make-Char-3D-memoized
   #\t 
   WIDTH-EM-3/8
   (λ () (combine
          ; ascender upper
          (rectangle (pos+ LINE/MID-Y/START +x WIDTH-STROKE-1/2)
                     (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z-1/2))
          ; cross
          (rectangle (pos+ LINE/MEAN/START
                           (dir (+ WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2)
                                WIDTH-STROKE-1/2
                                0.0))
                     (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; ascender lower
          (rectangle (pos+ LINE/MEAN/START (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
          ; curve
          (cirque-x-3/8 #:arc (arc 60.0 180.0) #:basis 'x)))))


(define char:u
  (make-Char-3D-memoized
   #\u
   WIDTH-EM-1/2
   (λ () (combine
          (rectangle (pos+ LINE/MEAN/START
                           (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
          (rectangle (pos+ LINE/MEAN/END-3/8
                           (dir (- 0.0 WIDTH-STROKE-1/2)
                                HEIGHT-X-1/4
                                0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
          (cirque-x-3/8 #:arc (arc 0.0 180.0) #:basis 'x)))))

(define char:v
  (make-Char-3D-memoized
   #\v
   WIDTH-EM-1/2
   (λ () (combine
          ; left
          (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                      (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                      (pos+ LINE/MEAN/START      +x WIDTH-STROKE)
                      (pos+ LINE/MEAN/START      +x 0.0))
          ; right
          (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                      (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                      (pos+ LINE/MEAN/END-3/8    -x 0.0)
                      (pos+ LINE/MEAN/END-3/8    -x WIDTH-STROKE))))))

(define char:w
  (make-Char-3D-memoized
   #\w
   WIDTH-EM-3/4
   (λ ()
     (define v (combine
                (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                            (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                            (pos+ LINE/MEAN/START      +x WIDTH-STROKE)
                            (pos+ LINE/MEAN/START      +x 0.0))
                (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                            (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                            (pos+ LINE/MEAN/END-3/8    -x 0.0)
                            (pos+ LINE/MEAN/END-3/8    -x WIDTH-STROKE))))
     (combine v (move-x v (- WIDTH-EM-3/8 WIDTH-STROKE))))))

(define char:x
  (make-Char-3D-memoized
   #\x
   WIDTH-EM-7/16
   (λ () (combine
          ; bottom-left to top-right
          (quad-thicc (pos+ LINE/BASE/START   +x 0.0)
                      (pos+ LINE/BASE/START   +x WIDTH-STROKE)
                      (pos+ LINE/MEAN/END-3/8 -x 0.0)
                      (pos+ LINE/MEAN/END-3/8 -x WIDTH-STROKE))
          ; bottom-right to top-left
          (quad-thicc (pos+ LINE/BASE/END-3/8 -x WIDTH-STROKE)
                      (pos+ LINE/BASE/END-3/8 -x 0.0)
                      (pos+ LINE/MEAN/START   +x WIDTH-STROKE)
                      (pos+ LINE/MEAN/START   +x 0.0))))))

(define char:y
  (make-Char-3D-memoized
   #\y
   WIDTH-EM-1/2
   (λ () (combine
          ; left
          (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                      (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                      (pos+ LINE/MEAN/START      +x WIDTH-STROKE)
                      (pos+ LINE/MEAN/START      +x 0.0))
          ; right
          (quad-thicc (pos+ LINE/BASE/CENTER-3/8 -x WIDTH-STROKE-1/2)
                      (pos+ LINE/BASE/CENTER-3/8 +x WIDTH-STROKE-1/2)
                      (pos+ LINE/MEAN/END-3/8    -x 0.0)
                      (pos+ LINE/MEAN/END-3/8    -x WIDTH-STROKE))
          ; descender
          (move (cirque-desc-3/8 #:arc (arc 0.0 90.0))
                (dir (- WIDTH-STROKE-1/2 WIDTH-EM-3/16) (- HEIGHT-DESC-1/2) 0.0))))))

(define char:z
  (make-Char-3D-memoized
   #\z
   WIDTH-EM-1/2
   (λ ()
     (define cf-top-inner (pos+ LINE/MEAN/END-3/8
                                (dir (- WIDTH-DIAGONAL-BASE) WIDTH-STROKE 0.0)))
     (define cf-top-outer (pos+ LINE/MEAN/END-3/8
                                (dir 0.0 WIDTH-STROKE 0.0)))
     (define cf-bottom-inner (pos+ LINE/BASE/START
                                   (dir WIDTH-DIAGONAL-BASE (- WIDTH-STROKE) 0.0)))
     (define cf-bottom-outer (pos+ LINE/BASE/START
                                   (dir 0.0 (- WIDTH-STROKE) 0.0)))
     (combine
      ; top bar
      (rectangle (pos+ LINE/MEAN/CENTER-3/8
                       (dir 0.0 WIDTH-STROKE-1/2 0.0))
                 (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
      ; cross
      (quad-thicc cf-top-outer
                  cf-top-inner
                  cf-bottom-outer
                  cf-bottom-inner)
      ; bottom bar
      (rectangle (pos+ LINE/BASE/CENTER-3/8
                       (dir 0.0 (- WIDTH-STROKE-1/2) 0.0))
                 (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))
