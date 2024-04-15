#lang typed/racket

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
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
   (λ () (combine
          (cirque-x-3/8 #:arc (arc 60.0 0.0) #:basis 'x)
          (rectangle LINE/MID-X/CENTER-3/8
                     (dir WIDTH-EM-3/16 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

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
          ; diagonal lower
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
#\l WIDTH-EM-9/16
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
   (λ () (combine
          (cirque-x-3/8 #:basis 'x)
          (rectangle (pos+ LINE/BASE/END-3/8 -x WIDTH-STROKE-1/2)
                     (dir WIDTH-STROKE-1/2 HEIGHT-X DEPTH-Z-1/2))))))

(define char:r
  (make-Char-3D-memoized
  #\r
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
   WIDTH-EM-3/8
   (λ ()
              (define meeting-angle-bot -40.0)
              (define meeting-angle-bot-r (degrees->radians meeting-angle-bot))
              (define meeting-angle-top (+ 180.0 meeting-angle-bot))
              (define meeting-angle-top-r (degrees->radians meeting-angle-top))
              (define radius-x WIDTH-EM-3/16)
              (define radius-y (+ HEIGHT-X-1/4 WIDTH-STROKE-1/4))

              (define radius-x-mid (- radius-x WIDTH-STROKE-1/2))
              (define radius-y-mid (- radius-y WIDTH-STROKE-1/2))

              (define offset-x-bot (+ (* -1.0 radius-x-mid (cos meeting-angle-bot-r))
                                      (*  1.0 radius-x-mid (cos meeting-angle-top-r))))
              (define offset-y-bot (* radius-y-mid (sin meeting-angle-bot-r)))
              (define offset-y-top (* radius-y-mid (sin meeting-angle-top-r)))

             (combine
              ; top curve
              (cirque-3/8 (pos+ LINE/MID-X/START
                                (dir 0.0 (+ (- radius-y) offset-y-top) 0.0))
                          radius-y
                          #:arc (arc meeting-angle-top 300.0)
                          #:basis #f)
              ; bottom curve
              (cirque-3/8 (pos+ LINE/MID-X/START
                                (dir offset-x-bot (+ radius-y offset-y-bot) 0.0))
                          radius-y
                          #:arc (arc meeting-angle-bot 120.0)
                          #:basis #f)))))

(define char:t
  (make-Char-3D-memoized
#\t
           WIDTH-EM-1/2
           (λ () (combine
                  ; ascender upper
                  (rectangle LINE/MID-Y/CENTER-3/8
                             (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z-1/2))
                  ; cross
                  (rectangle (pos+ LINE/MEAN/CENTER-3/8
                                  (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 0.0))
                             (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
                  ; ascender lower
                  (rectangle (pos+ LINE/MEAN/CENTER-3/8 +y HEIGHT-X-1/4)
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
                  ; curve
                  (move-x (cirque-x-3/8 #:arc (arc 60.0 180.0) #:basis 'x)
                          (- WIDTH-EM-3/16 WIDTH-STROKE-1/2))))))

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
