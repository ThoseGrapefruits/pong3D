#lang typed/racket

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt"
         "../util/quad-thicc.rkt")

(provide (all-defined-out))

(define symbol:?
  (Char-3D #\?
           WIDTH-EM-3/4
           (λ () (combine
                  ; arc
                  (cirque-y-1/2 #:arc (arc -165.0 90.0))
                  ; ascender (taper)
                  (quad-thicc (pos+ LINE/MEAN/CENTER-1/2
                                    (dir WIDTH-STROKE-1/2 (- WIDTH-STROKE) 0.0))
                              (pos+ LINE/MEAN/CENTER-1/2
                                    (dir (- WIDTH-STROKE-1/2) (- WIDTH-STROKE) 0.0))
                              (pos+ LINE/MID-X/CENTER-1/2 -x WIDTH-STROKE-1/4)
                              (pos+ LINE/MID-X/CENTER-1/2 +x WIDTH-STROKE-1/4))
                  ; dot
                  (cylinder (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-3/8)
                            (dir WIDTH-STROKE-3/4 WIDTH-STROKE-3/4 DEPTH-Z-1/2))))))

(define symbol:!
  (Char-3D #\!
           WIDTH-EM-3/4
           (λ () (combine
                  ; ascender top
                  (rectangle (pos+ LINE/MID-Y/CENTER-1/2 +y 0.0)
                             (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/2 DEPTH-Z-1/2))
                  ; ascender bottom (taper)
                  (quad-thicc (pos+ LINE/MEAN/CENTER-1/2 +x WIDTH-STROKE-1/2)
                              (pos+ LINE/MEAN/CENTER-1/2 -x WIDTH-STROKE-1/2)
                              (pos+ LINE/MID-X/CENTER-1/2 -x WIDTH-STROKE-1/4)
                              (pos+ LINE/MID-X/CENTER-1/2 +x WIDTH-STROKE-1/4))
                  ; dot
                  (cylinder (pos+ LINE/BASE/CENTER-1/2 -y WIDTH-STROKE-3/8)
                            (dir WIDTH-STROKE-3/4 WIDTH-STROKE-3/4 DEPTH-Z-1/2))))))


(define symbol:+
  (Char-3D #\+
           WIDTH-EM-1/2
           (λ () (combine
                  ; ascender
                  (rectangle LINE/MEAN/CENTER-3/8
                             (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))
                  ; cross
                  (rectangle LINE/MEAN/CENTER-3/8
                             (dir HEIGHT-X-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define symbol:=
  (Char-3D #\=
           WIDTH-EM-1/2
           (λ () (combine
                  ; top
                  (rectangle (pos+ LINE/MEAN/CENTER-3/8 -y (- HEIGHT-X-1/2 WIDTH-STROKE-1/2))
                             (dir HEIGHT-X-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
                  ; top
                  (rectangle (pos+ LINE/MEAN/CENTER-3/8 +y (- HEIGHT-X-1/2 WIDTH-STROKE-1/2))
                             (dir HEIGHT-X-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

(define symbol:@
  (Char-3D
   #\@
   WIDTH-EM-5/8
   (λ () (combine
          ; arc top
          (move-y (cirque-y-1/2 #:arc (arc -180.0 0.0)) HEIGHT-Y-1/4)
          ; connector left y
          (rectangle (pos+ LINE/MEAN/START
                           (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/8) 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/8 DEPTH-Z-1/2))
          ; connector left x
          (rectangle (pos+ LINE/MEAN/START
                           (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-X-1/4 DEPTH-Z-1/2))
          ; arc bottom
          (cirque-x-1/2 #:arc (arc 60.0 180.0))
          ; connector right y
          (rectangle (pos+ LINE/MEAN/END-1/2
                           (dir (- WIDTH-STROKE-1/2) (- HEIGHT-Y-1/8) 0.0))
                     (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/8 DEPTH-Z-1/2))
          ; inner circle
          (cirque-3/8 (pos+ LINE/MEAN/END-1/2 -x WIDTH-EM-3/8)
                      HEIGHT-CAP-1/4)))))

(define symbol:$
  (Char-3D
   #\$
   WIDTH-EM-5/8
   (λ () (combine
          ; ascender
          (rectangle (pos+ LINE/MID/CENTER-1/2 +y WIDTH-STROKE-1/4)
                     (dir WIDTH-STROKE-1/2 (+ HEIGHT-CAP-1/2 WIDTH-STROKE-1/4) DEPTH-Z-1/2))
          ; upper
          (cirque-1/2 (pos+ LINE/MID-Y/START +y WIDTH-STROKE-1/2)
                            (- HEIGHT-Y-1/2 WIDTH-STROKE-1/4)
                            #:arc (arc  90.0 330.0))
          ; lower
          (cirque-x-link-1/2 #:arc (arc -90.0 -210.0))))))


(define symbol:#
  (Char-3D
   #\#
   WIDTH-EM-5/8
   (λ () (combine
          ; ascender left
          (rectangle (pos+ LINE/MID/START (dir WIDTH-EM-1/8 WIDTH-STROKE-1/4 0.0))
                     (dir WIDTH-STROKE-1/2 (- HEIGHT-CAP-1/2 WIDTH-STROKE-1/4) DEPTH-Z-1/2))
          ; ascender right
          (rectangle (pos+ LINE/MID/START (dir WIDTH-EM-3/8 WIDTH-STROKE-1/4 0.0))
                     (dir WIDTH-STROKE-1/2 (- HEIGHT-CAP-1/2 WIDTH-STROKE-1/4) DEPTH-Z-1/2))
          ; cross top
          (rectangle (pos+ LINE/MEAN/CENTER-1/2 -y HEIGHT-X-1/2)
                     (dir WIDTH-EM-1/4 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
          ; cross bottom
          (rectangle (pos+ LINE/MEAN/CENTER-1/2 +y HEIGHT-X-1/2)
                     (dir WIDTH-EM-1/4 WIDTH-STROKE-1/2 DEPTH-Z-1/2))))))

; %

; ^

; &

; *

; (

; )

; [

; ]

; {

; }


; -

; _

; /

; \

; |


; '

; "

; :

; ;

(define symbol:comma
  (Char-3D #\,
           WIDTH-EM-1/4
           (λ () (combine
                  ; circle
                  (cylinder (pos+ LINE/BASE/CENTER-1/4 -y WIDTH-STROKE-3/8)
                            WIDTH-STROKE-3/4)
                            ; tail
                  (quad-thicc (pos+ LINE/BASE/CENTER-1/4
                                    (dir (- WIDTH-STROKE-1/4) 0.0 0.0))
                              (pos+ LINE/BASE/CENTER-1/4
                                    (dir (- WIDTH-STROKE-1/4) WIDTH-STROKE 0.0))
                              (pos+ LINE/BASE/CENTER-1/4
                                    (dir WIDTH-STROKE-1/4     WIDTH-STROKE 0.0))
                              (pos+ LINE/BASE/CENTER-1/4
                                    (dir WIDTH-STROKE-3/4     0.0          0.0)))))))

(define symbol:dot
  (Char-3D #\.
           WIDTH-EM-1/4
           (λ () (cylinder (pos+ LINE/BASE/CENTER-1/4 -y WIDTH-STROKE-3/8)
                         WIDTH-STROKE-3/4))))
