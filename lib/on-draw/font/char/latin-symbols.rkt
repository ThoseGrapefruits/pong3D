#lang typed/racket

(require pict3d
         "../statics/guides.rkt"
         "../statics/measurements.rkt"
         "../statics/types.rkt"
         "../util/cirque.rkt"
         "../util/quad-thicc.rkt")

(provide (all-defined-out))

; Safety thing to make sure we aren't accidentally using a character in the name
; that doesn't actually work.
(define symbol: #f)

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
                      HEIGHT-CAP-1/4
                      #:arc circle-arc
                      #:basis 'x)))))

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
                            #:arc (arc  90.0 330.0)
                            #:basis 'x)
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

(define symbol:%
  (Char-3D
   #\%
   WIDTH-EM-5/8
   (λ () (combine
          ; cross
          (quad-thicc (pos+ LINE/MEAN/START
                            (dir 0.0 (- HEIGHT-X-1/2 WIDTH-DIAGONAL-BASE)     0.0))
                      (pos+ LINE/MEAN/START
                            (dir 0.0 HEIGHT-X-1/2                             0.0))
                      (pos+ LINE/MEAN/END-1/2
                            (dir 0.0 (- HEIGHT-X-1/2)                         0.0))
                      (pos+ LINE/MEAN/END-1/2
                            (dir 0.0 (- (+ HEIGHT-X-1/2 WIDTH-DIAGONAL-BASE)) 0.0)))
          ; upper cirque
          (cirque-5/16 (pos+ LINE/CAP/START
                             (dir 0.0 (+ WIDTH-EM-3/16 WIDTH-STROKE-1/2) 0.0))
                       WIDTH-EM-3/16
                       #:arc circle-arc
                       #:basis #f)
          ; lower cirque
          (cirque-5/16 (pos+ LINE/BASE/START
                            (dir WIDTH-EM-3/16 (- WIDTH-EM-3/16) 0.0))
                      WIDTH-EM-3/16
                      #:arc circle-arc
                      #:basis #f)))))

(define symbol:^
  (Char-3D
   #\^
   WIDTH-EM-5/8
   (λ () (combine
          (quad-thicc LINE/MEAN/START
                      (pos+ LINE/MEAN/START +x WIDTH-DIAGONAL-SLIGHT-BASE)
                      (pos+ LINE/CAP/CENTER-1/2 +x WIDTH-DIAGONAL-SLIGHT-BASE-1/2)
                      (pos+ LINE/CAP/CENTER-1/2 -x WIDTH-DIAGONAL-SLIGHT-BASE-1/2))
          (quad-thicc (pos+ LINE/CAP/CENTER-1/2 +x WIDTH-DIAGONAL-SLIGHT-BASE-1/2)
                      (pos+ LINE/CAP/CENTER-1/2 -x WIDTH-DIAGONAL-SLIGHT-BASE-1/2)
                      (pos+ LINE/MEAN/END-1/2 -x WIDTH-DIAGONAL-SLIGHT-BASE)
                      LINE/MEAN/END-1/2)))))

(define symbol:&
  (Char-3D
   #\&
   WIDTH-EM-3/4
   (λ ()
   (define top-arc-angle-left -240.0)
   (define top-arc-angle-left-r (degrees->radians top-arc-angle-left))
   (define top-arc-radius-x WIDTH-EM-3/16)
   (define top-arc-radius-y HEIGHT-Y-3/8)
   (define top-arc-pos-left (pos+ LINE/MEAN/START (dir WIDTH-STROKE-1/2 (- top-arc-radius-y) 0.0)))
   (define top-arc-pos-center (pos+ top-arc-pos-left +x top-arc-radius-x))
   (define bottom-arc-angle-right 40.0)
   (define bottom-arc-angle-right-r (degrees->radians bottom-arc-angle-right))
   (define bottom-arc-radius-x WIDTH-EM-1/4)
   (define bottom-arc-radius-y (+ HEIGHT-X-1/2 WIDTH-STROKE-1/2))           ; from cirque-x-link
   (define bottom-arc-pos-left (pos+ LINE/MID-X/START -y WIDTH-STROKE-1/2)) ; from cirque-x-link
   (define bottom-arc-pos-center (pos+ bottom-arc-pos-left +x bottom-arc-radius-x))
   (combine
          ; top cirque
          (cirque-3/8 top-arc-pos-left top-arc-radius-y
                      #:arc (arc top-arc-angle-left -30.0)
                      #:basis 'x)
          ; ascender
          (quad-thicc (pos+ top-arc-pos-center
                            (dir (* (- top-arc-radius-x WIDTH-STROKE) (cos top-arc-angle-left-r))
                                 (* (- top-arc-radius-y WIDTH-STROKE) (sin top-arc-angle-left-r))
                                    0.0))
                      (pos+ top-arc-pos-center
                            (dir (* top-arc-radius-x (cos top-arc-angle-left-r))
                                 (* top-arc-radius-y (sin top-arc-angle-left-r))
                                 0.0))
                      (pos+ LINE/BASE/END-5/8 -y 0.0)
                      (pos+ LINE/BASE/END-5/8 -y WIDTH-DIAGONAL-SLIGHT-BASE))
          ; bottom tail
          (quad-thicc (pos+ bottom-arc-pos-center
                            (dir (* (- bottom-arc-radius-x WIDTH-STROKE)
                                    (cos bottom-arc-angle-right-r))
                                 (* (- bottom-arc-radius-y WIDTH-STROKE)
                                    (sin bottom-arc-angle-right-r))
                                    0.0))
                      (pos+ bottom-arc-pos-center
                            (dir (* bottom-arc-radius-x
                                    (cos bottom-arc-angle-right-r))
                                 (* bottom-arc-radius-y
                                    (sin bottom-arc-angle-right-r))
                                 0.0))
                      (pos+ LINE/MEAN/END-5/8 -y 0.0)
                      (pos+ LINE/MEAN/END-5/8 -x WIDTH-DIAGONAL-SLIGHT-BASE))
          ; bottom cirque
          (cirque-x-link-1/2 #:arc (arc bottom-arc-angle-right -110.0))))))

(define symbol:*
  (Char-3D
   #\*
   WIDTH-EM-3/8
   (λ ()
     (define center LINE/MID-Y/CENTER-1/4)
     (define radius WIDTH-EM-1/4)
     (define cross (combine
                    ; left arm
                    (quad-thicc (pos+ center (dir 0.0        WIDTH-STROKE-1/4     0.0))
                                (pos+ center (dir 0.0        (- WIDTH-STROKE-1/4) 0.0))
                                (pos+ center (dir (- radius) (- WIDTH-STROKE-1/2) 0.0))
                                (pos+ center (dir (- radius) WIDTH-STROKE-1/2     0.0)))
                    ; right arm
                    (quad-thicc (pos+ center (dir 0.0        (- WIDTH-STROKE-1/4) 0.0))
                                (pos+ center (dir 0.0        WIDTH-STROKE-1/4     0.0))
                                (pos+ center (dir (+ radius) WIDTH-STROKE-1/2     0.0))
                                (pos+ center (dir (+ radius) (- WIDTH-STROKE-1/2) 0.0)))))
     (combine
      cross
      (rotate-z/center cross 60.0)
      (rotate-z/center cross 120.0)))))

(define symbol:paren-left
  (Char-3D
   #\u0028
   WIDTH-EM-3/8
   (λ ()
     (define paren
       (combine
        ; curve top
        (cirque-y-3/8 #:arc (arc -180.0 -90.0) #:basis 'x)
        ; ascender y
        (rectangle (pos+ LINE/MEAN/START (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
        ; ascender x
        (rectangle (pos+ LINE/MID-X/START (dir WIDTH-STROKE-1/2 0.0 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))
        ; descender
        (rectangle (pos+ LINE/BASE/START (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/4 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/4 DEPTH-Z-1/2))
        ; curve bottom
        (cirque-desc-3/8 #:arc (arc 90.0 180.0) #:basis 'x)))
     (move-y paren (- WIDTH-STROKE)))))

(define symbol:paren-right
  (Char-3D
   #\u0029
   WIDTH-EM-3/8
   (λ ()
     (define curve-shift (- (- WIDTH-EM-3/8 WIDTH-STROKE)))
     (define paren
       (combine
        ; curve top
        (move-x (cirque-y-3/8 #:arc (arc -90.0 0.0) #:basis 'x) curve-shift)
        ; ascender y
        (rectangle (pos+ LINE/MEAN/START (dir WIDTH-STROKE-1/2 (- HEIGHT-Y-1/4) 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-Y-1/4 DEPTH-Z-1/2))
        ; ascender x
        (rectangle (pos+ LINE/MID-X/START (dir WIDTH-STROKE-1/2 0.0 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-X-1/2 DEPTH-Z-1/2))
        ; descender
        (rectangle (pos+ LINE/BASE/START (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/4 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/4 DEPTH-Z-1/2))
        ; curve bottom
        (move-x (cirque-desc-3/8 #:arc (arc 0.0 90.0) #:basis 'x) curve-shift)))
     (move-y paren (- WIDTH-STROKE)))))

(define symbol:bracket-square-left
  (Char-3D
   #\u005b
   WIDTH-EM-3/8
   (λ ()
     (define bracket
       (combine
        ; cap
        (rectangle (pos+ LINE/CAP/START (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 0.0))
                   (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
        ; ascender
        (rectangle (pos+ LINE/MID/START (dir WIDTH-STROKE-1/2 0.0 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
        ; descender
        (rectangle (pos+ LINE/MID-DESC/START (dir WIDTH-STROKE-1/2 0.0 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/2 DEPTH-Z-1/2))
        ; base
        (rectangle (pos+ LINE/DESC/START (dir WIDTH-BASE/NARROW-1/2 (- WIDTH-STROKE-1/2) 0.0))
                   (dir WIDTH-BASE/NARROW-1/2 (- WIDTH-STROKE-1/2) DEPTH-Z-1/2))))
     (move-y bracket (- WIDTH-STROKE)))))

(define symbol:bracket-square-right
  (Char-3D
   #\u005d
   WIDTH-EM-3/8
   (λ ()
     (define bracket
       (combine
        ; cap
        (rectangle (pos+ LINE/CAP/START
                         (dir (- WIDTH-STROKE WIDTH-BASE/NARROW-1/2) WIDTH-STROKE-1/2 0.0))
                   (dir WIDTH-BASE/NARROW-1/2 WIDTH-STROKE-1/2 DEPTH-Z-1/2))
        ; ascender
        (rectangle (pos+ LINE/MID/START (dir WIDTH-STROKE-1/2 0.0 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-CAP-1/2 DEPTH-Z-1/2))
        ; descender
        (rectangle (pos+ LINE/MID-DESC/START (dir WIDTH-STROKE-1/2 0.0 0.0))
                   (dir WIDTH-STROKE-1/2 HEIGHT-DESC-1/2 DEPTH-Z-1/2))
        ; base
        (rectangle (pos+ LINE/DESC/START
                         (dir (- WIDTH-STROKE WIDTH-BASE/NARROW-1/2) (- WIDTH-STROKE-1/2) 0.0))
                   (dir WIDTH-BASE/NARROW-1/2 (- WIDTH-STROKE-1/2) DEPTH-Z-1/2))))
     (move-y bracket (- WIDTH-STROKE)))))

; TODO figure out curly bracket descention
(define symbol:bracket-curly-left
  (Char-3D
   #\u007b
   WIDTH-EM-3/8
   (λ () 
     (define top-arc-angle 125.0)
     (define top-arc-angle-r (degrees->radians top-arc-angle))
     (define top-arc-radius-x WIDTH-EM-1/4)
     (define top-arc-radius-y HEIGHT-Y-1/2)
     (define top-arc-pos-left (pos+ LINE/MEAN/START (dir 0.0 (- top-arc-radius-y) 0.0)))
     (define top-arc-pos-center (pos+ top-arc-pos-left +x top-arc-radius-x))

     (define bottom-arc-angle -125.0)
     (define bottom-arc-angle-r (degrees->radians bottom-arc-angle))
     (define bottom-arc-radius-x WIDTH-EM-1/4)
     (define bottom-arc-radius-y (+ HEIGHT-X-1/2 WIDTH-STROKE-1/2))           ; from cirque-x-link
     (define bottom-arc-pos-left (pos+ LINE/MID-X/START -y WIDTH-STROKE-1/2)) ; from cirque-x-link
     (define bottom-arc-pos-center (pos+ bottom-arc-pos-left +x bottom-arc-radius-x))

     (define midline (pos+ LINE/MEAN/START -y WIDTH-STROKE-1/2))
     (define connector-end midline)
     (combine
      ; top arc
      (cirque-y-1/2 #:arc (arc top-arc-angle -70.0))
      ; joiner/tail top
      (quad-thicc (pos+ top-arc-pos-center
                        (dir (* top-arc-radius-x
                                (cos top-arc-angle-r))
                             (* top-arc-radius-y
                                (sin top-arc-angle-r))
                             0.0))
                  (pos+ top-arc-pos-center
                        (dir (* (- top-arc-radius-x WIDTH-STROKE)
                                (cos top-arc-angle-r))
                             (* (- top-arc-radius-y WIDTH-STROKE)
                                (sin top-arc-angle-r))
                             0.0))
                  (pos+ connector-end -y WIDTH-STROKE-1/4)
                  (pos+ connector-end +y WIDTH-STROKE-1/4))
      ; joiner/tail bottom
      (quad-thicc (pos+ bottom-arc-pos-center
                        (dir (* (- bottom-arc-radius-x WIDTH-STROKE)
                                (cos bottom-arc-angle-r))
                             (* (- bottom-arc-radius-y WIDTH-STROKE)
                                (sin bottom-arc-angle-r))
                             0.0))
                  (pos+ bottom-arc-pos-center
                        (dir (* bottom-arc-radius-x
                                (cos bottom-arc-angle-r))
                             (* bottom-arc-radius-y
                                (sin bottom-arc-angle-r))
                             0.0))
                  (pos+ connector-end -y WIDTH-STROKE-1/4)
                  (pos+ connector-end +y WIDTH-STROKE-1/4))
      ; bottom arc
      (cirque-x-link-1/2 #:arc (arc 70.0 bottom-arc-angle))))))

(define symbol:bracket-curly-right
  (Char-3D
   #\u007d
   WIDTH-EM-3/8
   (λ () 
     (define offset-x WIDTH-EM-1/8)
     (define arc-radius-x WIDTH-EM-1/4)
     (define top-arc-angle 55.0)
     (define top-arc-angle-r (degrees->radians top-arc-angle))
     (define top-arc-radius-y HEIGHT-Y-1/2)
     (define top-arc-pos-left (pos+ LINE/MEAN/START (dir WIDTH-STROKE-1/2 (- top-arc-radius-y) 0.0)))
     (define top-arc-pos-center (pos+ top-arc-pos-left +x (- arc-radius-x offset-x)))

     (define bottom-arc-angle -55.0)
     (define bottom-arc-angle-r (degrees->radians bottom-arc-angle))
     (define bottom-arc-radius-y (+ HEIGHT-X-1/2 WIDTH-STROKE-1/2))           ; from cirque-x-link
     (define bottom-arc-pos-left (pos+ LINE/MID-X/START -y WIDTH-STROKE-1/2)) ; from cirque-x-link
     (define bottom-arc-pos-center (pos+ bottom-arc-pos-left +x (- arc-radius-x offset-x)))

     (define midline (pos+ LINE/MEAN/START -y WIDTH-STROKE-1/2))
     (define connector-end (pos+ midline +x (+ arc-radius-x offset-x)))
     (combine
      ; top arc
      (move-x (cirque-y-1/2 #:arc (arc -110.0 top-arc-angle)) (- WIDTH-EM-1/8))
      ; joiner/tail top
      (quad-thicc (pos+ top-arc-pos-center
                        (dir (* (- arc-radius-x WIDTH-STROKE)
                                (cos top-arc-angle-r))
                             (* (- top-arc-radius-y WIDTH-STROKE)
                                (sin top-arc-angle-r))
                             0.0))
                  (pos+ top-arc-pos-center
                        (dir (* arc-radius-x
                                (cos top-arc-angle-r))
                             (* top-arc-radius-y
                                (sin top-arc-angle-r))
                             0.0))
                  (pos+ connector-end +y WIDTH-STROKE-1/4)
                  (pos+ connector-end -y WIDTH-STROKE-1/4))
      ; joiner/tail bottom
      (quad-thicc (pos+ bottom-arc-pos-center
                        (dir (* arc-radius-x
                                (cos bottom-arc-angle-r))
                             (* bottom-arc-radius-y
                                (sin bottom-arc-angle-r))
                             0.0))
                  (pos+ bottom-arc-pos-center
                        (dir (* (- arc-radius-x WIDTH-STROKE)
                                (cos bottom-arc-angle-r))
                             (* (- bottom-arc-radius-y WIDTH-STROKE)
                                (sin bottom-arc-angle-r))
                             0.0))
                  (pos+ connector-end +y WIDTH-STROKE-1/4)
                  (pos+ connector-end -y WIDTH-STROKE-1/4))
      ; bottom arc
      (move-x (cirque-x-link-1/2 #:arc (arc bottom-arc-angle 110.0)) (- WIDTH-EM-1/8))))))

(define symbol:-
  (Char-3D
   #\-
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

(define symbol:_
  (Char-3D
   #\_
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

(define symbol:/
  (Char-3D
   #\/
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

(define symbol:\
  (Char-3D
   #\\
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

(define symbol:vertical-line
  (Char-3D
   #\u007c
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

(define symbol:quote-single
  (Char-3D
   #\'
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

(define symbol:quote-double
  (Char-3D
   #\"
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

(define symbol::
  (Char-3D
   #\:
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

(define symbol:semicolon
  (Char-3D
   #\;
   WIDTH-EM-3/8
   (λ () (placeholder-tall WIDTH-EM-1/8))))

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
