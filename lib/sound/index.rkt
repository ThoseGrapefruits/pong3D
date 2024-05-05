#lang typed/racket

(require typed/racket/gui/base)

(module wrapper racket
  (require (prefix-in rs: rsound))
  (provide
   rs:andplay
   rs:default-sample-rate
   rs:ding
   rs:make-ding
   rs:play
   rs:rs-overlay
   rs:rs-append*
   rs:rs-write
   rs:rsound
   rs:rsound?
   rs:rs-scale
   rs:synth-note
   rs:stop))

(require/typed
 'wrapper
 [#:opaque RSound rs:rsound?]
 [rs:default-sample-rate (-> Positive-Real)]
 [rs:ding RSound]
 [rs:make-ding (-> Integer RSound)]
 [rs:rs-append* (-> (Sequenceof RSound) RSound)]
 [rs:rs-overlay (-> RSound RSound RSound)]
 [rs:rs-write (-> RSound Path Void)]
 [rs:rs-scale (-> Real RSound RSound)]
 [rs:stop (-> Void)]
 [rs:synth-note (-> String Number Natural Natural RSound)])

(provide
 rs-play
 rs-play-random
 rs-stop

 SOUND-DING
 SOUND-ENDGAME
 SOUND-MUSIC
 SOUND-SCORE-ON-OPPONENT
 SOUND-SCORE-ON-PLAYER
 SOUND-STARTUP
 SOUNDS-BALL-BOUNCE-OPPONENT
 SOUNDS-BALL-BOUNCE-PLAYER
 SOUNDS-BALL-BOUNCE-WALL)

;; PLAY/STOP

(: path-cache (Mutable-HashTable RSound Path))
(define path-cache (make-hash))

(: rs-play (->* (RSound) (Boolean) Thread))
(define (rs-play sound [loop #f])
  ; TODO would be good to pull this over but it's an internal function in rsound
  ; (rs:check-below-threshold sound 2.0)
  (define cached-path (hash-ref path-cache sound #f))
  (define path (or cached-path (make-temporary-file "pong3d-sound-~a.wav")))
  (hash-set! path-cache sound path)
  (rs:rs-write sound path)
  (: play (-> Boolean))
  (define play (if loop
                   (λ () (play-sound path #f) (play))
                   (λ () (play-sound path #f))))
  (thread play))

(: rs-play-random (->* ((Listof RSound)) (Boolean) Thread))
(define (rs-play-random sounds [loop #f])
  (define sound (list-ref sounds (random 0 (length sounds))))
  (rs-play sound loop))

(: rs-stop (-> Void))
(define rs-stop rs:stop)

; CONSTANTS — SOUND

(: bpm Positive-Real)
(define bpm 150)

(: dur Positive-Real)
(define dur (max 0.01 (/ 60 bpm)))

(: SOUND-DING RSound)
(define SOUND-DING rs:ding)

; Frequencies based on note function w/ middle C @ 440Hz
; y = 440 * 2^(x / 12)
;
;  C  C D D E  F  F G G A A B  C
;     #   #       #   #   #
;  0    2   4  5    7   9   11 12
; -11  -9   -7 -6  -5  -3   -1  0
; │  ┃ ┃ ┃ ┃  │  ┃ ┃ ┃ ┃ ┃ ┃  │  ┃ 
; │  ┃ ┃ ┃ ┃  │  ┃ ┃ ┃ ┃ ┃ ┃  │  ┃ 
; │  ┃ ┃ ┃ ┃  │  ┃ ┃ ┃ ┃ ┃ ┃  │  ┃ 
; │  ┃ ┃ ┃ ┃  │  ┃ ┃ ┃ ┃ ┃ ┃  │  ┃ 
; │  ┃ ┃ ┃ ┃  │  ┃ ┃ ┃ ┃ ┃ ┃  │  ┃ 
; │  ┗┯┛ ┗┯┛  │  ┗┯┛ ┗┯┛ ┗┯┛  │  ┗┯
; │   │   │   │   │   │   │   │   │
; │   │   │   │   │   │   │   │   │
; │   │   │   │   │   │   │   │   │
; │   │   │   │   │   │   │   │   │
; │   │   │   │   │   │   │   │   │
; └───┴───┴───┴───┴───┴───┴───┴───┘

(: note-to-frequency : Integer -> Nonnegative-Integer)
(define (note-to-frequency n)
  (exact-round (* 440 (expt 2 (/ (exact->inexact n) 12)))))

(: SOUNDS-BALL-BOUNCE-OPPONENT (Listof RSound))
(define SOUNDS-BALL-BOUNCE-OPPONENT
  (for/list : (Listof RSound)
    ([n (in-range -5 5)])
    (rs:rs-scale 0.5 (rs:make-ding (+ (note-to-frequency -1) (* n 1))))))

(: SOUNDS-BALL-BOUNCE-PLAYER (Listof RSound))
(define SOUNDS-BALL-BOUNCE-PLAYER
  (for/list : (Listof RSound)
    ([n (in-range -5 5)])
    (rs:rs-scale 0.5 (rs:make-ding (+ (note-to-frequency 3) (* n 1))))))

(: SOUNDS-BALL-BOUNCE-WALL (Listof RSound))
(define SOUNDS-BALL-BOUNCE-WALL
(for/list : (Listof RSound)
  ([n (in-range -5 5)])
  (rs:rs-scale 0.5 (rs:make-ding (+ (note-to-frequency 5) (* n 1))))))

(: SOUND-ENDGAME RSound)
(define SOUND-ENDGAME
  (rs:rs-append*
   (for*/list : (Listof RSound) ([note-dur (in-list '((8 0.5) (7 0.5) (1 0.5) (3 1)))])
     (define note (first note-dur))
     (define dur (second note-dur))
     (rs:synth-note "vgame" 49 (+ 52 note)
                    (exact-round
                     (* (rs:default-sample-rate) dur))))))

(: SOUND-MUSIC RSound)
(define SOUND-MUSIC
  (rs:rs-overlay
   (rs:rs-append*
    (for*/list : (Listof RSound) ([i (in-range 20)]
                                  [j (in-list '(0 2 3 7))])
      (rs:synth-note "vgame" 49 (+ 60 j)
                     (exact-round
                      (* (rs:default-sample-rate) (/ dur 4))))))
   (rs:rs-append*
    (for*/list : (Listof RSound) ([i (in-range 5)]
                                  [j (in-list '(8 3 4 3))])
      (rs:synth-note "vgame" 49 (+ 52 j)
                     (exact-round
                      (* (rs:default-sample-rate) dur)))))))

(: SOUND-SCORE-ON-OPPONENT RSound)
(define SOUND-SCORE-ON-OPPONENT
  (rs:rs-append*
   (for*/list : (Listof RSound)
     ([j (in-list '(2 3 7))])
     (rs:synth-note "vgame" 49 (+ 68 j)
                    (exact-round
                     (* (rs:default-sample-rate) (/ dur 4)))))))

(: SOUND-SCORE-ON-PLAYER RSound)
(define SOUND-SCORE-ON-PLAYER
  (rs:rs-append*
   (for*/list : (Listof RSound)
     ([j (in-list '(7 3 2))])
     (rs:synth-note "vgame" 49 (+ 52 j)
                    (exact-round
                     (* (rs:default-sample-rate) (/ dur 4)))))))

(: SOUND-STARTUP RSound)
(define SOUND-STARTUP
  (rs:rs-append*
   (for*/list : (Listof RSound)
     ([j (in-list '(2 3 7 6))])
     (rs:synth-note "vgame" 49 (+ 68 j)
                    (exact-round
                     (* (rs:default-sample-rate) (/ dur 2)))))))
