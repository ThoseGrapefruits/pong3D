#lang typed/racket/base

(require racket/file
         (only-in racket/list empty? first second)
         (only-in racket/match match-define)
         (only-in racket/math exact-round nonnegative-integer?)
         (only-in typed-map map)
         typed/racket/gui/base
         "../preferences/preferences.rkt"
         "./rsound.rkt")

(provide
 note-to-frequency
 (struct-out Pong-Sound)
 rs-play
 rs-play-random
 rs-stop
 Sound-Category

 SOUND-ENDGAME
 SOUND-MUSIC-BACKGROUND
 SOUND-SCORE-ON-OPPONENT
 SOUND-SCORE-ON-PLAYER
 SOUND-STARTUP
 SOUNDS-BALL-BOUNCE-OPPONENT
 SOUNDS-BALL-BOUNCE-PLAYER
 SOUNDS-BALL-BOUNCE-WALL
 volume-for
 volume-global
 volume-main)

; Fixed global volume scale, since generated sounds are very very loud.
(: volume-global : Nonnegative-Flonum)
(define volume-global 0.5)

; User-configurable main volume
(: volume-main : -> Flonum)
(define (volume-main) (get-pref-flonum 'volume-main (λ () 0.7)))

(: volume-for : Sound-Category -> Flonum)
(define (volume-for category)
  (cond [(eq? 'effect category) (get-pref-flonum 'volume-effects (λ () 0.7))]
        [(eq? 'music  category) (get-pref-flonum 'volume-music   (λ () 1.0))]
        [else (error 'volume-for "unknown category: ~s" category)]))

(define-type Sound-Category (U 'effect 'music))

(struct Pong-Sound
  ([category : Sound-Category]
   [name : Symbol]
   [sound : RSound])
  #:transparent)

(define-type Pong-Sounds (Listof Pong-Sound))

;; PLAY/STOP

(: path-cache (Mutable-HashTable String Path))
(define path-cache (make-hash))

(: rs-play (->* (Pong-Sound) (Boolean) Thread))
(define (rs-play sound [loop #f])
  ; TODO would be good to pull this over but it's an internal function in rsound
  ; (rs:check-below-threshold sound 2.0)

  (define (get-path)
    (define volume (* volume-global
                      (min 1.0 (volume-main))
                      (min 1.0 (volume-for (Pong-Sound-category sound)))))
    (define cache-key (format "~s-~s"
                              (symbol->string (Pong-Sound-name sound))
                              volume))
    (define cached-path (hash-ref path-cache cache-key #f))
    (define path (cond [cached-path cached-path]
                       [else
                        ; ~v gets used by make-temporary-file
                        (define filename
                          (format "pong3d-sound-~s-~~v.wav" cache-key))
                        (define path
                          (make-temporary-file filename))
                        (hash-set! path-cache cache-key path)
                        path]))
    (rs:write (rs:scale volume (Pong-Sound-sound sound)) path)
    path)

  (: play (-> Boolean))
  (define play
    (if loop
        (λ () (play-sound (get-path) #f) (play))
        (λ () (play-sound (get-path) #f))))

  (thread play))

(: rs-play-random (->* (Pong-Sounds) (Boolean) Thread))
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

(: SOUNDS-BALL-BOUNCE-OPPONENT Pong-Sounds)
(define SOUNDS-BALL-BOUNCE-OPPONENT
  (for/list : Pong-Sounds
    ([n (in-range -5 5)])
    (Pong-Sound 'effect
                'ball-bounce-player
                (rs:make-ding (assert (+ (note-to-frequency -1) (* n 1)) nonnegative-integer?)))))

(: SOUNDS-BALL-BOUNCE-PLAYER Pong-Sounds)
(define SOUNDS-BALL-BOUNCE-PLAYER
  (for/list : Pong-Sounds
    ([n (in-range -5 5)])
    (Pong-Sound 'effect
                'ball-bounce-player
                (rs:make-ding (assert (+ (note-to-frequency 3) (* n 1)) nonnegative-integer?)))))

(: SOUNDS-BALL-BOUNCE-WALL Pong-Sounds)
(define SOUNDS-BALL-BOUNCE-WALL
  (for/list : Pong-Sounds
    ([n (in-range -5 5)])
    (Pong-Sound 'effect
                'ball-bounce-wall
                (rs:make-ding (assert (+ (note-to-frequency 5) (* n 1)) nonnegative-integer?)))))

(: SOUND-ENDGAME Pong-Sound)
(define SOUND-ENDGAME
  (Pong-Sound 'music
              'endgame
              (rs:append*
               (for*/list : (Listof RSound)
                 ([note-dur (in-list '((8 0.5) (7 0.5) (1 0.5) (3 1)))])
                 (define note (first note-dur))
                 (define dur (second note-dur))
                 (rs:synth-note "vgame" 49 (+ 52 note)
                                (exact-round
                                 (* (rs:default-sample-rate) dur)))))))

(: SOUND-MUSIC-BACKGROUND Pong-Sound)
(define SOUND-MUSIC-BACKGROUND
  (Pong-Sound 'music
              'music-background
              (rs:overlay
               (rs:append*
                (for*/list : (Listof RSound) ([i (in-range 20)]
                                              [j (in-list '(0 2 3 7))])
                  (rs:synth-note "vgame" 49 (+ 60 j)
                                 (exact-round
                                  (* (rs:default-sample-rate) (/ dur 4))))))
               (rs:append*
                (for*/list : (Listof RSound) ([i (in-range 5)]
                                              [j (in-list '(8 3 4 3))])
                  (rs:synth-note "vgame" 49 (+ 52 j)
                                 (exact-round
                                  (* (rs:default-sample-rate) dur))))))))

(: SOUND-SCORE-ON-OPPONENT Pong-Sound)
(define SOUND-SCORE-ON-OPPONENT
  (Pong-Sound 'effect
              'score-on-opponent
              (rs:append*
               (for*/list : (Listof RSound)
                 ([j (in-list '(2 3 7))])
                 (rs:synth-note "vgame" 49 (+ 68 j)
                                (exact-round
                                 (* (rs:default-sample-rate) (/ dur 4))))))))

(: SOUND-SCORE-ON-PLAYER Pong-Sound)
(define SOUND-SCORE-ON-PLAYER
  (Pong-Sound 'effect
              'score-on-player
              (rs:append*
               (for*/list : (Listof RSound)
                 ([j (in-list '(7 3 2))])
                 (rs:synth-note "vgame" 49 (+ 52 j)
                                (exact-round
                                 (* (rs:default-sample-rate) (/ dur 4))))))))

(: SOUND-STARTUP Pong-Sound)
(define SOUND-STARTUP
  (Pong-Sound 'music
              'startup
              (rs:append*
               (for*/list : (Listof RSound)
                 ([j (in-list '(2 3 7 6))])
                 (rs:synth-note "vgame" 49 (+ 68 j)
                                (exact-round
                                 (* (rs:default-sample-rate) (/ dur 2))))))))
