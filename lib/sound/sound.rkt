#lang typed/racket/base

(require racket/file
         racket/list
         racket/math
         typed/racket/gui/base
         "../preferences/preferences.rkt")

(module wrapper racket/base
  (require rsound)
  (provide
   (prefix-out rs: andplay)
   (prefix-out rs: default-sample-rate)
   (struct-out rsound)
   (prefix-out rs: ding)
   (prefix-out rs: make-ding)
   (prefix-out rs: play)
   (rename-out [rs-overlay rs:overlay]
               [rs-append* rs:append*]
               [rs-write   rs:write]
               [rs-scale   rs:scale])
   (prefix-out rs: synth-note)
   (prefix-out rs: stop)))

(: volume-master : Flonum)
(define volume-master (get-preference-pong3d-flonum 'volume-master (λ () 0.5)))

(require/typed
 'wrapper
 [#:struct rsound ([data : (Vectorof Real)]
                      [start : Nonnegative-Real]
                      [stop : Nonnegative-Real]
                      [sample-rate : Nonnegative-Real])
                      #:type-name RSound]
 [rs:default-sample-rate (-> Positive-Real)]
 [rs:ding RSound]
 [rs:make-ding (-> Integer RSound)]
 [rs:append* (-> (Sequenceof RSound) RSound)]
 [rs:overlay (-> RSound RSound RSound)]
 [rs:write (-> RSound Path Void)]
 [rs:scale (-> Real RSound RSound)]
 [rs:stop (-> Void)]
 [rs:synth-note (-> String Number Natural Natural RSound)])

(provide
 rs-play
 rs-play-random
 rs-stop

 SOUND-ENDGAME
 SOUND-MUSIC
 SOUND-SCORE-ON-OPPONENT
 SOUND-SCORE-ON-PLAYER
 SOUND-STARTUP
 SOUNDS-BALL-BOUNCE-OPPONENT
 SOUNDS-BALL-BOUNCE-PLAYER
 SOUNDS-BALL-BOUNCE-WALL)

(struct PongSound ([name : Symbol]
                   [sound : RSound]))

(define-type PongSounds (Listof PongSound))

;; PLAY/STOP

(: path-cache (Mutable-HashTable String Path))
(define path-cache (make-hash))

(: rs-play (->* (PongSound) (Boolean) Thread))
(define (rs-play sound [loop #f])
  ; TODO would be good to pull this over but it's an internal function in rsound
  ; (rs:check-below-threshold sound 2.0)
  (define (get-path)
    (define volume volume-master)
    (define cache-key (format "~s-~s" (symbol->string (PongSound-name sound)) volume))
    (define cached-path (hash-ref path-cache cache-key #f))
    (define path (or cached-path
                     (make-temporary-file (format "pong3d-sound-~s-~~a.wav" cache-key))))
    (hash-set! path-cache cache-key path)
    (rs:write (rs:scale volume (PongSound-sound sound)) path)
    path)

  (: play (-> Boolean))
  (define play
    (if loop
        (λ () (play-sound (get-path) #f) (play))
        (λ () (play-sound (get-path) #f))))

  (thread play))

(: rs-play-random (->* (PongSounds) (Boolean) Thread))
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

(: SOUNDS-BALL-BOUNCE-OPPONENT PongSounds)
(define SOUNDS-BALL-BOUNCE-OPPONENT
  (for/list : PongSounds
    ([n (in-range -5 5)])
    (PongSound 'ball-bounce-player
               (rs:make-ding (+ (note-to-frequency -1) (* n 1))))))

(: SOUNDS-BALL-BOUNCE-PLAYER PongSounds)
(define SOUNDS-BALL-BOUNCE-PLAYER
  (for/list : PongSounds
    ([n (in-range -5 5)])
    (PongSound 'ball-bounce-player
               (rs:make-ding (+ (note-to-frequency 3) (* n 1))))))

(: SOUNDS-BALL-BOUNCE-WALL PongSounds)
(define SOUNDS-BALL-BOUNCE-WALL
(for/list : PongSounds
  ([n (in-range -5 5)])
  (PongSound 'ball-bounce-wall
             (rs:make-ding (+ (note-to-frequency 5) (* n 1))))))

(: SOUND-ENDGAME PongSound)
(define SOUND-ENDGAME
  (PongSound 'endgame
             (rs:append*
              (for*/list : (Listof RSound)
                ([note-dur (in-list '((8 0.5) (7 0.5) (1 0.5) (3 1)))])
                (define note (first note-dur))
                (define dur (second note-dur))
                (rs:synth-note "vgame" 49 (+ 52 note)
                               (exact-round
                                (* (rs:default-sample-rate) dur)))))))

(: SOUND-MUSIC PongSound)
(define SOUND-MUSIC
  (PongSound 'music
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

(: SOUND-SCORE-ON-OPPONENT PongSound)
(define SOUND-SCORE-ON-OPPONENT
  (PongSound 'score-on-opponent
             (rs:append*
              (for*/list : (Listof RSound)
                ([j (in-list '(2 3 7))])
                (rs:synth-note "vgame" 49 (+ 68 j)
                               (exact-round
                                (* (rs:default-sample-rate) (/ dur 4))))))))

(: SOUND-SCORE-ON-PLAYER PongSound)
(define SOUND-SCORE-ON-PLAYER
  (PongSound 'score-on-player
             (rs:append*
              (for*/list : (Listof RSound)
                ([j (in-list '(7 3 2))])
                (rs:synth-note "vgame" 49 (+ 52 j)
                               (exact-round
                                (* (rs:default-sample-rate) (/ dur 4))))))))

(: SOUND-STARTUP PongSound)
(define SOUND-STARTUP
  (PongSound 'startup
             (rs:append*
              (for*/list : (Listof RSound)
                ([j (in-list '(2 3 7 6))])
                (rs:synth-note "vgame" 49 (+ 68 j)
                               (exact-round
                                (* (rs:default-sample-rate) (/ dur 2))))))))
