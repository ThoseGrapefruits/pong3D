#lang typed/racket/base

(require racket/bool
         racket/file
         racket/list
         racket/match
         racket/math
         typed-map
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
   (prefix-out rs: make-tone)
   (prefix-out rs: play)
   (rename-out [rs-overlay rs:overlay]
               [rs-append* rs:append*]
               [rs-write   rs:write]
               [rs-scale   rs:scale])
   (prefix-out rs: synth-note)
   (prefix-out rs: stop)))

; Fixed global volume scale, since generated sounds are very very loud.
(: volume-global : Flonum)
(define volume-global 0.5)

; User-configurable main volume
(: volume-main : -> Flonum)
(define (volume-main) (get-pref-flonum 'volume-main (λ () 0.7)))

(: volume-for : Sound-Category -> Flonum)
(define (volume-for category)
  (cond [(symbol=? 'effect category) (get-pref-flonum 'volume-effects (λ () 0.7))]
        [(symbol=? 'music  category) (get-pref-flonum 'volume-music   (λ () 1.0))]
        [else (error 'volume-for "unknown category: ~s" category)]))

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
 SOUND-MUSIC-BACKGROUND
 SOUND-SCORE-ON-OPPONENT
 SOUND-SCORE-ON-PLAYER
 SOUND-STARTUP
 SOUNDS-BALL-BOUNCE-OPPONENT
 SOUNDS-BALL-BOUNCE-PLAYER
 SOUNDS-BALL-BOUNCE-WALL)

(define-type Sound-Category (U 'effect 'music))

(struct PongSound
  ([category : Sound-Category]
   [name : Symbol]
   [sound : RSound])
  #:transparent)

(define-type PongSounds (Listof PongSound))

;; PLAY/STOP

(: path-cache (Mutable-HashTable String Path))
(define path-cache (make-hash))

(: rs-play (->* (PongSound) (Boolean) Thread))
(define (rs-play sound [loop #f])
  ; TODO would be good to pull this over but it's an internal function in rsound
  ; (rs:check-below-threshold sound 2.0)
  (define (get-path)
    (define volume (* volume-global
                      (min 1.0 (volume-main))
                      (min 1.0 (volume-for (PongSound-category sound)))))
    (define cache-key (format "~s-~s"
                              (symbol->string (PongSound-name sound))
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

(: note-name : Nonnegative-Integer -> String)
(define (note-name n)
  (cond [(= n 0)  "C_" ]
        [(= n 1)  "C#"]
        [(= n 2)  "D_"]
        [(= n 3)  "D#"]
        [(= n 4)  "E_"]
        [(= n 5)  "F_"]
        [(= n 6)  "F#"]
        [(= n 7)  "G_"]
        [(= n 8)  "G#"]
        [(= n 9)  "A_"]
        [(= n 10) "A#"]
        [(= n 11) "B_"]
        [else (error "Unknown note index: ~a" n)]))

(: octave-name : Integer -> String)
(define (octave-name n)
  (cond [(= n -2)  "-2" ]
        [(= n -1)  "-1"]
        [(= n 0)   "+0"]
        [(= n 1)   "+1"]
        [(= n 2)   "+2"]
        [else (error "Unknown octave index: ~a" n)]))

(: NOTES : (Listof Nonnegative-Integer))
(define NOTES (list 0 1 2 3 4 5 6 7 8 9 10 11))

(: OCTAVES : (Listof Integer))
(define OCTAVES (list -2 -1 0 1 2))

(: note-to-frequency : Integer -> Nonnegative-Integer)
(define (note-to-frequency n)
  (exact-round (* 440 (expt 2 (/ (exact->inexact n) 12)))))

; TODO we'll probably want this to just be a list of referencable frequencies,
; rather than a list of rendered sounds, that get mapped by different synth
; sound options in some library to be able to have a sort of "tracker" in a cod
; file.
(match-define
  (list NOTE:C_:-2
        NOTE:C#:-2
        NOTE:D_:-2
        NOTE:D#:-2
        NOTE:E_:-2
        NOTE:F_:-2
        NOTE:F#:-2
        NOTE:G_:-2
        NOTE:G#:-2
        NOTE:A_:-2
        NOTE:A#:-2
        NOTE:B_:-2

        NOTE:C_:-1
        NOTE:C#:-1
        NOTE:D_:-1
        NOTE:D#:-1
        NOTE:E_:-1
        NOTE:F_:-1
        NOTE:F#:-1
        NOTE:G_:-1
        NOTE:G#:-1
        NOTE:A_:-1
        NOTE:A#:-1
        NOTE:B_:-1

        NOTE:C_:+0
        NOTE:C#:+0
        NOTE:D_:+0
        NOTE:D#:+0
        NOTE:E_:+0
        NOTE:F_:+0
        NOTE:F#:+0
        NOTE:G_:+0
        NOTE:G#:+0
        NOTE:A_:+0
        NOTE:A#:+0
        NOTE:B_:+0

        NOTE:C_:+1
        NOTE:C#:+1
        NOTE:D_:+1
        NOTE:D#:+1
        NOTE:E_:+1
        NOTE:F_:+1
        NOTE:F#:+1
        NOTE:G_:+1
        NOTE:G#:+1
        NOTE:A_:+1
        NOTE:A#:+1
        NOTE:B_:+1

        NOTE:C_:+2
        NOTE:C#:+2
        NOTE:D_:+2
        NOTE:D#:+2
        NOTE:E_:+2
        NOTE:F_:+2
        NOTE:F#:+2
        NOTE:G_:+2
        NOTE:G#:+2
        NOTE:A_:+2
        NOTE:A#:+2
        NOTE:B_:+2)
  (flatten (map (λ (octave)
                  (map (λ (note)
                         (define name (string->symbol (format "note:~a:~a" (note-name note) octave)))
                         (PongSound 'music
                                    name
                                    (rs:make-ding (note-to-frequency -1))))
                       NOTES))
                OCTAVES)))

(: SOUNDS-BALL-BOUNCE-OPPONENT PongSounds)
(define SOUNDS-BALL-BOUNCE-OPPONENT
  (for/list : PongSounds
    ([n (in-range -5 5)])
    (PongSound 'effect
               'ball-bounce-player
               (rs:make-ding (+ (note-to-frequency -1) (* n 1))))))

(: SOUNDS-BALL-BOUNCE-PLAYER PongSounds)
(define SOUNDS-BALL-BOUNCE-PLAYER
  (for/list : PongSounds
    ([n (in-range -5 5)])
    (PongSound 'effect
               'ball-bounce-player
               (rs:make-ding (+ (note-to-frequency 3) (* n 1))))))

(: SOUNDS-BALL-BOUNCE-WALL PongSounds)
(define SOUNDS-BALL-BOUNCE-WALL
(for/list : PongSounds
  ([n (in-range -5 5)])
  (PongSound 'effect
             'ball-bounce-wall
             (rs:make-ding (+ (note-to-frequency 5) (* n 1))))))

(: SOUND-ENDGAME PongSound)
(define SOUND-ENDGAME
  (PongSound 'music
             'endgame
             (rs:append*
              (for*/list : (Listof RSound)
                ([note-dur (in-list '((8 0.5) (7 0.5) (1 0.5) (3 1)))])
                (define note (first note-dur))
                (define dur (second note-dur))
                (rs:synth-note "vgame" 49 (+ 52 note)
                               (exact-round
                                (* (rs:default-sample-rate) dur)))))))

(: SOUND-MUSIC-BACKGROUND PongSound)
(define SOUND-MUSIC-BACKGROUND
  (PongSound 'music
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

(: SOUND-SCORE-ON-OPPONENT PongSound)
(define SOUND-SCORE-ON-OPPONENT
  (PongSound 'effect
             'score-on-opponent
             (rs:append*
              (for*/list : (Listof RSound)
                ([j (in-list '(2 3 7))])
                (rs:synth-note "vgame" 49 (+ 68 j)
                               (exact-round
                                (* (rs:default-sample-rate) (/ dur 4))))))))

(: SOUND-SCORE-ON-PLAYER PongSound)
(define SOUND-SCORE-ON-PLAYER
  (PongSound 'effect
             'score-on-player
             (rs:append*
              (for*/list : (Listof RSound)
                ([j (in-list '(7 3 2))])
                (rs:synth-note "vgame" 49 (+ 52 j)
                               (exact-round
                                (* (rs:default-sample-rate) (/ dur 4))))))))

(: SOUND-STARTUP PongSound)
(define SOUND-STARTUP
  (PongSound 'music
             'startup
             (rs:append*
              (for*/list : (Listof RSound)
                ([j (in-list '(2 3 7 6))])
                (rs:synth-note "vgame" 49 (+ 68 j)
                               (exact-round
                                (* (rs:default-sample-rate) (/ dur 2))))))))
