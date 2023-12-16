#lang typed/racket

(require typed/racket/gui/base)

(module wrapper racket
  (require (prefix-in rs: rsound))
  (require (prefix-in rs: rsound))
  (provide
   rs:default-sample-rate
   rs:ding
   rs:make-ding
   rs:play
   rs:rs-overlay
   rs:rs-append*
   rs:rs-write
   rs:rsound
   rs:rsound?
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
;  [rs:rsound any/c]
 [rs:stop (-> Void)]
 [rs:synth-note (-> String Number Natural Natural RSound)])

(provide
 rs-play
 rs-play-random
 rs-stop

 SOUND-DING
 SOUND-ENDGAME
 SOUND-MUSIC
 SOUNDS-BALL-BOUNCE-BUMPER
 SOUNDS-BALL-BOUNCE-WALL)

;; PLAY/STOP

(: path-cache (Mutable-HashTable RSound Path))
(define path-cache (make-hash))

(: rs-play (->* (RSound) (Boolean) Thread))
(define (rs-play sound [loop #f])
  ; TODO would be good to pull this over but it's an internal function in rsound
  ; (rs:check-below-threshold sound 2.0)
  (define cached-path (hash-ref path-cache sound #f))
  (define path (if cached-path cached-path (make-temporary-file "pong3d-sound-~a.wav")))
  (hash-set! path-cache sound path)
  (rs:rs-write sound path)
  (: play (-> Boolean))
  (define play (if loop
                   (lambda () (play-sound path #f) (play))
                   (lambda () (play-sound path #f))))
  (thread play))

(: rs-play-random (->* ((Listof RSound)) (Boolean) Thread))
(define (rs-play-random sounds [loop #f])
  (define sound (list-ref sounds (random 0 (length sounds))))
  (rs-play sound loop))

(: rs-stop (-> Void))
(define rs-stop rs:stop)

;; SOUND CONSTANTS

(: bpm Positive-Real)
(define bpm 150)

(: dur Positive-Real)
(define dur (max 0.01 (/ 60 bpm)))

(: SOUND-DING RSound)
(define SOUND-DING rs:ding)

(: SOUNDS-BALL-BOUNCE-BUMPER (Listof RSound))
(define SOUNDS-BALL-BOUNCE-BUMPER (for/list : (Listof RSound) ([n (in-range -5 5)])
                                    (rs:make-ding (+ 523 (* n 1)))))

(: SOUNDS-BALL-BOUNCE-WALL (Listof RSound))
(define SOUNDS-BALL-BOUNCE-WALL (for/list ([n (in-range -5 5)])
                                  (rs:make-ding (+ 587 (* n 1)))))

(: SOUND-ENDGAME RSound)
(define SOUND-ENDGAME
  (rs:rs-append*
   (for*/list : (Listof RSound) ([note-dur (in-list '((8 0.5) (7 0.5) (1 0.5) (3 1)))])
    (define note (first note-dur))
    (define dur (second note-dur))
     (rs:synth-note "vgame" 49 (+ 52 note)
                    (exact-round
                     (* (rs:default-sample-rate) dur))))))

(rs-play SOUND-ENDGAME)

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
