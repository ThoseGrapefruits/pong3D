#lang typed/racket/base

(require "./sound.rkt"
         (only-in racket/match match-define)
         racket/math)

(provide get-note-frequency
         get-note-frequency-value)

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

(struct Pong-Frequency
  ([category  : Sound-Category]
   [name      : Symbol]
   [frequency : Nonnegative-Integer])
  #:transparent)

(: NOTES : (Listof Nonnegative-Integer))
(define NOTES (list 0 1 2 3 4 5 6 7 8 9 10 11))

(: OCTAVES : (Listof Integer))
(define OCTAVES (list -2 -1 0 1 2))

(: note-to-pong-frequency : Integer Nonnegative-Integer -> Pong-Frequency)
(define (note-to-pong-frequency octave note)
  (define name (string->symbol
                (format "~a:~a" (note-name note) (octave-name octave))))
  (Pong-Frequency 'music
                  name
                  (note-to-frequency (+ note (* 8 octave)))))

(: NOTE-FREQUENCIES : (Listof Pong-Frequency))
(define NOTE-FREQUENCIES
  (for*/list : (Listof Pong-Frequency)
    ([octave OCTAVES]
     [note NOTES])
     (note-to-pong-frequency octave (assert note nonnegative-integer?))))

(: NOTE-FREQUENCIES-MAP : (HashTable Symbol Pong-Frequency))
(define NOTE-FREQUENCIES-MAP
  (make-immutable-hash
   (for/list : (Listof (Pairof Symbol Pong-Frequency))
     ([pf NOTE-FREQUENCIES])
     (cons (Pong-Frequency-name pf) pf))))

(: get-note-frequency : Symbol -> (U Pong-Frequency #f))
(define (get-note-frequency note)
  (and (not (eq? note '__:__))
       (hash-ref NOTE-FREQUENCIES-MAP note)))

(: get-note-frequency-value : Symbol -> (U Nonnegative-Integer #f))
(define (get-note-frequency-value note)
  (and (not (eq? note '__:__))
       (Pong-Frequency-frequency (hash-ref NOTE-FREQUENCIES-MAP note))))

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
  NOTE-FREQUENCIES)
