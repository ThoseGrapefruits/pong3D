#lang typed/racket/base

(require
  (only-in racket/match match-define)
  "../state/state.rkt"
  "./text.rkt"
  "./types.rkt"
  pict3d)

(provide
  (struct-out Score-Section)
  get-on-draw-game-score
  SCORE-SECTIONS)

(: get-on-draw-game-score : State-Game-Over -> font:On-Draw-Handler)
(define (get-on-draw-game-score s)
  (define len (length SCORE-SECTIONS))
  (λ (draw c i)
    (define score-section (list-ref SCORE-SECTIONS (- len i 1)))
    (match-define (Score-Section color-emitted _ _ _) score-section)
    (parameterize ([current-emitted color-emitted]) (draw))))