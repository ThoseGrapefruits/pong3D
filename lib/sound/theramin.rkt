#lang typed/racket/base
;; Build: raco make lib/sound/theramin.rkt
;; Play:  racket lib/sound/theramin-play.rkt

(provide song)

(require
  (only-in racket/base build-list)
  (only-in racket/list empty? empty first rest)
  (only-in racket/local local)
  (only-in racket/math exact-round nonnegative-integer?)
  "./rsound.rkt"
  "./sound.rkt")

;; pick-trans : pick one of the transitions. ASSUMES THAT THE 
;; PROBABILITIES OF THE TRANSES ADD UP TO ONE
(: pick-trans (-> Real (Listof Transition) Nonnegative-Integer))
(define (pick-trans rand transes)
  (cond [(empty? transes) (error "ran out of transes!\n")]
        [else (local [(define first-prob
                        (Transition-prob (first transes)))]
                (cond [(< rand first-prob) 
                       (Transition-idx (first transes))]
                      [else 
                       (pick-trans (- rand first-prob)
                                   (rest transes))]))]))

;; generate a table of 4 random states
(define num-states 4)

(: random-states (-> Void (Listof State)))
(define (random-states _)
  (build-list 4 random-state))

;; how many random transitions should come from each note?
(define NUM-TRANSES 4)

(: random-state (-> Index State))
(define (random-state _)
  (make-State (make-PD (list-ref tones-list 
                                 (random (length tones-list)))
                       (list-ref durs-list
                                 (random (length durs-list))))
              (build-list NUM-TRANSES random-trans)))

;; make a random transition
(define (random-trans _)
  (make-Transition (/ 1 NUM-TRANSES) (random num-states)))

;; the list of notes chosen from
(define tones-list (list 60 62 64 65 67 69 71 72))

;; the list of durations chosen from
(: durs-list (Listof Positive-Exact-Rational))
(define durs-list (list 2 1 1 1 1/2 1/2 1/4))

;; a pd is [make-pd midi-note-num beats]
(define-struct PD
  ([pitch    : Positive-Integer]
   [duration : Positive-Exact-Rational]))

;; a Transition is [make-Transition number number]
;; representing a transition to state 'idx' with probability 'prob'
(define-struct Transition
  ([prob : Positive-Real]
   [idx  : Nonnegative-Integer]))

;; a State is [make-state PD transes]
(define-struct State
  ([pd      : PD]
   [transes : (Listof Transition)]))

(: iter-state (-> Real Nonnegative-Integer (Listof State) (Listof PD)))
(define (iter-state dur cur-idx model)
  (cond [(<= dur 0) empty]
        [else 
         (local
           ((define cur-state (list-ref model cur-idx)))
           (cons (State-pd cur-state)
                 (iter-state (- dur (PD-duration
                                     (State-pd cur-state)))
                             (pick-trans (random) (State-transes cur-state))
                             model)))]))


;;; EXAMPLES:

(define sample-states
  (list (make-State (make-PD 60 1) (list (make-Transition 1 1)))
        (make-State (make-PD 62 1) (list (make-Transition 1 2)))
        (make-State (make-PD 64 1) (list (make-Transition 1 3)))
        (make-State (make-PD 65 1) (list (make-Transition 1 0)))))

(define awesome-states
  (list (make-State (make-PD 60 1) (list (make-Transition 1 1)))
        (make-State (make-PD 62 1) (list (make-Transition 1 2)))
        (make-State (make-PD 64 1) (list (make-Transition 1 3)))
        (make-State (make-PD 65 1) (list (make-Transition 1/2 0)
                                         (make-Transition 1/4 1)
                                         (make-Transition 1/4 2)))))

#;(random-seed 282887)
(define rand-states
  (random-states (void)))

(define bass-states
  (list (make-State (make-PD 48 2) (list (make-Transition 1 1)))
        (make-State (make-PD 55 2) (list (make-Transition 1 2)))
        (make-State (make-PD 53 2) (list (make-Transition 1 3)))
        (make-State (make-PD 55 2) (list (make-Transition 1 0)))))

(define BPM 120)
(: beat-secs Positive-Real)
(define beat-secs (/ 60 BPM))
(: eighth-secs Nonnegative-Real)
(define eighth-secs (* beat-secs 1/2))
(: sixteenth-secs Nonnegative-Real)
(define sixteenth-secs (* beat-secs 1/4))
(: beat-frames Nonnegative-Integer)
(define beat-frames (exact-round (* 44100 beat-secs)))
(: quarter-frames Nonnegative-Integer)
(define quarter-frames beat-frames)

(: make-note (-> PD RSound))
(define (make-note pd)
  (cond [(number? (PD-pitch pd))
         (rs:overlay
           (rs:synth-note "main" 43
                          (PD-pitch pd)
                          (exact-round (* quarter-frames (PD-duration pd))))
           (rs:synth-note "main" 43
                          (exact-round (* 1.00025 (PD-pitch pd)))
                          (exact-round (* quarter-frames (PD-duration pd)))))]
        [else
         (rs:silence (exact-round
                       (* quarter-frames
                          (PD-duration pd))))]))

(define song
  (rs:overlay
   (rs:scale #i0.4
             (rs:append*
   (map make-note (iter-state 16 0 awesome-states))))
   (rs:scale #i0.4 
             (rs:append*
   (map make-note (iter-state 16 0 bass-states))))))
