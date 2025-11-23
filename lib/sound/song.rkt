#lang typed/racket/base

(require
  (only-in racket/match match-define)
  "./notes.rkt"
  "./rsound.rkt"
  "./sound.rkt")

(provide
 rs-play-song
 (struct-out Song))

(define-struct Song
  ([loop? : (Union 'loop 'one-shot)]
   [tempo  : Positive-Integer]
   [track-count : Positive-Integer]
   [tracks : (Listof Symbol)]))

(define-struct StreamedTrack
  ([stream : PStream]
   [track : (Listof Symbol)]))

(define-type StreamedSong (Listof StreamedTrack))

(: get-next-time : PStream Nonnegative-Integer -> Nonnegative-Integer)
(define get-next-time (λ (ps frames-per-beat) (+ (ps:current-frame ps) frames-per-beat)))

(: rs-play-song : Song -> StreamedSong)
(define (rs-play-song song)
  (define track-count (Song-track-count song))
  (: tracks : (Listof (Listof Symbol)))
  (define tracks
    (vector->list
     (for/foldr : (Mutable-Vectorof (Listof Symbol))
       ([acc : (Mutable-Vectorof (Listof Symbol)) (make-vector track-count (ann '() (Listof Symbol)))])
       ([note : Symbol (Song-tracks song)]
        [i : Integer (in-range track-count)])
       (define track-index (modulo i track-count))
       (vector-set! acc track-index (cons note (vector-ref acc track-index)))
       acc)))

  (define streamed-tracks
    (map (λ ([track : (Listof Symbol)])
           (define ps (ps:make-pstream))
           (ps:set-volume! ps (volume-for 'music))
           (StreamedTrack ps track))
         tracks))
  (define frames-per-beat (inexact->exact (round (/ (* 60 44100) (Song-tempo song)))))
  (define track-length (length (StreamedTrack-track (car streamed-tracks))))
  (when (for/or : Boolean ([track streamed-tracks])
          (not (= (length (StreamedTrack-track track)) track-length)))
    (error 'rs-play-song "track length mismatch"))

  (: beat-box : (Boxof Nonnegative-Integer))
  (define beat-box (box 0))

  ; (: volume-last : (Boxof Flonum))
  ; (define volume-last (box (volume-for 'music)))

  (: load-next : (-> Void))
  (define load-next
    (λ ()
      (define beat (unbox beat-box))
      (for ([streamed-track streamed-tracks])
        (: ps : PStream)
        (: track : (Listof Symbol))
        (match-define (StreamedTrack ps track) streamed-track)
        (ps:set-volume! ps (volume-for 'music))
        (when (< beat (length track))
          (define note-symbol (list-ref track beat))
          (define freq (get-note-frequency-value note-symbol))
          (define tone (rs:make-tone (or freq 0)
                                     (if freq volume-global 0.0)
                                     frames-per-beat))
          (define duration (* beat frames-per-beat))
          (when freq (ps:queue ps tone duration))))

      (box-cas! beat-box beat (add1 beat))

      ;; Check if we need to loop
      (when (and (eq? (Song-loop? song) 'loop)
                 (>= beat track-length))
        ;; Reset all track positions to start over
        (set-box! beat-box 0))

      ;; Schedule next callback if there are still notes to play
      (when (or (eq? (Song-loop? song) 'loop)
                (< beat track-length))
        (define first-pstream (StreamedTrack-stream (car streamed-tracks)))
        (ps:queue-callback first-pstream
                           load-next
                           (get-next-time first-pstream frames-per-beat)))
      (void)))

  ;; Start the song playback
  (ps:queue-callback (StreamedTrack-stream (car streamed-tracks)) load-next 0)
  streamed-tracks)

(: rs-stop-song : StreamedSong -> Void)
(define (rs-stop-song streamed-tracks)
  (for ([streamed-track streamed-tracks])
    (ps:clear! (StreamedTrack-stream streamed-track))))
