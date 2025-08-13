#lang racket

(require ffi/unsafe)

;; Load the PortAudio library
(define portaudio (ffi-lib "libportaudio"
                           (list "2" "2.0.0" #f)
                           #:fail (lambda (msg)
                                    (error 'portaudio-test msg)
                                    (void))))

;; Define the necessary PortAudio functions
(define pa_initialize (get-ffi-obj "Pa_Initialize" portaudio (_fun -> _int)))
(define pa_terminate (get-ffi-obj "Pa_Terminate" portaudio (_fun -> _int)))
(define pa_get_version (get-ffi-obj "Pa_GetVersion" portaudio (_fun -> _int)))

;; Example function to initialize PortAudio
(define (initialize-portaudio)
  (if (= (pa_initialize) 0)
      (begin
        (displayln "PortAudio initialized successfully.")
        (displayln (format "PortAudio version: ~a" (pa_get_version)))
        #t)
      (begin
        (displayln "Failed to initialize PortAudio.")
        #f)))

;; Example function to terminate PortAudio
(define (terminate-portaudio)
  (pa_terminate)
  (displayln "PortAudio terminated."))

(initialize-portaudio)

(terminate-portaudio)
