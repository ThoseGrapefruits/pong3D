#lang typed/racket/base

(require
  "./notes.rkt"
  "./sound.rkt"
  racket/list)

(provide SONG-BACKGROUND)

(define-type Song (Listof (Listof Symbol)));

(: SONG-BACKGROUND : Song)
(define SONG-BACKGROUND
  (list (list 'test)))
