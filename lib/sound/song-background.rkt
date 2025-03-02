#lang typed/racket/base

(require
  racket/list
  "./song.rkt")

(provide SONG-MENU)

(: SONG-MENU : Song)
(define SONG-MENU
  (Song
   'loop
   10
   '((C_:-1 A_:+2)
     (B_:-1 B_:+2)
     (A_:-1 A_:+1))))
