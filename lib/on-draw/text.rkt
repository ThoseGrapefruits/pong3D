#lang typed/racket

(require pict3d)

(require (prefix-in font: "font.rkt")
         (prefix-in font: "./font/statics/measurements.rkt"))

(provide text On-Char-Handler)

(: char-list : (Listof font:Char-3D))
(define char-list
  (list font:num:0
        font:num:1
        font:num:2
        font:num:3
        font:num:4
        font:num:5
        font:num:6
        font:num:7
        font:num:8
        font:num:9

        font:char:a
        font:char:b
        font:char:c
        font:char:d
        font:char:e
        font:char:f
        font:char:g
        font:char:h
        font:char:i
        font:char:j
        font:char:k
        font:char:l
        font:char:m
        font:char:n
        font:char:o
        font:char:p
        font:char:q
        font:char:r
        font:char:s
        font:char:t
        font:char:u
        font:char:v
        font:char:w
        font:char:x
        font:char:y
        font:char:z

        font:char:A
        font:char:B
        font:char:C
        font:char:D
        font:char:E
        font:char:F
        font:char:G
        font:char:H
        font:char:I
        font:char:J
        font:char:K
        font:char:L
        font:char:M
        font:char:N
        font:char:O
        font:char:P
        font:char:Q
        font:char:R
        font:char:S
        font:char:T
        font:char:U
        font:char:V
        font:char:W
        font:char:X
        font:char:Y
        font:char:Z

        font:ws:space

        font:symbol:?
        font:symbol:!
        font:symbol:+
        font:symbol:=
        font:symbol:@
        font:symbol:$
        font:symbol:#
        font:symbol:comma
        font:symbol:dot))

(: char-to-pair : font:Char-3D -> (Pairof Char font:Char-3D))
(define (char-to-pair char) (cons (font:Char-3D-char char) char))

; TODO it would be nice to use something like parser-tools/lex to 
; pull out multi-character things and allow for e.g. ligatures.
(: char-map : (Immutable-HashTable Char font:Char-3D))
(define char-map (make-immutable-hash (map char-to-pair char-list)))


; TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct Word-3D
  ([width : Flonum]
   [chars : (Listof font:Char-3D)]))

(: Word-3D-from-chars : (Listof font:Char-3D) -> Word-3D)
(define (Word-3D-from-chars chars)
 (define width : Flonum (foldl + 0.0 (map font:Char-3D-width chars)))
 (Word-3D width chars))

(: Word-3D-from-string : String -> Word-3D)
(define (Word-3D-from-string s)
  (define chars (string->list s))
  (: chars-mapped (Listof font:Char-3D))
  (define chars-mapped
    (map (λ (c) (hash-ref char-map c (λ () font:misc:unknown)))
         chars))
  (define width : Flonum (foldl + 0.0 (map font:Char-3D-width chars-mapped)))
  (Word-3D width chars-mapped))

(: WORD-SPACE Word-3D)
(define WORD-SPACE (Word-3D-from-chars (list font:ws:space)))

(: WORD-SPACE/WIDTH Flonum)
(define WORD-SPACE/WIDTH (Word-3D-width WORD-SPACE))

(define-type On-Char-Handler (-> Pict3D Char Integer Pict3D))

(: text : String
   [#:line-spacing Flonum]
   [#:onchar On-Char-Handler]
   [#:wrap Flonum]
   -> Pict3D)
(define (text s
              #:line-spacing [line-spacing 0.4]
              #:onchar [onchar (λ (pict c i) pict)]
              #:wrap [wrap 40.0])
  (define line-height (+ line-spacing font:HEIGHT-CAP))
  (define words (string-split s))
  (define words-rendered (map Word-3D-from-string words))
  (define lines (make-lines words-rendered #:wrap wrap))
  (combine
   (for/fold : (Listof Pict3D)
     ([lines empty-list/Pict3D])
     ([i (range 0 (length lines))]
      [line lines])
     (define line-y (* i line-height))
     (: line-rendered (Listof Pict3D))
     (define-values (_ line-rendered)
       (for/fold : (Values Flonum (Listof Pict3D))
         ([word-x : Flonum 0.0]
          [out empty-list/Pict3D])
         ([word line])
         (define-values (_ word-x-new chars-rendered)
           (for/fold : (Values Integer Flonum (Listof Pict3D))
             ([char-i : Integer 0]
              [char-x word-x]
              [out empty-list/Pict3D])
             ([char (Word-3D-chars word)])
             (define char-positioned
               (onchar (transform
                ((font:Char-3D-draw char))
                (affine-compose
                 (move-x char-x)
                 (move-y line-y))) (font:Char-3D-char char) char-i))
             (values (+ 1 char-i)
                     (+ char-x (font:Char-3D-width char))
                     (cons char-positioned out))))
         (values word-x-new
                 (append chars-rendered out))))
     (append line-rendered lines))))

(: make-lines : (Listof Word-3D) [#:wrap Flonum] -> (Listof (Listof Word-3D)))
(define (make-lines words #:wrap [wrap 40.0])
  (map reverse-line (reverse (make-lines-reversed words #:wrap wrap))))

(: make-lines-reversed : (Listof Word-3D) [#:wrap Flonum] -> (Listof (Listof Word-3D)))
(define (make-lines-reversed words #:wrap [wrap 40.0])
  (define-values (_ lines)
    (for/fold : (Values Flonum (Listof (Listof Word-3D))) ([line-length 0.0]
               [lines empty-list-list/Word-3D])
              ([word words])
      (define word-width (Word-3D-width word))
      (if (> (+ line-length word-width) wrap)
          (values word-width
                  (cons
                   (list word)
                   lines))
          (values (+ line-length word-width WORD-SPACE/WIDTH)
                  (cons
                   (cons word (cons WORD-SPACE (car lines)))
                   (cdr lines))))))
  lines)


; UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; typed racket chokes without this stuff and I couldn't figure out why

(: reverse-line : (Listof Word-3D) -> (Listof Word-3D))
(define (reverse-line l) (reverse l))

(: empty-list/Pict3D (Listof Pict3D))
(define empty-list/Pict3D empty)

(: empty-list-list/Word-3D (Listof (Listof Word-3D)))
(define empty-list-list/Word-3D (list empty))
