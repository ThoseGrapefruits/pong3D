#lang typed/racket

(require pict3d)

(require (prefix-in font: "font.rkt"))

(provide text)

; TODO it would be nice to use something like parser-tools/lex to 
; pull out multi-character things and allow for e.g. ligatures.
(: text-map : (Immutable-HashTable Char font:Char-3D))
(define text-map
  (make-immutable-hash
   (list
    (cons #\0 font:num:0)
    (cons #\1 font:num:1)
    (cons #\2 font:num:2)
    (cons #\3 font:num:3)
    (cons #\4 font:num:4)
    (cons #\5 font:num:5)
    (cons #\6 font:num:6)
    (cons #\7 font:num:7)
    (cons #\8 font:num:8)
    (cons #\9 font:num:9)

    (cons #\a font:char:a)
    (cons #\b font:char:b)
    (cons #\c font:char:c)
    (cons #\d font:char:d)
    (cons #\e font:char:e)
    (cons #\f font:char:f)
    (cons #\g font:char:g)
    (cons #\h font:char:h)
    (cons #\i font:char:i)
    (cons #\j font:char:j)
    (cons #\k font:char:k)
    (cons #\l font:char:l)
    (cons #\m font:char:m)
    (cons #\n font:char:n)
    (cons #\o font:char:o)
    (cons #\p font:char:p)
    (cons #\q font:char:q)
    (cons #\r font:char:r)
    (cons #\s font:char:s)
    (cons #\t font:char:t)
    (cons #\u font:char:u)
    (cons #\v font:char:v)
    (cons #\w font:char:w)
    (cons #\x font:char:x)
    (cons #\y font:char:y)
    (cons #\z font:char:z)

    (cons #\A font:char:A)
    (cons #\B font:char:B)
    (cons #\C font:char:C)
    (cons #\D font:char:D)
    (cons #\E font:char:E)
    (cons #\F font:char:F)
    (cons #\G font:char:G)
    (cons #\H font:char:H)
    (cons #\I font:char:I)
    (cons #\J font:char:J)
    (cons #\K font:char:K)
    (cons #\L font:char:L)
    (cons #\M font:char:M)
    (cons #\N font:char:N)
    (cons #\O font:char:O)
    (cons #\P font:char:P)
    (cons #\Q font:char:Q)
    (cons #\R font:char:R)
    (cons #\S font:char:S)
    (cons #\T font:char:T)
    (cons #\U font:char:U)
    (cons #\V font:char:V)
    (cons #\W font:char:W)
    (cons #\X font:char:X)
    (cons #\Y font:char:Y)
    (cons #\Z font:char:Z)

    (cons #\space font:ws:space)

    (cons #\? font:symbol:?)
    (cons #\. font:symbol:dot)
    )))


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
    (map (lambda (c) (hash-ref text-map c (lambda () font:misc:unknown)))
         chars))
  (define width : Flonum (foldl + 0.0 (map font:Char-3D-width chars-mapped)))
  (Word-3D width chars-mapped))

(: WORD-SPACE Word-3D)
(define WORD-SPACE (Word-3D-from-chars (list font:ws:space)))

(: WORD-SPACE/WIDTH Flonum)
(define WORD-SPACE/WIDTH (Word-3D-width WORD-SPACE))

(: text : String [#:line-spacing Flonum] [#:wrap Flonum] -> Pict3D)
(define (text s
              #:line-spacing [line-spacing 0.4]
              #:wrap [wrap 40.0])
  (define line-height (+ line-spacing font:EM-HEIGHT))
  (define words (string-split s))
  (define words-rendered (map Word-3D-from-string words))
  (define lines (make-lines words-rendered #:wrap wrap))
  (combine
   (for/fold : (Listof Pict3D)
     ([lines empty])
     ([i (range 0 (length lines))]
      [line lines])
     (define line-y (* i line-height))
     (: line-rendered (Listof Pict3D))
     (define-values (_ line-rendered)
       (for/fold : (Values Flonum (Listof Pict3D))
         ([word-x : Flonum 0.0]
          [out empty-list/Pict3D])
         ([word line])
         (define-values (word-x-new chars-rendered)
           (for/fold : (Values Flonum (Listof Pict3D))
             ([char-x word-x]
              [out empty-list/Pict3D])
             ([char (Word-3D-chars word)])
             (define char-positioned
               (transform
                ((font:Char-3D-draw char))
                (affine-compose
                 (move-x char-x)
                 (move-y line-y))))
             (values (+ char-x (font:Char-3D-width char))
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
