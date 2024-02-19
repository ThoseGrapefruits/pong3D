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
    (cons #\a font:a)
    (cons #\space font:ws-space))))

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
    (map (lambda (c) (hash-ref text-map c (lambda () font:unknown)))
         chars))
  (define width : Flonum (foldl + 0.0 (map font:Char-3D-width chars-mapped)))
  (Word-3D width chars-mapped))

(: WORD-SPACE Word-3D)
(define WORD-SPACE (Word-3D-from-chars (list font:ws-space)))

(: WORD-SPACE/WIDTH Flonum)
(define WORD-SPACE/WIDTH (Word-3D-width WORD-SPACE))

(: text : String [#:line-height Flonum] [#:wrap Flonum] -> Pict3D)
(define (text s
              #:line-height [line-height 1.2]
              #:wrap [wrap 40.0])
  (define words (string-split s))
  (define words-rendered (map Word-3D-from-string words))
  (define lines (make-lines words-rendered #:wrap wrap))
  (combine
   (for/list : (Listof Pict3D)
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
                (font:Char-3D-drawn char)
                (affine-compose
                 (move-x char-x)
                 (move-y line-y))))
             (values (+ char-x (font:Char-3D-width char))
                     (cons char-positioned out))))
         (values word-x-new
                 (cons (combine chars-rendered) out))))
     (combine line-rendered))))

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
          (values 0.0
                  (cons
                   (list word)
                   lines))
          (values (+ line-length word-width WORD-SPACE/WIDTH)
                  (cons
                   (cons word (cons WORD-SPACE (car lines)))
                   (cdr lines))))))
  lines)

; UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; typed racket chokes without this and I couldn't figure out why
(: reverse-line : (Listof Word-3D) -> (Listof Word-3D))
(define (reverse-line l) (reverse l))

(: empty-list/Pict3D (Listof Pict3D))
(define empty-list/Pict3D empty)

(: empty-list-list/Word-3D (Listof (Listof Word-3D)))
(define empty-list-list/Word-3D (list empty))
