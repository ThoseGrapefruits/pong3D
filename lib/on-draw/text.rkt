#lang typed/racket

(require parser-tools/lex
         pict3d)

(require (prefix-in font: "font.rkt"))

(provide text)

(: text-map : (Immutable-HashTable Char font:Char-3D))
(define text-map
  (make-immutable-hash
   (list
    (cons #\a font:a)
    (cons #\space font:ws-space))))

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

(: text : String [#:wrap Flonum] -> Pict3D)
(define (text s #:wrap [wrap 40.0])
  (define words (string-split s))
  (define words-rendered (map Word-3D-from-string words))
  (define lines-reversed (foldl (line-maker wrap) '() words-rendered))
  (define lines (map reverse-line (reverse lines-reversed)))
  ; TODO render things in correct positions
  (combine
   (for*/list : (Listof Pict3D) ([line lines])
     (combine
      (for*/list : (Listof Pict3D) ([ word line ])
      (combine
       (for*/list : (Listof Pict3D) ([ char (Word-3D-chars word) ])
       (font:Char-3D-drawn char))))))))

(: line-maker : Flonum -> (-> Word-3D (Listof (Listof Word-3D)) (Listof (Listof Word-3D)))) 
(define (line-maker [wrap 40.0])
  (lambda (word lines)
    (define word-width (Word-3D-width word))
    (define line-length (foldl + word-width (map Word-3D-width (first lines))))
    (if (> line-length wrap)
        (cons
          (list word)
          lines)
        (cons
          (cons word (first lines))
          (rest lines)))))

; typed racket chokes without this and I couldn't figure out why
(: reverse-line : (Listof Word-3D) -> (Listof Word-3D))
(define (reverse-line l) (reverse l))