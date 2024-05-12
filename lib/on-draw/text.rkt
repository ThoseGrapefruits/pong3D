#lang typed/racket/base

(require pict3d
         racket/list
         racket/string)

(require (prefix-in font: "font.rkt")
         (prefix-in font: "./font/statics/measurements.rkt"))

(provide text
         font:On-Char-Handler
         font:On-Draw-Handler)

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
        font:symbol:%
        font:symbol:^
        font:symbol:&
        font:symbol:*
        font:symbol:paren-left
        font:symbol:paren-right
        font:symbol:bracket-square-left
        font:symbol:bracket-square-right
        font:symbol:bracket-curly-left
        font:symbol:bracket-curly-right
        font:symbol:-
        font:symbol:—
        font:symbol:_
        font:symbol:/
        font:symbol:\
        font:symbol:vertical-line
        font:symbol:quote-single
        font:symbol:quote-double
        font:symbol::
        font:symbol:semicolon
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

(: text (->* (String)
             (#:onchar font:On-Char-Handler
              #:ondraw font:On-Draw-Handler
              #:spacing-line Flonum
              #:spacing-paragraph Flonum
              #:wrap Flonum)
             Pict3D))
(define (text s
              #:onchar [onchar : font:On-Char-Handler (λ (pict c i) pict)]
              #:ondraw [ondraw : font:On-Draw-Handler (λ (draw c i) (draw))]
              #:spacing-line [spacing-line 0.4]
              #:spacing-paragraph [spacing-paragraph 0.2]
              #:wrap [wrap 40.0])
  (define paragraphs (map string-trim (string-split s #px"(?:\r?\n)+")))
  (define-values (_ paragraphs-rendered)
    (for/fold : (Values Flonum (Listof Pict3D))
      ([paragraph-y : Flonum          0.0]
       [out         : (Listof Pict3D) '()])
      ([i           : Integer         (range 0 (length paragraphs))]
       [paragraph   : String          paragraphs])
      (define paragraph-y-now (+ paragraph-y spacing-paragraph))
      (define-values (paragraph-y-new paragraph-rendered)
        (text-paragraph paragraph paragraph-y-now
                        #:onchar onchar
                        #:ondraw ondraw
                        #:spacing-line spacing-line
                        #:wrap wrap))
      (values paragraph-y-new (append paragraph-rendered out))))
  (combine paragraphs-rendered))

(: text-paragraph (->* (String
                        Flonum
                        #:spacing-line Flonum
                        #:onchar font:On-Char-Handler
                        #:ondraw font:On-Draw-Handler
                        #:wrap Flonum)
                       ()
                       (Values Flonum (Listof Pict3D))))
(define (text-paragraph paragraph
                        y-start
                        #:onchar onchar
                        #:ondraw ondraw
                        #:spacing-line spacing-line
                        #:wrap wrap)
  (define line-height (+ spacing-line font:HEIGHT-CAP))
  (define words (string-split paragraph))
  (define words-rendered (map Word-3D-from-string words))
  (define lines (make-lines words-rendered #:wrap wrap))
  (for/fold : (Values Flonum (Listof Pict3D))
    ([line-y : Flonum y-start]
     [out    : (Listof Pict3D) '()])
    ([line lines])
    (define line-y-now (+ line-y line-height))
    (define-values (_ line-rendered)
      (text-line line line-y-now #:onchar onchar #:ondraw ondraw))
    (values line-y-now (append line-rendered out))))

(: text-line (->* ((Listof Word-3D)
                   Flonum
                   #:onchar font:On-Char-Handler
                   #:ondraw font:On-Draw-Handler)
                  ()
                  (Values Flonum (Listof Pict3D))))
(define (text-line line line-y #:onchar onchar #:ondraw ondraw)
  (for/fold : (Values Flonum (Listof Pict3D))
    ([word-x : Flonum          0.0]
     [out    : (Listof Pict3D) '()])
    ([word   : Word-3D         line])
    (define-values (word-x-new chars-rendered)
      (text-word word line-y word-x #:onchar onchar #:ondraw ondraw))
    (values word-x-new (append chars-rendered out))))

(: text-word (->* (Word-3D
                   Flonum
                   Flonum
                   #:onchar font:On-Char-Handler
                   #:ondraw font:On-Draw-Handler)
                  ()
                  (Values Flonum (Listof Pict3D))))
(define (text-word word
                   line-y
                   word-x
                   #:onchar onchar
                   #:ondraw ondraw)
  (define chars (Word-3D-chars word))
  (cond [(empty? chars) (values word-x '())]
        [else (define-values (_ word-x-new chars-rendered)
                (if (empty? chars)
                    (values word-x '())
                    (for/fold : (Values Integer Flonum (Listof Pict3D))
                      ([char-i : Integer         0]
                       [char-x : Flonum          word-x]
                       [out    : (Listof Pict3D) '()])
                      ([char   : font:Char-3D    chars])
                      (define char-rendered ((font:Char-3D-draw char) ondraw
                                                                      (font:Char-3D-char char)
                                                                      char-i))
                      (define char-positioned
                        (onchar (transform char-rendered
                                           (affine-compose (move-x char-x)
                                                           (move-y line-y)))
                                (font:Char-3D-char char)
                                char-i))
                      (values (+ 1 char-i)
                              (+ char-x (font:Char-3D-width char))
                              (cons char-positioned out)))))
              (values word-x-new chars-rendered) ]))

(: make-lines (->* ((Listof Word-3D) #:wrap Flonum) () (Listof (Listof Word-3D))))
(define (make-lines words #:wrap wrap)
  (: lines-reversed : (Listof (Listof Word-3D)))
  (define lines-reversed (reverse (make-lines-reversed words #:wrap wrap)))
  ; Typed Racket doesn't infer this correctly. Might be the nested lists?
  (: reverse-line : (Listof Word-3D) -> (Listof Word-3D))
  (define (reverse-line l) (reverse l))

  (map reverse-line lines-reversed))

(: make-lines-reversed (->* ((Listof Word-3D) #:wrap Flonum) () (Listof (Listof Word-3D))))
(define (make-lines-reversed words #:wrap wrap)
  (define-values (_ lines)
    (for/fold : (Values Flonum (Listof (Listof Word-3D))) ([line-length 0.0]
               [lines : (Listof (Listof Word-3D)) '(())])
              ([word  : Word-3D words])
      (define word-width (Word-3D-width word))
      (if (> (+ line-length word-width) wrap)
          (values word-width
                  (cons
                   (list word)
                   lines))
          (values (+ line-length word-width WORD-SPACE/WIDTH)
                  (cons
                   ; Space goes before the word because these lines will be reversed.
                   (cons WORD-SPACE (cons word (car lines)))
                   (cdr lines))))))
  lines)
