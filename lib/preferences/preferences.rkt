#lang typed/racket/base

(require
  (only-in racket/file
           get-preference
           put-preferences))

(provide get-pref-boolean
         get-pref-flonum
         get-pref-real
         get-pref-string
         put-pref
         Pref-Key)

(define path-base  (build-path (find-system-path 'pref-dir) (string->path "pong3d")))
(define path-prefs (build-path path-base (string->path "pong3d.ss")))

; Main list of all preference keys
(define-type Pref-Key
  (U 'gameplay-guides
     'volume-effects
     'volume-main
     'volume-music))

(: get-preference-string : Pref-Key -> (U String #f))
(define (get-preference-string key)
  (define result (get-preference key (λ () #f) #f path-prefs))
  (cond [(not result) #f]
        [(string? result) result]
        [else
         (eprintf "~s: non-string result for ~s: ~s~n" 'get-preference-string key result)
         #f]))

(: get-pref-boolean : Pref-Key (-> Boolean) -> Boolean)
(define (get-pref-boolean key failure)
  (define result-string (get-preference-string key))
  (define result-boolean (and result-string
                              (cond [(eq? result-string "#t")    #t]
                                    [(eq? result-string "true")  #t]
                                    [(eq? result-string "#f")    #f]
                                    [(eq? result-string "false") #f]
                                    [else                        null])))
  (cond [(boolean? result-boolean) result-boolean]
        [else (failure)]))

(: get-pref-string : (case-> [Pref-Key             -> (U String #f)]
                             [Pref-Key (-> String) ->    String    ]))
(define get-pref-string
  (case-lambda
     [([key : Pref-Key])
      (get-preference-string key)]
     [([key : Pref-Key]
       [failure : (-> String)])
      (or (get-preference-string key) (failure))]))

(: get-pref-real : Pref-Key (-> Real) -> Real)
(define (get-pref-real key failure)
  (define result-string (get-preference-string key))
  (define result-number (and result-string
                             (string->number result-string)))
  (cond [(real? result-number) result-number]
        [else (failure)]))

(: get-pref-flonum : Pref-Key (-> Flonum) -> Flonum)
(define (get-pref-flonum key failure)
  (define result-string (get-preference-string key))
  (define result-number (and result-string
                             (string->number result-string)))
  (cond [(flonum? result-number) result-number]
        [else (failure)]))

(: put-pref : Pref-Key Any -> Void)
(define (put-pref key value)
  (printf "preference saved: ~v = ~v~n" key value)
  (put-preferences `(,key)
                   `(,value)
                   (λ (path) (error 'put-pref "lock is held on prefs file: ~v" path))
                   path-prefs))
