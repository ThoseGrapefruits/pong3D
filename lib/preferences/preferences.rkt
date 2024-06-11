#lang typed/racket/base

(require racket/file)

(provide get-preference-pong3d
         get-preference-pong3d-flonum
         get-preference-pong3d-real)

(define path-base  (build-path (find-system-path 'pref-dir) (string->path "pong3d")))
(define path-prefs (build-path path-base (string->path "pong3d.ss")))

(: get-preference-string : Symbol -> (U String #f))
(define (get-preference-string symbol)
  (define result (get-preference symbol (λ () #f) #f path-prefs))
  (cond [(not result) #f]
        [(string? result) result]
        [else (error 'get-preference-pong3d "non-string result: ~s" result)]))

(: get-preference-pong3d : (case-> [Symbol             -> (U String #f)]
                                   [Symbol (-> String) ->    String    ]))
(define get-preference-pong3d
  (case-lambda
     [([symbol : Symbol])
      (get-preference-string symbol)]
     [([symbol : Symbol]
       [failure : (-> String)])
      (or (get-preference-string symbol) (failure))]))

(: get-preference-pong3d-real : Symbol (-> Real) -> Real)
(define (get-preference-pong3d-real symbol failure)
  (define result-string (get-preference-string symbol))
  (define result-number (and result-string
                             (string->number result-string)))
  (cond [(real? result-number) result-number]
        [else (failure)]))

(: get-preference-pong3d-flonum : Symbol (-> Flonum) -> Flonum)
(define (get-preference-pong3d-flonum symbol failure)
  (define result-string (get-preference-string symbol))
  (define result-number (and result-string
                             (string->number result-string)))
  (cond [(flonum? result-number) result-number]
        [else (failure)]))
