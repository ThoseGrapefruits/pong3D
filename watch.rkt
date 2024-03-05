#lang typed/racket

(require racket/system
         racket/file)

(module wrapper-file-watchers racket
  (require file-watchers)
  (provide apathetic-watch
           file-activity-channel
           file-watcher-channel-get
           file-watcher-status-channel
           path-on-disk?
           suggest-approach
           watch))

(require/typed 'wrapper-file-watchers
  [#:opaque Path-On-Disk path-on-disk?]
  [apathetic-watch (-> Path Thread)]
  [file-activity-channel (Parameter (Async-Channelof Any))]
  [file-watcher-channel-get (-> (U Boolean (Listof Any)))]
  [file-watcher-status-channel (Parameter (Async-Channelof Any))]
  [watch (->* ()
              ((Listof Path) ; paths
               (-> (Listof Any) Any) ; on-activity
               (-> (Listof Any) Any) ; on-status
               (-> Path Thread))
              Thread)])


(define paths (list (string->path "pong.rkt") (string->path "lib/")))

(: thread-box (Boxof (U Thread #f)))
(define thread-box (box #f))

(let loop ()
  (define unboxed-thread (unbox thread-box))
  (if unboxed-thread (kill-thread unboxed-thread) '())
  (define new-thread (watch paths
                            (lambda (lst) (system "raco make pong.rkt && racket pong.rkt"))
                            (lambda (lst) '())))
  (box-cas! thread-box unboxed-thread new-thread)
  (thread-wait new-thread)
  (loop))