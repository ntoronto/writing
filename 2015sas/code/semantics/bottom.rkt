#lang typed/racket/base

(provide (rename-out [-Bottom Bottom])
         bottom
         bottom?)

(struct Bottom () #:transparent
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (_ port mode) (write-string "bottom" port))
  )

(define-type -Bottom Bottom)
(define bottom (Bottom))
(define bottom? Bottom?)
