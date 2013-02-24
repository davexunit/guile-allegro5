(define-module (allegro utils)
  #:use-module (system foreign)
  #:use-module (ice-9 regex)
  #:export (number->boolean
            define-foreign))

(define liballegro (dynamic-link "liballegro"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name liballegro) args)))

(define (number->boolean n)
  (not (zero? n)))
