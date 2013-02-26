(define-module (allegro utils)
  #:use-module (system foreign)
  #:use-module (ice-9 regex)
  #:use-module (rnrs bytevectors)
  #:export (number->boolean
            boolean->number
            bytevector->int
            define-foreign))

(define liballegro (dynamic-link "liballegro"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name liballegro) args)))

(define (number->boolean n)
  (not (zero? n)))

(define (boolean->number b)
  (if b 1 0))

(define* (bytevector->int bv #:optional (offset 0))
  (bytevector-sint-ref bv offset (native-endianness) (sizeof int)))
