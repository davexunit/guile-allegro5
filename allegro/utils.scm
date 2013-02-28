(define-module (allegro utils)
  #:use-module (system foreign)
  #:use-module (ice-9 regex)
  #:use-module (rnrs bytevectors)
  #:export (number->boolean
            boolean->number
            bytevector->int
            define-foreign
            allegro-color
            pointer->color
            color->pointer))

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

;; ALLEGRO_COLOR type
(define allegro-color (list float float float float))

;; Color
(define (pointer->color pointer)
  (parse-c-struct pointer allegro-color))

(define* (color->pointer r g b #:optional (a 1.0))
  (make-c-struct allegro-color (list r g b a)))
