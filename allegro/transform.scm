(define-module (allegro transform)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (allegro utils)
  #:export (al-create-transform
            al-identity-transform
            al-identity-transform
            al-identity-transform
            al-copy-transform
            al-use-transform
            al-translate-transform
            al-get-current-transform
            al-invert-transform
            al-transform-invertible?
            al-identity-transform
            al-identity-transform
            al-build-transform
            al-translate-transform
            al-rotate-transform
            al-scale-transform
            al-transform-coordinates
            al-compose-transform))

(define types-transform (make-list 16 float))

(define-wrapped-pointer-type <allegro-transform>
  allegro-transform?
  wrap-allegro-transform unwrap-allegro-transform
  (lambda (t port)
    (format port "#<allegro-transform ~x>"
            (pointer-address (unwrap-allegro-transform t)))))

(define (al-create-transform)
  (let* ((bv (make-bytevector (sizeof types-transform)))
         (transform (wrap-allegro-transform (bytevector->pointer bv))))
    (al-identity-transform transform)
    transform))

(define-foreign %al-copy-transform
  void "al_copy_transform" (list '* '*))

(define-foreign %al-use-transform
  void "al_use_transform" (list '*))

(define-foreign %al-get-current-transform
  '* "al_get_current_transform" '())

(define-foreign %al-invert-transform
  void "al_invert_transform" (list '*))

(define-foreign %al-check-inverse
  int "al_check_inverse" (list '* float))

(define-foreign %al-identity-transform
  void "al_identity_transform" (list '*))

(define-foreign %al-build-transform
  void "al_build_transform" (list '* float float float float float))

(define-foreign %al-translate-transform
  void "al_translate_transform" (list '* float float))

(define-foreign %al-rotate-transform
  void "al_rotate_transform" (list '* float))

(define-foreign %al-scale-transform
  void "al_scale_transform" (list '* float float))

(define-foreign %al-transform-coordinates
  void "al_transform_coordinates" (list '* '* '*))

(define-foreign %al-compose-transform
  void "al_compose_transform" (list '* '*))

(define (al-copy-transform dest src)
  (%al-copy-transform (unwrap-allegro-transform dest)
                      (unwrap-allegro-transform src)))

(define (al-use-transform transform)
  (%al-use-transform (unwrap-allegro-transform transform)))

(define (al-get-current-transform)
  (wrap-allegro-transform (%al-get-current-transform)))

(define (al-invert-transform transform)
  (%al-invert-transform (unwrap-allegro-transform transform)))

(define (al-transform-invertible? transform tolerance)
  (number->boolean (%al-check-inverse (unwrap-allegro-transform transform)
                                      tolerance)))

(define (al-identity-transform transform)
  (%al-identity-transform (unwrap-allegro-transform transform)))

(define (al-build-transform transform x y sx sy theta)
  (%al-build-transform (unwrap-allegro-transform transform)
                       x y sx sy theta))

(define (al-translate-transform transform x y)
  (%al-translate-transform (unwrap-allegro-transform transform) x y))

(define (al-rotate-transform transform theta)
  (%al-rotate-transform (unwrap-allegro-transform transform) theta))

(define (al-scale-transform transform sx sy)
  (%al-scale-transform (unwrap-allegro-transform transform) sx sy))

(define (al-transform-coordinates transform x y)
  (let ((pos (make-bytevector (* (sizeof float) 2))))
    (bytevector-ieee-single-native-set! pos              0 x)
    (bytevector-ieee-single-native-set! pos (sizeof float) y)
    (%al-transform-coordinates (unwrap-allegro-transform transform)
                               (bytevector->pointer pos)
                               (bytevector->pointer pos (sizeof float)))
    (values (bytevector-ieee-single-native-ref pos 0)
            (bytevector-ieee-single-native-ref pos (sizeof float)))))

(define (al-compose-transform transform other)
  (%al-compose-transform (unwrap-allegro-transform transform)
                         (unwrap-allegro-transform other)))
