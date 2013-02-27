(define-module (allegro time)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (allegro utils)
  #:export (al-get-time
            al-create-timeout
            al-rest
            al-rest))

(define-wrapped-pointer-type <allegro-timeout>
  allegro-timeout?
  wrap-allegro-timeout unwrap-allegro-timeout
  (lambda (t port)
    (format port "#<allegro-timeout ~x>"
            (pointer-address (unwrap-allegro-timeout t)))))

(define types-timeout (list uint64 uint64))

(define-foreign al-get-time
  double "al_get_time" '())

(define-foreign %al-init-timeout
  void "al_init_timeout" (list '* double))

(define-foreign al-rest
  void "al_rest" (list double))

(define (al-create-timeout seconds)
  (let* ((bv (make-bytevector (sizeof types-timeout)))
         (timeout (bytevector->pointer bv)))
    (%al-init-timeout timeout seconds)
    (wrap-allegro-timeout timeout)))
