(define-module (allegro addons image)
  #:use-module (system foreign)
  #:export (al-init-image-addon
            al-shutdown-image-addon
            al-get-allegro-image-version))

(define liballegro-image (dynamic-link "liballegro_image"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name liballegro-image) args)))

(define-foreign %al-init-image-addon
  uint8 "al_init_image_addon" '())

(define-foreign al-shutdown-image-addon
  void "al_shutdown_image_addon" '())

(define-foreign al-get-allegro-image-version
  uint32 "al_get_allegro_image_version" '())

(define (al-init-image-addon)
  (number->boolean (%al-init-image-addon)))
