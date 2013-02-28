(define-module (allegro addons ttf)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (allegro utils)
  #:use-module (allegro addons font)
  #:export (al-init-ttf-addon
            al-shutdown-ttf-addon
            al-load-ttf-font
            ;al-load-ttf-font-f
            al-load-ttf-font-stretch
            ;al-load-ttf-font-stretch-f
            al-get-allegro-ttf-version))

(define liballegro-ttf (dynamic-link "liballegro_ttf"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name liballegro-ttf) args)))

;; Foreign function bindings
(define-foreign %al-init-ttf-addon
  uint8 "al_init_ttf_addon" '())

(define-foreign al-shutdown-ttf-addon
  void "al_shutdown_ttf_addon" '())

(define-foreign %al-load-ttf-font
  '* "al_load_ttf_font" (list '* int int))

(define-foreign %al-load-ttf-font-f
  '* "al_load_ttf_font_f" (list '* '* int int))

(define-foreign %al-load-ttf-font-stretch
  '* "al_load_ttf_font_stretch" (list '* int int int))

(define-foreign %al-load-ttf-font-stretch-f
  '* "al_load_ttf_font_stretch_f" (list '* '* int int int))

(define-foreign al-get-allegro-ttf-version
  uint32 "al_get_allegro_ttf_version" '())

;; Wrapper procedures
(define (al-init-ttf-addon)
  (number->boolean (%al-init-ttf-addon)))

(define (al-load-ttf-font filename size flags)
  (wrap-allegro-font (%al-load-ttf-font (string->pointer filename)
                                        size flags)))

(define (al-load-ttf-font-stretch filename width height flags)
  (wrap-allegro-font (%al-load-ttf-font-stretch (string->pointer filename)
                                                width height flags)))
