(define-module (allegro addons font)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (allegro utils)
  #:use-module (allegro graphics)
  #:export (allegro-align-left
            allegro-align-centre
            allegro-align-right
            al-init-font-addon
            al-shutdown-font-addon
            al-load-font
            al-destroy-font
            ;al-register-font-loader
            al-get-font-line-height
            al-get-font-ascent
            al-get-font-descent
            al-get-text-width
            ;al-get-ustr-width
            al-draw-text
            ;al-draw-ustr
            al-draw-justified-text
            al-draw-justified-ustr
            al-draw-textf
            al-draw-justified-textf
            al-get-text-dimensions
            ;al-get-ustr-dimensions
            al-get-allegro-font-version
            al-grab-font-from-bitmap
            al-load-bitmap-font
            wrap-allegro-font))

;; Constants
(define allegro-align-left   0)
(define allegro-align-centre 1)
(define allegro-align-right  2)

;; Shared library
(define liballegro-font (dynamic-link "liballegro_font"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name liballegro-font) args)))

;; Font wrapper
(define-wrapped-pointer-type <allegro-font>
  allegro-font?
  wrap-allegro-font unwrap-allegro-font
  (lambda (b port)
    (format port "<allegro-font ~x>"
            (pointer-address (unwrap-allegro-font b)))))

;; Foreign function bindings
(define-foreign al-init-font-addon
  void "al_init_font_addon" '())

(define-foreign al-shutdown-font-addon
  void "al_shutdown_font_addon" '())

(define-foreign %al-load-font
  '* "al_load_font" (list '* int int))

(define-foreign %al-destroy-font
  void "al_destroy_font" (list '*))

(define-foreign %al-register-font-loader
  uint8 "al_register_font_loader" (list '* '* int int))

(define-foreign %al-get-font-line-height
  int "al_get_font_line_height" (list '*))

(define-foreign %al-get-font-ascent
  int "al_get_font_ascent" (list '*))

(define-foreign %al-get-font-descent
  int "al_get_font_descent" (list '*))

(define-foreign %al-get-text-width
  int "al_get_text_width" (list '* '*))

(define-foreign %al-get-ustr-width
  int "al_get_ustr_width" (list '* '*))

(define-foreign %al-draw-text
  void "al_draw_text" (list '* allegro-color float float int '*))

(define-foreign %al-draw-ustr
  void "al_draw_ustr" (list '* allegro-color  float float int '*))

(define-foreign %al-draw-justified-text
  void "al_draw_justified_text" (list '* '* float float float float int '*))

(define-foreign %al-get-text-dimensions
  void "al_get_text_dimensions" (list '* '* int int int int))

(define-foreign %al-get-ustr-dimensions
  void "al_get_ustr_dimensions" (list '* '* int int int int))

(define-foreign al-get-allegro-font-version
  uint32 "al_get_allegro_font_version" '())

(define-foreign %al-grab-font-from-bitmap
  '* "al_grab_font_from_bitmap" (list '* int int))

(define-foreign %al-load-bitmap-font
  '* "al_load_bitmap_font" (list '*))

;; Wrappers
(define (al-load-font filename size flags)
  (wrap-allegro-font (%al-load-font (string->pointer filename)
                                    size flags)))

(define (al-destroy-font font)
  (%al-destroy-font (unwrap-allegro-font font)))

(define (al-get-font-line-height font)
  (%al-get-font-line-height (unwrap-allegro-font font)))

(define (al-get-font-ascent font)
  (%al-get-font-ascent (unwrap-allegro-font font)))

(define (al-get-font-descent font)
  (%al-get-font-descent (unwrap-allegro-font font)))

(define (al-get-text-width font text)
  (%al-get-text-width (unwrap-allegro-font font)
                      (string->pointer text)))

(define (al-draw-text font color x y flags text)
  (%al-draw-text (unwrap-allegro-font font)
                 (apply color->pointer color)
                 x y flags
                 (string->pointer text)))

(define (al-draw-justified-text font color x1 x2 y diff flags text)
  (%al-draw-justified-text (unwrap-allegro-font font)
                           (apply color->pointer color)
                           x1 x1 y diff flags
                           (string->pointer text)))

(define (al-draw-textf font color x y flags format-string . args)
  (let ((text (apply format #f format-string args)))
    (%al-draw-text (unwrap-allegro-font font)
                   (apply color->pointer color)
                   x y flags
                   (string->pointer text))))

(define (al-draw-justified-textf font color x1 x2 y diff flags
                                 format-string . args)
  (let ((text (apply format #f format-string args)))
    (%al-draw-justified-text (unwrap-allegro-font font)
                             (apply color->pointer color)
                             x1 x1 y diff flags
                             (string->pointer text))))

(define (al-get-text-dimensions font text)
  (let ((bv (make-bytevector (* (sizeof int) 4)))
        (y-offset (sizeof int))
        (w-offset (* (sizeof int) 2))
        (h-offset (* (sizeof int) 3)))
    (%al-get-text-dimensions (unwrap-allegro-font font)
                             (bytevector->pointer bv)
                             (bytevector->pointer bv y-offset)
                             (bytevector->pointer bv w-offset)
                             (bytevector->pointer bv h-offset))
    (values (bytevector->int bv)
            (bytevector->int bv y-offset)
            (bytevector->int bv w-offset)
            (bytevector->int bv h-offset))))

;; TODO
(define (al-grab-font-from-bitmap bitmap ranges)
  (define (ranges->pointer)
    #f)
  (let ((num-ranges (length ranges)))
    (wrap-allegro-font
     (%al-grab-font-from-bitmap (unwrap-allegro-bitmap bitmap)
                                num-ranges
                                (ranges->pointer)))))

(define (al-load-bitmap-font filename)
  (wrap-allegro-font (%al-load-bitmap-font (pointer->string filename))))
