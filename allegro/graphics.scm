(define-module (allegro graphics)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (allegro utils)
  #:use-module (allegro display)
  #:export (allegro-pixel-format-any
            allegro-pixel-format-any-no-alpha
            allegro-pixel-format-any-with-alpha
            allegro-pixel-format-any-15-no-alpha
            allegro-pixel-format-any-16-no-alpha
            allegro-pixel-format-any-16-with-alpha
            allegro-pixel-format-any-24-no-alpha
            allegro-pixel-format-any-32-no-alpha
            allegro-pixel-format-any-32-with-alpha
            allegro-pixel-format-argb-8888
            allegro-pixel-format-rgba-8888
            allegro-pixel-format-argb-4444
            allegro-pixel-format-rgb-888
            allegro-pixel-format-rgb-565
            allegro-pixel-format-rgb-555
            allegro-pixel-format-rgba-5551
            allegro-pixel-format-argb-1555
            allegro-pixel-format-abgr-8888
            allegro-pixel-format-xbgr-8888
            allegro-pixel-format-bgr-888
            allegro-pixel-format-bgr-565
            allegro-pixel-format-bgr-555
            allegro-pixel-format-rgbx-8888
            allegro-pixel-format-xrgb-8888
            allegro-pixel-format-abgr-f32
            allegro-pixel-format-abgr-8888-le
            allegro-pixel-format-rgba-4444
            allegro-memory-bitmap
            allegro-keep-bitmap-format
            allegro-force-locking
            allegro-no-preserve-texture
            allegro-alpha-test
            allegro-min-linear
            allegro-mag-linear
            allegro-mipmap
            allegro-no-premultiplied-alpha
            allegro-video-bitmap
            allegro-flip-horizontal
            allegro-flip-vertical
            allegro-lock-readwrite
            allegro-lock-readonly
            allegro-lock-writeonly
            allegro-zero
            allegro-one
            allegro-alpha
            allegro-inverse-alpha
            allegro-add
            allegro-src-minus-dest
            allegro-dest-minus-src
            al-map-rgb
            al-map-rgb-f
            al-map-rgba
            al-map-rgba-f
            al-unmap-rgb
            al-unmap-rgb-f
            al-unmap-rgba
            al-unmap-rgba-f
            al-get-pixel-size
            al-get-pixel-format-bits
            al-lock-bitmap
            al-lock-bitmap-region
            al-unlock-bitmap
            al-create-bitmap
            al-create-sub-bitmap
            al-clone-bitmap
            al-destroy-bitmap
            al-get-new-bitmap-flags
            al-get-new-bitmap-format
            al-set-new-bitmap-flags
            al-set-new-bitmap-format
            al-add-new-bitmap-flag
            al-get-bitmap-flags
            al-get-bitmap-format
            al-get-bitmap-height
            al-get-bitmap-width
            al-get-pixel
            al-is-bitmap-locked
            al-is-compatible-bitmap
            al-is-sub-bitmap
            al-get-parent-bitmap
            al-clear-to-color
            al-draw-bitmap
            al-draw-tinted-bitmap
            al-draw-bitmap-region
            al-draw-tinted-bitmap-region
            al-draw-pixel
            al-draw-rotated-bitmap
            al-draw-tinted-rotated-bitmap
            al-draw-scaled-rotated-bitmap
            ;; al-draw-tinted-scaled-rotated-bitmap
            ;; al-draw-tinted-scaled-rotated-bitmap-region
            ;; al-draw-scaled-bitmap
            ;; al-draw-tinted-scaled-bitmap
            al-get-target-bitmap
            al-put-pixel
            al-put-blended-pixel
            al-set-target-bitmap
            al-lock-bitmap
            al-set-target-bitmap
            al-set-target-backbuffer
            al-get-current-display
            al-get-blender
            al-get-separate-blender
            al-set-blender
            al-set-separate-blender
            al-get-clipping-rectangle
            al-set-clipping-rectangle
            al-reset-clipping-rectangle
            al-convert-mask-to-alpha
            al-hold-bitmap-drawing
            al-is-bitmap-drawing-held
            al-register-bitmap-loader
            al-register-bitmap-saver
            al-register-bitmap-loader-f
            al-register-bitmap-saver-f
            al-load-bitmap
            al-load-bitmap-f
            al-save-bitmap
            al-save-bitmap-f))

;; Pixel formats
(define allegro-pixel-format-any               0)
(define allegro-pixel-format-any-no-alpha      1)
(define allegro-pixel-format-any-with-alpha    2)
(define allegro-pixel-format-any-15-no-alpha   3)
(define allegro-pixel-format-any-16-no-alpha   4)
(define allegro-pixel-format-any-16-with-alpha 5)
(define allegro-pixel-format-any-24-no-alpha   6)
(define allegro-pixel-format-any-32-no-alpha   7)
(define allegro-pixel-format-any-32-with-alpha 8)
(define allegro-pixel-format-argb-8888         9)
(define allegro-pixel-format-rgba-8888         10)
(define allegro-pixel-format-argb-4444         11)
(define allegro-pixel-format-rgb-888           12)
(define allegro-pixel-format-rgb-565           13)
(define allegro-pixel-format-rgb-555           14)
(define allegro-pixel-format-rgba-5551         15)
(define allegro-pixel-format-argb-1555         16)
(define allegro-pixel-format-abgr-8888         17)
(define allegro-pixel-format-xbgr-8888         18)
(define allegro-pixel-format-bgr-888           19)
(define allegro-pixel-format-bgr-565           20)
(define allegro-pixel-format-bgr-555           21)
(define allegro-pixel-format-rgbx-8888         22)
(define allegro-pixel-format-xrgb-8888         23)
(define allegro-pixel-format-abgr-f32          24)
(define allegro-pixel-format-abgr-8888-le      25)
(define allegro-pixel-format-rgba-4444         26)

;; Bitmap flags
(define allegro-memory-bitmap          #x0001)
(define allegro-keep-bitmap-format     #x0002)
(define allegro-force-locking          #x0004)
(define allegro-no-preserve-texture    #x0008)
(define allegro-alpha-test             #x0010)
(define allegro-min-linear             #x0040)
(define allegro-mag-linear             #x0080)
(define allegro-mipmap                 #x0100)
(define allegro-no-premultiplied-alpha #x0200)
(define allegro-video-bitmap           #x0400)

;; Flags for blitting procedures
(define allegro-flip-horizontal #x00001)
(define allegro-flip-vertical   #x00002)

;; Locking flags
(define allegro-lock-readwrite 0)
(define allegro-lock-readonly  1)
(define allegro-lock-writeonly 2)

;; Blending modes
(define allegro-zero          0)
(define allegro-one           1)
(define allegro-alpha         2)
(define allegro-inverse-alpha 3)

(define allegro-add            0)
(define allegro-src-minus-dest 1)
(define allegro-dest-minus-src 2)

;; ALLEGRO_COLOR type
(define allegro-color (list float float float float))

(define-foreign %al-map-rgb
  allegro-color "al_map_rgb" (list uint8 uint8 uint8))

(define-foreign %al-map-rgb-f
  allegro-color "al_map_rgb_f" (list float float float))

(define-foreign %al-map-rgba
  allegro-color "al_map_rgba" (list uint8 uint8 uint8 uint8))

(define-foreign %al-map-rgba-f
  allegro-color "al_map_rgba_f" (list float float float float))

(define-foreign %al-unmap-rgb
  void "al_unmap_rgb" (list '* uint8 uint8 uint8))

(define-foreign %al-unmap-rgb-f
  void "al_unmap_rgb_f" (list '* float float float))

(define-foreign %al-unmap-rgba
  void "al_unmap_rgba" (list '* uint8 uint8 uint8 uint8))

(define-foreign %al-unmap-rgba-f
  void "al_unmap_rgba_f" (list '* float float float float))

(define-foreign al-get-pixel-size
  int "al_get_pixel_size" (list int))

(define-foreign al-get-pixel-format-bits
  int "al_get_pixel_format_bits" (list int))

(define-foreign %al-lock-bitmap
  '* "al_lock_bitmap" (list '* int int))

(define-foreign %al-lock-bitmap-region
  '* "al_lock_bitmap_region" (list '* int int int int int int))

(define-foreign %al-unlock-bitmap
  void "al_unlock_bitmap" (list '*))

(define-foreign %al-create-bitmap
  '* "al_create_bitmap" (list int int))

(define-foreign %al-create-sub-bitmap
  '* "al_create_sub_bitmap" (list '* int int int int))

(define-foreign %al-clone-bitmap
  '* "al_clone_bitmap" (list '*))

(define-foreign %al-destroy-bitmap
  void "al_destroy_bitmap" (list '*))

(define-foreign al-get-new-bitmap-flags
  int "al_get_new_bitmap_flags" '())

(define-foreign al-get-new-bitmap-format
  int "al_get_new_bitmap_format" '())

(define-foreign al-set-new-bitmap-flags
  void "al_set_new_bitmap_flags" (list int))

(define-foreign al-add-new-bitmap-flag
  void "al_add_new_bitmap_flag" (list int))

(define-foreign al-set-new-bitmap-format
  void "al_set_new_bitmap_format" (list int))

(define-foreign %al-get-bitmap-flags
  int "al_get_bitmap_flags" (list '*))

(define-foreign %al-get-bitmap-format
  int "al_get_bitmap_format" (list '*))

(define-foreign %al-get-bitmap-height
  int "al_get_bitmap_height" (list '*))

(define-foreign %al-get-bitmap-width
  int "al_get_bitmap_width" (list '*))

(define-foreign %al-get-pixel
  '* "al_get_pixel" (list '* int int))

(define-foreign %al-is-bitmap-locked?
  uint8 "al_is_bitmap_locked" (list '*))

(define-foreign %al-is-compatible-bitmap?
  uint8 "al_is_compatible_bitmap" (list '*))

(define-foreign %al-is-sub-bitmap?
  uint8 "al_is_sub_bitmap" (list '*))

(define-foreign %al-get-parent-bitmap
  '* "al_get_parent_bitmap" (list '*))

(define-foreign %al-clear-to-color
  void "al_clear_to_color" (list allegro-color))

(define-foreign %al-draw-bitmap
  void "al_draw_bitmap" (list '* float float int))

(define-foreign %al-draw-tinted-bitmap
  void "al_draw_tinted_bitmap" (list '* allegro-color float float int))

(define-foreign %al-draw-bitmap-region
  void "al_draw_bitmap_region" (list '* float float float float float float int))

(define-foreign %al-draw-tinted-bitmap-region
  void "al_draw_tinted_bitmap_region" (list '* allegro-color float float float
                                            float float float int))

(define-foreign %al-draw-pixel
  void "al_draw_pixel" (list float float allegro-color))

(define-foreign %al-draw-rotated-bitmap
  void "al_draw_rotated_bitmap" (list '* float float float float float int))

(define-foreign %al-draw-tinted-rotated-bitmap
  void "al_draw_tinted_rotated_bitmap" (list '* allegro-color float float float
                                             float float int))

(define-foreign %al-draw-scaled-rotated-bitmap
  void "al_draw_scaled_rotated_bitmap" (list '* float float float float float
                                             float float int))

;; Arg limit <=10 for foreign functions. FUCK.
;; Fortunately, I think these functions are unnecessary because
;; transforms can be used instead.
;; (define-foreign %al-draw-tinted-scaled-rotated-bitmap
;;   void "al_draw_tinted_scaled_rotated_bitmap" (list '* '* float float float float
;;                                                     float float float int))
;; (define-foreign %al-draw-tinted-scaled-rotated-bitmap-region
;;   void "al_draw_tinted_scaled_rotated_bitmap_region" (list '* float float float
;;                                                            float '* float float
;;                                                            float float float
;;                                                            float float int))
;; (define-foreign %al-draw-scaled-bitmap
;;   void "al_draw_scaled_bitmap" (list '* float float float float float float
;;                                      float float int))
;; (define-foreign %al-draw-tinted-scaled-bitmap
;;   void "al_draw_tinted_scaled_bitmap" (list '* '* float float float float float
;;                                             float float float int))

(define-foreign %al-get-target-bitmap
  '* "al_get_target_bitmap" '())

(define-foreign %al-put-pixel
  void "al_put_pixel" (list int int allegro-color))

(define-foreign %al-put-blended-pixel
  void "al_put_blended_pixel" (list int int allegro-color))

(define-foreign %al-set-target-bitmap
  void "al_set_target_bitmap" (list '*))

(define-foreign %al-set-target-backbuffer
  void "al_set_target_backbuffer" (list '*))

(define-foreign %al-get-current-display
  '* "al_get_current_display" '())

(define-foreign %al-get-blender
  void "al_get_blender" (list '* '* '*))

(define-foreign %al-get-separate-blender
  void "al_get_separate_blender" (list '* '* '* '* '* '*))

(define-foreign al-set-blender
  void "al_set_blender" (list int int int))

(define-foreign al-set-separate-blender
  void "al_set_separate_blender" (list int int int int int int))

(define-foreign %al-get-clipping-rectangle
  void "al_get_clipping_rectangle" (list int int int int))

(define-foreign %al-set-clipping-rectangle
  void "al_set_clipping_rectangle" (list int int int int))

(define-foreign %al-reset-clipping-rectangle
  void "al_reset_clipping_rectangle" '())

(define-foreign %al-convert-mask-to-alpha
  void "al_convert_mask_to_alpha" (list '* '*))

(define-foreign %al-hold-bitmap-drawing
  void "al_hold_bitmap_drawing" (list uint8))

(define-foreign %al-is-bitmap-drawing-held?
  uint8 "al_is_bitmap_drawing_held" '())

(define-foreign %al-register-bitmap-loader
  uint8 "al_register_bitmap_loader" (list '* '*))

(define-foreign %al-register-bitmap-saver
  uint8 "al_register_bitmap_saver" (list '* uint8 '*))

(define-foreign %al-register-bitmap-loader-f
  uint8 "al_register_bitmap_loader_f" (list '* '*))

(define-foreign %al-register-bitmap-saver-f
  uint8 "al_register_bitmap_saver_f" (list '* uint8 '*))

(define-foreign %al-load-bitmap
  '* "al_load_bitmap" (list '*))

(define-foreign %al-load-bitmap-f
  '* "al_load_bitmap_f" (list '* '*))

(define-foreign %al-save-bitmap
  uint8 "al_save_bitmap" (list '* '*))

(define-foreign %al-save-bitmap-f
  uint8 "al_save_bitmap_f" (list '* '* '*))

;; Color
(define (pointer->color pointer)
  (parse-c-struct pointer allegro-color))

(define* (color->pointer r g b #:optional (a 1.0))
  (make-c-struct allegro-color (list r g b a)))

;; Why does this always return #000000?
(define (al-map-rgb r g b)
  (pointer->color (%al-map-rgb r g b)))

(define (al-map-rgb-f r g b)
  (pointer->color (%al-map-rgb-f r g b)))

(define (al-map-rgba r g b a)
  (pointer->color (%al-map-rgba r g b a)))

(define (al-map-rgba-f r g b a)
  (pointer->color (%al-map-rgba-f r g b a)))

;; Bitmap
(define-wrapped-pointer-type <allegro-bitmap>
  allegro-bitmap?
  wrap-allegro-bitmap unwrap-allegro-bitmap
  (lambda (b port)
    (format port "<allegro-bitmap ~d ~d ~x>"
            (al-get-bitmap-width b)
            (al-get-bitmap-height b)
            (pointer-address (unwrap-allegro-bitmap b)))))

(define-wrapped-pointer-type <allegro-locked-region>
  allegro-locked-region?
  wrap-allegro-locked-region unwrap-allegro-locked-region
  (lambda (r port)
    (format port "<allegro-locked-region ~x>"
            (pointer-address (unwrap-allegro-locked-region r)))))

(define (al-lock-bitmap bitmap format flags)
  (wrap-allegro-locked-region (%al-lock-bitmap (wrap-allegro-bitmap bitmap)
                                               format flags)))

(define (al-lock-bitmap-region bitmap x y width height format flags)
  (wrap-allegro-locked-region
   (%al-lock-bitmap-region (wrap-allegro-bitmap bitmap)
                           x y width height format flags)))

(define (al-unlock-bitmap bitmap)
  (%al-unlock-bitmap (unwrap-allegro-bitmap bitmap)))

(define (al-create-bitmap width height)
  (wrap-allegro-bitmap (al-create-bitmap width height)))

(define (al-create-sub-bitmap parent-bitmap x y width height)
  (wrap-allegro-bitmap
   (al-create-sub-bitmap (unwrap-allegro-bitmap parent-bitmap)
                         x y width height)))

(define (al-clone-bitmap bitmap)
  (wrap-allegro-bitmap (%al-clone-bitmap (unwrap-allegro-bitmap bitmap))))

(define (al-destroy-bitmap bitmap)
  (%al-destroy-bitmap (unwrap-allegro-bitmap bitmap)))

(define (al-get-bitmap-flags bitmap)
  (%al-get-bitmap-flags (unwrap-allegro-bitmap bitmap)))

(define (al-get-bitmap-format bitmap)
  (%al-get-bitmap-format (unwrap-allegro-bitmap bitmap)))

(define (al-get-bitmap-width bitmap)
  (%al-get-bitmap-width (unwrap-allegro-bitmap bitmap)))

(define (al-get-bitmap-height bitmap)
  (%al-get-bitmap-height (unwrap-allegro-bitmap bitmap)))

(define (al-get-pixel bitmap x y)
  (pointer->color (%al-get-pixel (unwrap-allegro-bitmap bitmap) x y)))

(define (al-is-bitmap-locked? bitmap)
  (number->boolean (%al-is-bitmap-locked? (unwrap-allegro-bitmap bitmap))))

(define (al-is-compatible-bitmap? bitmap)
  (number->boolean (%al-is-compatible-bitmap (unwrap-allegro-bitmap bitmap))))

(define (al-is-sub-bitmap? bitmap)
  (number->boolean (%al-is-sub-bitmap? (unwrap-allegro-bitmap bitmap))))

(define (al-get-parent-bitmap bitmap)
  (wrap-allegro-bitmap (%al-get-parent-bitmap (unwrap-allegro-bitmap bitmap))))

(define* (al-clear-to-color r g b #:optional (a 1.0))
  (%al-clear-to-color (color->pointer r g b a)))

(define* (al-draw-bitmap bitmap x y #:optional (flags 0))
  (%al-draw-bitmap (unwrap-allegro-bitmap bitmap) x y flags))

(define* (al-draw-tinted-bitmap bitmap tint x y #:optional (flags 0))
  (%al-draw-tinted-bitmap (unwrap-allegro-bitmap bitmap)
                          (apply color->pointer tint)
                          x y flags))

(define* (al-draw-bitmap-region bitmap sx sy sw sh dx dy #:optional (flags 0))
  (%al-draw-bitmap-region (unwrap-allegro-bitmap bitmap)
                          sx sy sw sh dx dy flags))

(define* (al-draw-tinted-bitmap-region bitmap tint sx sy sw sh dx dy
                                       #:optional (flags 0))
  (%al-draw-tinted-bitmap-region (unwrap-allegro-bitmap bitmap)
                                 (apply color->pointer tint)
                                 sx sy sw sh dx dy flags))

(define* (al-draw-rotated-bitmap bitmap cx cy dx dy angle #:optional (flags 0))
  (%al-draw-rotated-bitmap (unwrap-allegro-bitmap bitmap)
                           cx cy dx dy angle flags))

(define* (al-draw-tinted-rotated-bitmap bitmap tint cx cy dx dy angle
                                        #:optional (flags 0))
  (%al-draw-tinted-rotated-bitmap (unwrap-allegro-bitmap bitmap)
                                  (apply color->pointer tint)
                                  cx cy dx dy angle flags))

(define* (al-draw-scaled-rotated-bitmap bitmap cx cy dx dy xscale yscale
                                       angle #:optional (flags 0))
  (%al-draw-scaled-rotated-bitmap (unwrap-allegro-bitmap bitmap)
                                  cx cy dx dy xscale yscale angle flags))
(define (al-draw-pixel x y color)
  (%al-draw-pixel x y (apply color->pointer color)))

(define (al-get-target-bitmap)
  (wrap-allegro-bitmap (%al-get-target-bitmap)))

(define (al-put-pixel x y color)
  (%al-put-pixel x y (apply color->pointer color)))

(define (al-put-blended-pixel x y color)
  (%al-put-blended-pixel x y (apply color->pointer color)))

(define (al-set-target-bitmap bitmap)
  (%al-set-target-bitmap (unwrap-allegro-bitmap bitmap)))

(define (al-set-target-backbuffer display)
  (%al-set-target-backbuffer (unwrap-allegro-display display)))

(define (al-get-current-display)
  (wrap-allegro-display (%al-get-current-display)))

;; TODO: BLENDERS!!!
(define (al-get-blender)
  (let ((op  (make-bytevector (sizeof int)))
        (src (make-bytevector (sizeof int)))
        (dst (make-bytevector (sizeof int))))
    (%al-get-blender (bytevector->pointer op)
                     (bytevector->pointer src)
                     (bytevector->pointer dst))
    (values (bytevector->int op)
            (bytevector->int src)
            (bytevector->int dst))))

;; TODO: Clipping

(define (al-hold-bitmap-drawing hold)
  (%al-hold-bitmap-drawing (boolean->number hold)))

(define (al-is-bitmap-drawing-held?)
  (number->boolean (%al-is-bitmap-drawing-held?)))

(define (al-load-bitmap filename)
  (wrap-allegro-bitmap (%al-load-bitmap (string->pointer filename))))

(define (al-save-bitmap filename bitmap)
  (number->boolean (%al-save-bitmap (string->pointer filename)
                                    (unwrap-allegro-bitmap bitmap))))
