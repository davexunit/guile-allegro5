(define-module (allegro display)
  #:use-module (system foreign)
  #:use-module (allegro utils)
  #:use-module (rnrs bytevectors)
  #:export (al-create-display
            al-destroy-display
            al-get-new-display-flags
            al-get-new-display-refresh-rate
            al-get-new-window-position
            al-set-new-display-option
            al-get-new-display-option
            al-reset-new-display-options
            al-set-new-display-flags
            al-set-new-display-refresh-rate
            al-set-new-window-position
            al-acknowledge-resize
            al-flip-display
            al-get-backbuffer
            al-get-display-flags
            al-get-display-format
            al-get-display-height
            al-get-display-refresh-rate
            al-get-display-width
            al-get-window-position
            al-inhibit-screensaver
            al-resize-display
            al-set-display-icon
            al-get-display-option
            al-set-window-position
            al-set-window-title
            al-set-display-flag
            al-toggle-display-flag
            al-update-display-region
            al-wait-for-vsync
            al-get-display-event-source
            al-get-display-mode
            al-get-num-display-modes
            al-get-new-display-adapter
            al-set-new-display-adapter
            al-get-monitor-info
            al-get-num-video-adapters
            unwrap-allegro-display
            wrap-allegro-display))

;; Foreign function bindings
(define-foreign %al-create-display
  '* "al_create_display" (list int int))

(define-foreign %al-destroy-display
  void "al_destroy_display" (list '*))

(define-foreign al-get-new-display-flags
  int "al_get_new_display_flags" '())

(define-foreign al-get-new-display-refresh-rate
  int "al_get_new_display_refresh_rate" '())

(define-foreign %al-get-new-window-position
  void "al_get_new_window_position" (list '* '*))

(define-foreign al-set-new-display-option
  void "al_set_new_display_option" (list int int int))

(define-foreign %al-get-new-display-option
  int "al_get_new_display_option" (list int int))

(define-foreign al-reset-new-display-options
  void "al_reset_new_display_options" '())

(define-foreign al-set-new-display-flags
  void "al_set_new_display_flags" (list int))

(define-foreign al-set-new-display-refresh-rate
  void "al_set_new_display_refresh_rate" (list int))

(define-foreign al-set-new-window-position
  void "al_set_new_window_position" (list int int))

(define-foreign %al-acknowledge-resize
  uint8 "al_acknowledge_resize" (list '*))

(define-foreign al-flip-display
  void "al_flip_display" '())

(define-foreign %al-get-backbuffer
  '* "al_get_backbuffer" (list '*))

(define-foreign %al-get-display-flags
  int "al_get_display_flags" (list '*))

(define-foreign %al-get-display-format
  int "al_get_display_format" (list '*))

(define-foreign %al-get-display-height
  int "al_get_display_height" (list '*))

(define-foreign %al-get-display-refresh-rate
  int "al_get_display_refresh_rate" (list '*))

(define-foreign %al-get-display-width
  int "al_get_display_width" (list '*))

(define-foreign %al-get-window-position
  void "al_get_window_position" (list '* '* '*))

(define-foreign %al-inhibit-screensaver
  uint8 "al_inhibit_screensaver" (list uint8))

(define-foreign %al-resize-display
  uint8 "al_resize_display" (list '* int int))

(define-foreign %al-set-display-icon
  void "al_set_display_icon" (list '* '*))

(define-foreign %al-get-display-option
  int "al_get_display_option" (list '* int))

(define-foreign %al-set-window-position
  void "al_set_window_position" (list '* int int))

(define-foreign %al-set-window-title
  void "al_set_window_title" (list '* '*))

(define-foreign %al-set-display-flag
  uint8 "al_set_display_flag" (list uint8 int '*))

(define-foreign %al-toggle-display-flag
  uint8 "al_toggle_display_flag" (list uint8 int '*))

(define-foreign %al-update-display-region
  void "al_update_display_region" (list int int int int))

(define-foreign %al-wait-for-vsync
  uint8 "al_wait_for_vsync" '())

(define-foreign %al-get-display-event-source
  '* "al_get_display_event_source" (list '*))

(define-foreign %al-get-display-mode
  '* "al_get_display_mode" (list '* int))

(define-foreign al-get-num-display-modes
  int "al_get_num_display_modes" '())

(define-foreign al-get-new-display-adapter
  int "al_get_new_display_adapter" '())

(define-foreign %al-set-new-display-adapter
  void "al_set_new_display_adapter" (list int))

(define-foreign %al-get-monitor-info
  uint8 "al_get_monitor_info" (list '* int))

(define-foreign %al-get-num-video-adapters
  int "al_get_num_video_adapters" '())

;; Wrappers
(define-wrapped-pointer-type <allegro-display>
  allegro-display?
  wrap-allegro-display unwrap-allegro-display
  (lambda (d port)
    (format port "#<allegro-display ~d ~d ~x>"
            (al-get-display-width d)
            (al-get-display-height d)
            (pointer-address (unwrap-allegro-display d)))))

(define (al-create-display width height)
  (wrap-allegro-display (%al-create-display width height)))

(define (al-destroy-display display)
  (%al-destroy-display (unwrap-allegro-display display)))

(define (al-get-display-flags display)
  (%al-get-display-flags (unwrap-allegro-display display)))

(define (al-get-display-format display)
  (%al-get-display-format (unwrap-allegro-display display)))

(define (al-get-display-width display)
  (%al-get-display-width (unwrap-allegro-display display)))

(define (al-get-display-height display)
  (%al-get-display-height (unwrap-allegro-display display)))

(define (al-get-display-refresh-rate display)
  (%al-get-display-refresh-rate (unwrap-allegro-display display)))

(define (al-get-window-position display)
  (let ((x (make-bytevector (sizeof int)))
        (y (make-bytevector (sizeof int))))
    (%al-get-window-position (unwrap-allegro-display display)
                             (bytevector->pointer x)
                             (bytevector->pointer y))
    (values (bytevector->int x)
            (bytevector->int y))))

(define (al-inhibit-screensaver inhibit)
  (number->boolean (%al-inhibit-screensaver (boolean->number inhibit))))

(define (al-resize-display display width height)
  (number->boolean (%al-resize-display (unwrap-allegro-display display) width height)))

(define (al-get-display-option display option)
  (%al-get-display-option (unwrap-allegro-display display) option))

(define (al-set-window-position display x y)
  (%al-set-window-position (unwrap-allegro-display display) x y))

(define (al-set-window-title display title)
  (%al-set-window-title (unwrap-allegro-display display) (string->pointer title)))
