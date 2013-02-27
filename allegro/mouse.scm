(define-module (allegro mouse)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (allegro utils)
  #:use-module (allegro display)
  #:use-module (allegro events)
  #:export (allegro-mouse-x
            allegro-mouse-y
            allegro-mouse-z
            allegro-mouse-w
            al-install-mouse
            al-is-mouse-installed?
            al-uninstall-mouse
            al-get-mouse-num-axes
            al-get-mouse-num-buttons
            al-get-mouse-state
            al-get-mouse-state
            al-get-mouse-state-axis
            al-mouse-button-down?
            al-set-mouse-position
            al-set-mouse-z
            al-set-mouse-w
            al-set-mouse-axis
            al-get-mouse-event-source
            al-create-mouse-cursor
            al-destroy-mouse-cursor
            al-set-mouse-cursor
            al-set-system-mouse-cursor
            al-get-mouse-cursor-position
            al-hide-mouse-cursor
            al-show-mouse-cursor
            al-grab-mouse
            al-ungrab-mouse
            allegro-system-mouse-cursor-none
            allegro-system-mouse-cursor-default
            allegro-system-mouse-cursor-arrow
            allegro-system-mouse-cursor-busy
            allegro-system-mouse-cursor-question
            allegro-system-mouse-cursor-edit
            allegro-system-mouse-cursor-move
            allegro-system-mouse-cursor-resize-n
            allegro-system-mouse-cursor-resize-w
            allegro-system-mouse-cursor-resize-s
            allegro-system-mouse-cursor-resize-e
            allegro-system-mouse-cursor-resize-nw
            allegro-system-mouse-cursor-resize-sw
            allegro-system-mouse-cursor-resize-se
            allegro-system-mouse-cursor-resize-ne
            allegro-system-mouse-cursor-progress
            allegro-system-mouse-cursor-precision
            allegro-system-mouse-cursor-link
            allegro-system-mouse-cursor-alt-select
            allegro-system-mouse-cursor-unavailable))

(define allegro-system-mouse-cursor-none         0)
(define allegro-system-mouse-cursor-default      1)
(define allegro-system-mouse-cursor-arrow        2)
(define allegro-system-mouse-cursor-busy         3)
(define allegro-system-mouse-cursor-question     4)
(define allegro-system-mouse-cursor-edit         5)
(define allegro-system-mouse-cursor-move         6)
(define allegro-system-mouse-cursor-resize-n     7)
(define allegro-system-mouse-cursor-resize-w     8)
(define allegro-system-mouse-cursor-resize-s     9)
(define allegro-system-mouse-cursor-resize-e    10)
(define allegro-system-mouse-cursor-resize-nw   11)
(define allegro-system-mouse-cursor-resize-sw   12)
(define allegro-system-mouse-cursor-resize-se   13)
(define allegro-system-mouse-cursor-resize-ne   14)
(define allegro-system-mouse-cursor-progress    15)
(define allegro-system-mouse-cursor-precision   16)
(define allegro-system-mouse-cursor-link        17)
(define allegro-system-mouse-cursor-alt-select  18)
(define allegro-system-mouse-cursor-unavailable 19)

(define allegro-mouse-x 0)
(define allegro-mouse-y 1)
(define allegro-mouse-z 2)
(define allegro-mouse-w 3)

(define-wrapped-pointer-type <allegro-mouse-state>
  allegro-mouse-state?
  wrap-allegro-mouse-state unwrap-allegro-mouse-state
  (lambda (ms port)
    (format port "<allegro-mouse-state ~x>"
            (pointer-address (unwrap-allegro-mouse-state ms)))))

(define-wrapped-pointer-type <allegro-mouse-cursor>
  allegro-mouse-cursor?
  wrap-allegro-mouse-cursor unwrap-allegro-mouse-cursor
  (lambda (mc port)
    (format port "#<allegro-mouse-cursor ~x>"
            (pointer-address (unwrap-allegro-mouse-cursor mc)))))

(define types-mouse-state (list int int int int
                                int int int int
                                int float '*))

(define-foreign %al-install-mouse
  uint8 "al_install_mouse" '())

(define-foreign %al-is-mouse-installed?
  uint8 "al_is_mouse_installed" '())

(define-foreign al-uninstall-mouse
  void "al_uninstall_mouse" '())

(define-foreign al-get-mouse-num-axes
  unsigned-int "al_get_mouse_num_axes" '())

(define-foreign al-get-mouse-num-buttons
  unsigned-int "al_get_mouse_num_buttons" '())

(define-foreign %al-get-mouse-state
  void "al_get_mouse_state" (list '*))

(define-foreign %al-get-mouse-state-axis
  int "al_get_mouse_state_axis" (list '* int))

(define-foreign %al-mouse-button-down?
  uint8 "al_mouse_button_down" (list '* int))

(define-foreign %al-set-mouse-xy
  uint8 "al_set_mouse_xy" (list '* int int))

(define-foreign %al-set-mouse-z
  uint8 "al_set_mouse_z" (list int))

(define-foreign %al-set-mouse-w
  uint8 "al_set_mouse_w" (list int))

(define-foreign %al-set-mouse-axis
  uint8 "al_set_mouse_axis" (list int int))

(define-foreign %al-get-mouse-event-source
  '* "al_get_mouse_event_source" '())

(define-foreign %al-create-mouse-cursor
  '* "al_create_mouse_cursor" (list '* int int))

(define-foreign %al-destroy-mouse-cursor
  void "al_destroy_mouse_cursor" (list '*))

(define-foreign %al-set-mouse-cursor
  uint8 "al_set_mouse_cursor" (list '* '*))

(define-foreign %al-set-system-mouse-cursor
  uint8 "al_set_system_mouse_cursor" (list '* int))

(define-foreign %al-get-mouse-cursor-position
  uint8 "al_get_mouse_cursor_position" (list '* '*))

(define-foreign %al-hide-mouse-cursor
  uint8 "al_hide_mouse_cursor" (list '*))

(define-foreign %al-show-mouse-cursor
  uint8 "al_show_mouse_cursor" (list '*))

(define-foreign %al-grab-mouse
  uint8 "al_grab_mouse" (list '*))

(define-foreign %al-ungrab-mouse
  uint8 "al_ungrab_mouse" '())

(define (al-install-mouse)
  (number->boolean (%al-install-mouse)))

(define (al-is-mouse-installed?)
  (number->boolean (%al-is-mouse-installed?)))

(define (al-get-mouse-state)
  (let* ((v (make-bytevector (sizeof types-mouse-state)))
         (pointer (bytevector->pointer v)))
    (%al-get-mouse-state pointer)
    (wrap-allegro-mouse-state pointer)))

(define (al-get-mouse-state-axis mouse-state axis)
  (%al-get-mouse-state-axis (unwrap-allegro-mouse-state mouse-state) axis))

(define (al-mouse-button-down? mouse-state button)
  (number->boolean
   (%al-mouse-button-down? (unwrap-allegro-mouse-state mouse-state) button)))

(define (al-set-mouse-position display x y)
  (number->boolean (%al-set-mouse-xy (unwrap-allegro-display display) x y)))

(define (al-set-mouse-z z)
  (number->boolean (%al-set-mouse-z z)))

(define (al-set-mouse-w w)
  (number->boolean (%al-set-mouse-w w)))

(define (al-set-mouse-axis axis value)
  (number->boolean (%al-set-mouse-axis axis value)))

(define (al-get-mouse-event-source)
  (wrap-allegro-event-source (%al-get-mouse-event-source)))

(define (al-create-mouse-cursor bitmap x-focus y-focus)
  (wrap-allegro-mouse-cursor (%al-create-mouse-cursor bitmap x-focus y-focus)))

(define (al-destroy-mouse-cursor cursor)
  (%al-destroy-mouse-cursor (unwrap-allegro-mouse-cursor cursor)))

(define (al-set-mouse-cursor display cursor)
  (number->boolean
   (%al-set-mouse-cursor (unwrap-allegro-display display)
                         (unwrap-allegro-mouse-cursor cursor))))

(define (al-set-system-mouse-cursor display cursor-id)
  (number->boolean
   (%al-set-system-mouse-cursor (unwrap-allegro-display display) cursor-id)))

(define (al-get-mouse-cursor-position)
  (let* ((pos (make-bytevector (* (sizeof int) 2)))
         (x (bytevector->pointer pos))
         (y (bytevector->pointer pos (sizeof int)))
         (ret (%al-get-mouse-cursor-position x y)))
    (values (number->boolean ret)
            (bytevector->int pos)
            (bytevector->int pos (sizeof int)))))

(define (al-hide-mouse-cursor display)
  (number->boolean (%al-hide-mouse-cursor (unwrap-allegro-display display))))

(define (al-show-mouse-cursor display)
  (number->boolean (%al-show-mouse-cursor (unwrap-allegro-display display))))

(define (al-grab-mouse display)
  (number->boolean (%al-grab-mouse (unwrap-allegro-display display))))

(define (al-ungrab-mouse)
  (number->boolean (%al-ungrab-mouse)))
