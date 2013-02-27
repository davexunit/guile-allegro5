(define-module (allegro events)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (allegro utils)
  #:export (al-create-event-queue
            al-destroy-event-queue
            al-register-event-source
            al-unregister-event-source
            al-is-event-queue-empty?
            al-get-next-event
            al-peek-next-event
            al-drop-next-event
            al-flush-event-queue
            al-wait-for-event
            al-wait-for-event-timed
            ;; al-wait-for-event-until
            ;; al-init-user-event-source
            ;; al-destroy-user-event-source
            ;; al-emit-user-event
            ;; al-unref-user-event
            ;; al-get-event-source-data
            ;; al-set-event-source-data
            wrap-allegro-event-source
            allegro-event?
            al-get-event-type
            al-get-event-source
            al-get-event-timestamp
            al-get-display-event
            al-get-timer-event
            al-get-key-event
            al-get-mouse-event
            al-get-joystick-event
            allegro-timer-event?
            al-get-timer-event-src
            al-get-timer-event-timestamp
            al-get-timer-event-count
            al-get-timer-event-error
            allegro-display-event?
            al-get-display-event-src
            al-get-display-event-timestamp
            al-get-display-event-x
            al-get-display-event-y
            al-get-display-event-width
            al-get-display-event-height
            allegro-joystick-event?
            al-get-joystick-event-src
            al-get-joystick-event-timestamp
            al-get-joystick-event-id
            al-get-joystick-event-stick
            al-get-joystick-event-axis
            al-get-joystick-event-pos
            al-get-joystick-event-button
            allegro-key-event?
            al-get-key-event-source
            al-get-key-event-timestamp
            al-get-key-event-display
            al-get-key-event-keycode
            al-get-key-event-unichar
            al-get-key-event-modifiers
            al-get-key-event-repeat
            allegro-mouse-event?
            al-get-mouse-event-src
            al-get-mouse-event-timestamp
            al-get-mouse-event-x
            al-get-mouse-event-y
            al-get-mouse-event-z
            al-get-mouse-event-w
            al-get-mouse-event-dx
            al-get-mouse-event-dy
            al-get-mouse-event-dz
            al-get-mouse-event-dw
            al-get-mouse-event-button
            al-get-mouse-event-pressure
            allegro-event-joystick-axis
            allegro-event-joystick-button-down
            allegro-event-joystick-button-up
            allegro-event-joystick-configuration
            allegro-event-key-down
            allegro-event-key-char
            allegro-event-key-up
            allegro-event-mouse-axes
            allegro-event-mouse-button-down
            allegro-event-mouse-button-up
            allegro-event-mouse-enter-display
            allegro-event-mouse-leave-display
            allegro-event-mouse-warped
            allegro-event-timer
            allegro-event-display-expose
            allegro-event-display-resize
            allegro-event-display-close
            allegro-event-display-lost
            allegro-event-display-found
            allegro-event-display-switch-in
            allegro-event-display-switch-out
            allegro-event-display-orientation))

;; Event type enumeration.
(define allegro-event-joystick-axis           1)
(define allegro-event-joystick-button-down    2)
(define allegro-event-joystick-button-up      3)
(define allegro-event-joystick-configuration  4)

(define allegro-event-key-down                10)
(define allegro-event-key-char                11)
(define allegro-event-key-up                  12)

(define allegro-event-mouse-axes              20)
(define allegro-event-mouse-button-down       21)
(define allegro-event-mouse-button-up         22)
(define allegro-event-mouse-enter-display     23)
(define allegro-event-mouse-leave-display     24)
(define allegro-event-mouse-warped            25)

(define allegro-event-timer                   30)

(define allegro-event-display-expose          40)
(define allegro-event-display-resize          41)
(define allegro-event-display-close           42)
(define allegro-event-display-lost            43)
(define allegro-event-display-found           44)
(define allegro-event-display-switch-in       45)
(define allegro-event-display-switch-out      46)
(define allegro-event-display-orientation     47)

;; Foreign function bindings
(define-foreign %al-create-event-queue
  '* "al_create_event_queue" '())

(define-foreign %al-destroy-event-queue
  void "al_destroy_event_queue" (list '*))

(define-foreign %al-register-event-source
  void "al_register_event_source" (list '* '*))

(define-foreign %al-unregister-event-source
  void "al_unregister_event_source" (list '* '*))

(define-foreign %al-is-event-queue-empty?
  uint8 "al_is_event_queue_empty" (list '*))

(define-foreign %al-get-next-event
  uint8 "al_get_next_event" (list '* '*))

(define-foreign %al-peek-next-event
  uint8 "al_peek_next_event" (list '* '*))

(define-foreign %al-drop-next-event
  uint8 "al_drop_next_event" (list '*))

(define-foreign %al-flush-event-queue
  void "al_flush_event_queue" (list '*))

(define-foreign %al-wait-for-event
  void "al_wait_for_event" (list '* '*))

(define-foreign %al-wait-for-event-timed
  uint8 "al_wait_for_event_timed" (list '* '* float))

(define-foreign %al-wait-for-event-until
  uint8 "al_wait_for_event_until" (list '* '* '*))

(define-foreign %al-init-user-event-source
  void "al_init_user_event_source" (list '*))

(define-foreign %al-destroy-user-event-source
  void "al_destroy_user_event_source" (list '*))

(define-foreign %al-emit-user-event
  uint8 "al_emit_user_event" (list '* '* '*))

(define-foreign %al-unref-user-event
  void "al_unref_user_event" (list '*))

(define-foreign %al-get-event-source-data
  int "al_get_event_source_data" (list '*))

(define-foreign %al-set-event-source-data
  void "al_set_event_source_data" (list '* int))

;; Type lists to convert bytevectors to event data.
(define (build-event-types types)
  (append types-event-any types))

(define types-event-any      (list int '* double))
(define types-event-display  (build-event-types (list int int int int int)))
(define types-event-joystick (build-event-types (list '* int int float int)))
(define types-event-keyboard (build-event-types (list int '*)))
(define types-event-mouse    (build-event-types (list int int int int
                                                      int int int int
                                                      unsigned-int float)))
(define types-event-timer    (build-event-types (list int64 double)))

;; Wrappers
(define-wrapped-pointer-type <allegro-event-queue>
  allegro-event-queue?
  wrap-allegro-event-queue unwrap-allegro-event-queue
  (lambda (q port)
    (format port "#<allegro-event-queue ~x>"
            (pointer-address (unwrap-allegro-event-queue q)))))

(define-wrapped-pointer-type <allegro-event-source>
  allegro-event-source?
  wrap-allegro-event-source unwrap-allegro-event-source
  (lambda (s port)
    (format port "#<allegro-event-source ~x>"
            (pointer-address (unwrap-allegro-event-source s)))))

;; Base event
(define-record-type <allegro-event>
  (%make-allegro-event type source timestamp pointer)
  allegro-event?
  (type al-get-event-type)
  (source %al-get-event-source)
  (timestamp al-get-event-timestamp)
  (pointer al-get-event-pointer))

(define (make-allegro-event pointer)
  (let ((data (parse-c-struct pointer types-event-any)))
    (%make-allegro-event (first data)
                         (second data)
                         (third data)
                         pointer)))

(define (al-get-event-source event)
  (wrap-allegro-event-source (%al-get-event-source event)))

(define (al-get-display-event event)
  (make-allegro-display-event (al-get-event-pointer event)))

(define (al-get-joystick-event event)
  (make-allegro-joystick-event (al-get-event-pointer event)))

(define (al-get-key-event event)
  (make-allegro-key-event (al-get-event-pointer event)))

(define (al-get-mouse-event event)
  (make-allegro-mouse-event (al-get-event-pointer event)))

(define (al-get-timer-event event)
  (make-allegro-timer-event (al-get-event-pointer event)))

(define (%make-event pointer types constructor)
  (let ((data (parse-c-struct pointer types)))
    ;; Leave off event type
    (apply constructor (cdr data))))

;; Display event
(define-record-type <allegro-display-event>
  (%make-allegro-display-event source timestamp x y width height)
  allegro-display-event?
  (source al-get-display-event-src)
  (timestamp al-get-display-event-timestamp)
  (x al-get-display-event-x)
  (y al-get-display-event-y)
  (width al-get-display-event-width)
  (height al-get-display-event-height))

(define (make-allegro-display-event pointer)
  (%make-event pointer types-event-display %make-allegro-display-event))

;; Joystick event
(define-record-type <allegro-joystick-event>
  (%make-allegro-joystick-event source timestamp id stick axis pos button)
  allegro-joystick-event?
  (source al-get-joystick-event-src)
  (timestamp al-get-joystick-event-timestamp)
  (id al-get-joystick-event-id)
  (stick al-get-joystick-event-stick)
  (axis al-get-joystick-event-axis)
  (pos al-get-joystick-event-pos)
  (button al-get-joystick-event-button))

(define (make-allegro-joystick-event pointer)
  (%make-event pointer types-event-joystick %make-allegro-joystick-event))

;; Keyboard event
(define-record-type <allegro-key-event>
  (%make-allegro-key-event source timestamp display keycode unichar
                   modifiers repeat)
  allegro-key-event?
  (source al-get-key-event-source)
  (timestamp al-get-key-event-timestamp)
  (display al-get-key-event-display)
  (keycode al-get-key-event-keycode)
  (unichar al-get-key-event-unichar)
  (modifiers al-get-key-event-modifiers)
  (repeat al-get-key-event-repeat))

(define (make-allegro-key-event pointer)
  (%make-event pointer types-event-keyboard %make-allegro-key-event))

;; Mouse event
(define-record-type <allegro-mouse-event>
  (%make-allegro-mouse-event source timestamp x y z wdx dy dz dw
                             button pressure)
  allegro-mouse-event?
  (source al-get-mouse-event-src)
  (timestamp al-get-mouse-event-timestamp)
  (x al-get-mouse-event-x)
  (y al-get-mouse-event-y)
  (z al-get-mouse-event-z)
  (w al-get-mouse-event-w)
  (dx al-get-mouse-event-dx)
  (dy al-get-mouse-event-dy)
  (dz al-get-mouse-event-dz)
  (dw al-get-mouse-event-dw)
  (button al-get-mouse-event-button)
  (pressure al-get-mouse-event-pressure))

(define (make-allegro-mouse-event pointer)
  (%make-event pointer types-event-mouse %make-allegro-mouse-event))

;; Timer event
(define-record-type <allegro-timer-event>
  (%make-allegro-timer-event source timestamp count error)
  allegro-timer-event?
  (source al-get-timer-event-src)
  (timestamp al-get-timer-event-timestamp)
  (count al-get-timer-event-count)
  (error al-get-timer-event-error))

(define (make-allegro-timer-event pointer)
  (%make-event pointer types-event-timer %make-allegro-timer-event))

;; 72 is the size of the ALLEGRO_EVENT struct.
(define (make-event-bytevector)
  (make-bytevector 72))

(define (al-create-event-queue)
  (wrap-allegro-event-queue (%al-create-event-queue)))

(define (al-destroy-event-queue event-queue)
  (%al-destroy-event-queue (unwrap-allegro-event-queue event-queue)))

(define (al-register-event-source event-queue event-source)
  (%al-register-event-source (unwrap-allegro-event-queue event-queue)
                             (unwrap-allegro-event-source event-source)))

(define (al-unregister-event-source event-queue event-source)
  (%al-unregister-event-source (unwrap-allegro-event-queue event-queue)
                               (unwrap-allegro-event-source event-source)))

(define (al-is-event-queue-empty? event-queue)
  (number->boolean (%al-is-event-queue-empty? (unwrap-allegro-event-queue event-queue))))
(define (al-get-event event-queue proc . args)
  (let* ((event (make-event-bytevector))
         (pointer (bytevector->pointer event)))
    (apply proc (unwrap-allegro-event-queue event-queue) pointer args)
    (make-allegro-event pointer)))

(define (al-get-next-event event-queue)
  (al-get-event event-queue %al-get-next-event))

(define (al-peek-next-event event-queue)
  (al-get-event event-queue %al-peek-next-event))

(define (al-drop-next-event event-queue)
  (number->boolean (%al-drop-next-event (unwrap-allegro-event-queue event-queue))))

(define (al-flush-event-queue event-queue)
  (%al-flush-event-queue (unwrap-allegro-event-queue event-queue)))

(define (al-wait-for-event event-queue)
  (al-get-event event-queue %al-wait-for-event))

(define (al-wait-for-event-timed event-queue timeout)
  (al-get-event event-queue %al-wait-for-event-timed timeout))
