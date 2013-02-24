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
            al-wait-for-event-until
            al-init-user-event-source
            al-destroy-user-event-source
            al-emit-user-event
            al-unref-user-event
            al-get-event-source-data
            al-set-event-source-data
            wrap-allegro-event-source
            allegro-event?
            allegro-event-type
            allegro-event-event-source
            allegro-event-timestamp
            allegro-event-build))

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

;; Event record
(define-record-type <allegro-event>
  (make-allegro-event type source timestamp pointer)
  allegro-event?
  (type al-get-event-type)
  (source %al-get-event-source)
  (timestamp al-get-event-timestamp)
  (pointer al-get-event-pointer))

(define (al-get-event-event-source event)
  (wrap-allegro-event-source (%al-get-event-source event)))

;; (define (al-build-event event)
;;   (let ((constructor (hash-ref event-constructors
;;                                (allegro-event-type event))))
;;     (apply constructor (allegro-event-data event))))

;; Type lists to convert bytevectors to event data.
(define (build-event-types types)
  (append types-event-any types))

(define types-event-any      (list int '* double))
(define types-event-display  (build-event-types (list int int int int int)))
(define types-event-keyboard (build-event-types (list int '*)))
(define types-event-mouse    (build-event-types (list int int int int
                                                      int int int int
                                                      unsigned-int float)))
(define types-event-timer    (build-event-types (list int64 double)))

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

;; Register event types with their associated constuctor procedures.
(define event-constructors (make-hash-table))

(define (register-event-constuctor type constructor)
  (hash-set! event-constructors type constructor))

;; Register event constructors for the built in event types.
;; (register-event-constuctor allegro-event-key-down
;;                            make-allegro-key-event)
;; (register-event-constuctor allegro-event-key-char
;;                            make-allegro-key-event)
;; (register-event-constuctor allegro-event-key-up
;;                            make-allegro-key-event)

;; (register-event-constuctor allegro-event-mouse-axes
;;                            make-allegro-mouse-event)
;; (register-event-constuctor allegro-event-mouse-button-down
;;                            make-allegro-mouse-event)
;; (register-event-constuctor allegro-event-mouse-button-up
;;                            make-allegro-mouse-event)
;; (register-event-constuctor allegro-event-mouse-enter-display
;;                            make-allegro-mouse-event)
;; (register-event-constuctor allegro-event-mouse-leave-display
;;                            make-allegro-mouse-event)
;; (register-event-constuctor allegro-event-mouse-warped
;;                            make-allegro-mouse-event)

;; (register-event-constuctor allegro-event-timer
;;                            make-al-timer-event)

;; 72 is the size of the ALLEGRO_EVENT struct.
(define (make-event-bytevector)
  (make-bytevector 72))

;; (define (make-event pointer)
;;   (let* ((event (parse-c-struct pointer types-event-any))
;;          (type (car event))
;;          (constructor (hash-ref event-constructors type)))
;;     (apply constructor event)))

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

(define (al-get-next-event event-queue)
  (let* ((event (make-event-bytevector))
         (pointer (bytevector->pointer event)))
    (%al-get-next-event (unwrap-allegro-event-queue event-queue) pointer)
    (let ((data (parse-c-struct pointer types-event-any)))
      (make-allegro-event (first data)
                          (second data)
                          (third data)
                          pointer))))
