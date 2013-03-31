(define-module (allegro timer)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (allegro events)
  #:use-module (allegro utils)
  #:export (al-create-timer
            al-start-timer
            al-stop-timer
            al-get-timer-started
            al-destroy-timer
            al-get-timer-count
            al-set-timer-count
            al-add-timer-count
            al-set-timer-count
            al-get-timer-speed
            al-set-timer-speed
            al-get-timer-event-source))

;; Foreign function bindings.
(define-foreign %al-create-timer
  '* "al_create_timer" (list double))

(define-foreign %al-start-timer
  void "al_start_timer" (list '*))

(define-foreign %al-stop-timer
  void "al_stop_timer" (list '*))

(define-foreign %al-get-timer-started
  uint8 "al_get_timer_started" (list '*))

(define-foreign %al-destroy-timer
  void "al_destroy_timer" (list '*))

(define-foreign %al-get-timer-count
  int "al_get_timer_count" (list '*))

(define-foreign %al-set-timer-count
  void "al_set_timer_count" (list '* int))

(define-foreign %al-add-timer-count
  void "al_add_timer_count" (list '* int))

(define-foreign %al-get-timer-speed
  double "al_get_timer_speed" (list '*))

(define-foreign %al-set-timer-speed
  void "al_set_timer_speed" (list '* double))

(define-foreign %al-get-timer-event-source
  '* "al_get_timer_event_source" (list '*))

;; Wrappers
(define-wrapped-pointer-type <allegro-timer>
  allegro-timer?
  wrap-allegro-timer unwrap-allegro-timer
  (lambda (t port)
    (format port "#<allegro-timer ~x>"
            (pointer-address (unwrap-allegro-timer t)))))

(define (al-create-timer speed)
  (wrap-allegro-timer (%al-create-timer speed)))

(define (al-destroy-timer timer)
  (%al-destroy-timer (unwrap-allegro-timer timer)))

(define (al-start-timer timer)
  (%al-start-timer (unwrap-allegro-timer timer)))

(define (al-stop-timer timer)
  (%al-stop-timer (unwrap-allegro-timer timer)))

(define (al-get-timer-started timer)
  (number->boolean (%al-get-timer-started (unwrap-allegro-timer timer))))

(define (al-get-timer-count timer)
  (%al-get-timer-count (unwrap-allegro-timer timer)))

(define (al-set-timer-count timer count)
  (%al-set-timer-count (unwrap-allegro-timer timer) count))

(define (al-add-timer-count timer count)
  (%al-add-timer-count (unwrap-allegro-timer timer) count))

(define (al-get-timer-speed timer)
  (%al-get-timer-speed (unwrap-allegro-timer timer)))

(define (al-set-timer-speed timer speed)
  (%al-set-timer-speed (unwrap-allegro-timer timer) speed))

(define (al-get-timer-event-source timer)
  (wrap-allegro-event-source
   (%al-get-timer-event-source (unwrap-allegro-timer timer))))
