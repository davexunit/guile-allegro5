Guile Bindings for Allegro 5
============================

Allegro 5 is a cross-platform, open source, game programming library
written in C. Guile-allegro5 is a collection of modules that provides
Allegro 5 bindings for Guile.

Example
-------
```scheme
(use-modules (allegro system)
             (allegro display)
             (allegro events)
             (allegro keyboard)
             (allegro timer)
             (allegro graphics)
             (allegro addons image))

(define timer #f)
(define events #f)
(define running #t)
(define redraw #t)
(define image #f)
(define window #f)

(al-init)
(al-init-image-addon)
(al-install-keyboard)

(define (game-loop)
  (set! window (al-create-display 640 480))
  (set! timer (al-create-timer (/ 1 60)))
  (set! events (al-create-event-queue))
  (set! image (al-load-bitmap "player.png"))
  (al-register-event-source events (al-get-timer-event-source timer))
  (al-register-event-source events (al-get-keyboard-event-source))

  (set! running #t)
  (set! redraw #t)
  (al-start-timer timer)
  (while running
    (let ((event (al-wait-for-event events)))
      (cond ((= (al-get-event-type event) allegro-event-key-up)
             (set! running #f))
            ((= (al-get-event-type event) allegro-event-timer)
             (set! redraw #t)))
      (when (and redraw (al-is-event-queue-empty? events))
        (set! redraw #f)
        (al-clear-to-color 0 0 0)
        (al-draw-bitmap image 100 100 0)
        (al-flip-display))))
  (al-destroy-bitmap image)
  (al-destroy-event-queue events)
  (al-destroy-timer timer)
  (al-destroy-display window))

(game-loop)
```

Hacking
-------
The bindings are incomplete. Please help fill in the gaps.

License
-------
BSD 2-Clause License
