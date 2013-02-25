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
             (allegro graphics)
             (allegro addons image))

(define *display* #f)
(define *image* #f)

(al-init)
(al-init-image-addon)
(set! *display* (al-create-display 640 480))
(set! *image* (al-load-bitmap "foo.png"))
(al-clear-to-color .5 .5 .5)
(al-draw-bitmap *image* 100 100 0)
(al-flip-display)
```

This example isn't so useful as there is no game loop, but it works for now.

Hacking
-------
The bindings are incomplete. Please help fill in the gaps.

License
-------
BSD 2-Clause License
