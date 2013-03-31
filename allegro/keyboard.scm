(define-module (allegro keyboard)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (allegro utils)
  #:use-module (allegro events)
  #:export (al-install-keyboard
            al-is-keyboard-installed?
            al-uninstall-keyboard
            al-get-keyboard-state
            al-key-down?
            al-keycode-to-name
            al-set-keyboard-leds
            al-get-keyboard-event-source
            allegro-key-a
            allegro-key-b
            allegro-key-c
            allegro-key-d
            allegro-key-e
            allegro-key-f
            allegro-key-g
            allegro-key-h
            allegro-key-i
            allegro-key-j
            allegro-key-k
            allegro-key-l
            allegro-key-m
            allegro-key-n
            allegro-key-o
            allegro-key-p
            allegro-key-q
            allegro-key-r
            allegro-key-s
            allegro-key-t
            allegro-key-u
            allegro-key-v
            allegro-key-w
            allegro-key-x
            allegro-key-y
            allegro-key-z
            allegro-key-0
            allegro-key-1
            allegro-key-2
            allegro-key-3
            allegro-key-4
            allegro-key-5
            allegro-key-6
            allegro-key-7
            allegro-key-8
            allegro-key-9
            allegro-key-pad-0
            allegro-key-pad-1
            allegro-key-pad-2
            allegro-key-pad-3
            allegro-key-pad-4
            allegro-key-pad-5
            allegro-key-pad-6
            allegro-key-pad-7
            allegro-key-pad-8
            allegro-key-pad-9
            allegro-key-f1
            allegro-key-f2
            allegro-key-f3
            allegro-key-f4
            allegro-key-f5
            allegro-key-f6
            allegro-key-f7
            allegro-key-f8
            allegro-key-f9
            allegro-key-f10
            allegro-key-f11
            allegro-key-f12
            allegro-key-escape
            allegro-key-tilde
            allegro-key-minus
            allegro-key-equals
            allegro-key-backspace
            allegro-key-tab
            allegro-key-openbrace
            allegro-key-closebrace
            allegro-key-enter
            allegro-key-semicolon
            allegro-key-quote
            allegro-key-backslash
            allegro-key-backslash2
            allegro-key-comma
            allegro-key-fullstop
            allegro-key-slash
            allegro-key-space
            allegro-key-insert
            allegro-key-delete
            allegro-key-home
            allegro-key-end
            allegro-key-pgup
            allegro-key-pgdn
            allegro-key-left
            allegro-key-right
            allegro-key-up
            allegro-key-down
            allegro-key-pad-slash
            allegro-key-pad-asterisk
            allegro-key-pad-minus
            allegro-key-pad-plus
            allegro-key-pad-delete
            allegro-key-pad-enter
            allegro-key-printscreen
            allegro-key-pause
            allegro-key-abnt-c1
            allegro-key-yen
            allegro-key-kana
            allegro-key-convert
            allegro-key-noconvert
            allegro-key-at
            allegro-key-circumflex
            allegro-key-colon2
            allegro-key-kanji
            allegro-key-pad-equals
            allegro-key-backquote
            allegro-key-semicolon2
            allegro-key-command
            allegro-key-unknown
            allegro-key-modifiers
            allegro-key-lshift
            allegro-key-rshift
            allegro-key-lctrl
            allegro-key-rctrl
            allegro-key-alt
            allegro-key-altgr
            allegro-key-lwin
            allegro-key-rwin
            allegro-key-menu
            allegro-key-scrolllock
            allegro-key-numlock
            allegro-key-capslock
            allegro-keymod-shift
            allegro-keymod-ctrl
            allegro-keymod-alt
            allegro-keymod-lwin
            allegro-keymod-rwin
            allegro-keymod-menu
            allegro-keymod-altgr
            allegro-keymod-command
            allegro-keymod-scrolllock
            allegro-keymod-numlock
            allegro-keymod-capslock
            allegro-keymod-inaltseq
            allegro-keymod-accent1
            allegro-keymod-accent2
            allegro-keymod-accent3
            allegro-keymod-accent4))

;; Keycodes
(define allegro-key-a            1)
(define allegro-key-b            2)
(define allegro-key-c            3)
(define allegro-key-d            4)
(define allegro-key-e            5)
(define allegro-key-f            6)
(define allegro-key-g            7)
(define allegro-key-h            8)
(define allegro-key-i            9)
(define allegro-key-j            10)
(define allegro-key-k            11)
(define allegro-key-l            12)
(define allegro-key-m            13)
(define allegro-key-n            14)
(define allegro-key-o            15)
(define allegro-key-p            16)
(define allegro-key-q            17)
(define allegro-key-r            18)
(define allegro-key-s            19)
(define allegro-key-t            20)
(define allegro-key-u            21)
(define allegro-key-v            22)
(define allegro-key-w            23)
(define allegro-key-x            24)
(define allegro-key-y            25)
(define allegro-key-z            26)
(define allegro-key-0            27)
(define allegro-key-1            28)
(define allegro-key-2            29)
(define allegro-key-3            30)
(define allegro-key-4            31)
(define allegro-key-5            32)
(define allegro-key-6            33)
(define allegro-key-7            34)
(define allegro-key-8            35)
(define allegro-key-9            36)
(define allegro-key-pad-0        37)
(define allegro-key-pad-1        38)
(define allegro-key-pad-2        39)
(define allegro-key-pad-3        40)
(define allegro-key-pad-4        41)
(define allegro-key-pad-5        42)
(define allegro-key-pad-6        43)
(define allegro-key-pad-7        44)
(define allegro-key-pad-8        45)
(define allegro-key-pad-9        46)
(define allegro-key-f1           47)
(define allegro-key-f2           48)
(define allegro-key-f3           49)
(define allegro-key-f4           50)
(define allegro-key-f5           51)
(define allegro-key-f6           52)
(define allegro-key-f7           53)
(define allegro-key-f8           54)
(define allegro-key-f9           55)
(define allegro-key-f10          56)
(define allegro-key-f11          57)
(define allegro-key-f12          58)
(define allegro-key-escape       59)
(define allegro-key-tilde        60)
(define allegro-key-minus        61)
(define allegro-key-equals       62)
(define allegro-key-backspace    63)
(define allegro-key-tab          64)
(define allegro-key-openbrace    65)
(define allegro-key-closebrace   66)
(define allegro-key-enter        67)
(define allegro-key-semicolon    68)
(define allegro-key-quote        69)
(define allegro-key-backslash    70)
(define allegro-key-backslash2   71)
(define allegro-key-comma        72)
(define allegro-key-fullstop     73)
(define allegro-key-slash        74)
(define allegro-key-space        75)
(define allegro-key-insert       76)
(define allegro-key-delete       77)
(define allegro-key-home         78)
(define allegro-key-end          79)
(define allegro-key-pgup         80)
(define allegro-key-pgdn         81)
(define allegro-key-left         82)
(define allegro-key-right        83)
(define allegro-key-up           84)
(define allegro-key-down         85)
(define allegro-key-pad-slash    86)
(define allegro-key-pad-asterisk 87)
(define allegro-key-pad-minus    88)
(define allegro-key-pad-plus     89)
(define allegro-key-pad-delete   90)
(define allegro-key-pad-enter    91)
(define allegro-key-printscreen  92)
(define allegro-key-pause        93)
(define allegro-key-abnt-c1      94)
(define allegro-key-yen          95)
(define allegro-key-kana         96)
(define allegro-key-convert      97)
(define allegro-key-noconvert    98)
(define allegro-key-at           99)
(define allegro-key-circumflex   100)
(define allegro-key-colon2       101)
(define allegro-key-kanji        102)
(define allegro-key-pad-equals   103)
(define allegro-key-backquote    104)
(define allegro-key-semicolon2   105)
(define allegro-key-command      106)
(define allegro-key-unknown      107)
(define allegro-key-modifiers    215)
(define allegro-key-lshift       215)
(define allegro-key-rshift       216)
(define allegro-key-lctrl        217)
(define allegro-key-rctrl        218)
(define allegro-key-alt          219)
(define allegro-key-altgr        220)
(define allegro-key-lwin         221)
(define allegro-key-rwin         222)
(define allegro-key-menu         223)
(define allegro-key-scrolllock   224)
(define allegro-key-numlock      225)
(define allegro-key-capslock     226)
(define allegro-key-max          227)

;; Key modifiers
(define allegro-keymod-shift      #x00001)
(define allegro-keymod-ctrl       #x00002)
(define allegro-keymod-alt        #x00004)
(define allegro-keymod-lwin       #x00008)
(define allegro-keymod-rwin       #x00010)
(define allegro-keymod-menu       #x00020)
(define allegro-keymod-altgr      #x00040)
(define allegro-keymod-command    #x00080)
(define allegro-keymod-scrolllock #x00100)
(define allegro-keymod-numlock    #x00200)
(define allegro-keymod-capslock   #x00400)
(define allegro-keymod-inaltseq   #x00800)
(define allegro-keymod-accent1    #x01000)
(define allegro-keymod-accent2    #x02000)
(define allegro-keymod-accent3    #x04000)
(define allegro-keymod-accent4    #x08000)

;; Keyboard state
(define-wrapped-pointer-type <allegro-keyboard-state>
  allegro-keyboard-state?
  wrap-allegro-keyboard-state unwrap-allegro-keyboard-state
  (lambda (ks port)
    (format port "#<allegro-keyboard-state ~x>"
            (pointer-address (unwrap-allegro-keyboard-state ks)))))

;; This does the equivalent of what happens in the C source for
;; determining how large the internal key state array should be.
(define key-state-internal-length
  (inexact->exact (round (/ (+ allegro-key-max 31) 32))))

(define types-keyboard-state (append (list '*)
                                     (make-list key-state-internal-length
                                                unsigned-int)))

;; Foreign function bindings
(define-foreign %al-install-keyboard
  uint8 "al_install_keyboard" '())

(define-foreign %al-is-keyboard-installed?
  uint8 "al_is_keyboard_installed" '())

(define-foreign al-uninstall-keyboard
  void "al_uninstall_keyboard" '())

(define-foreign %al-get-keyboard-state
  void "al_get_keyboard_state" (list '*))

(define-foreign %al-key-down?
  uint8 "al_key_down" (list '* int))

(define-foreign %al-keycode-to-name
  '* "al_keycode_to_name" (list int))

(define-foreign %al-set-keyboard-leds
  uint8 "al_set_keyboard_leds" (list int))

(define-foreign %al-get-keyboard-event-source
  '* "al_get_keyboard_event_source" '())

(define (al-install-keyboard)
  (number->boolean (%al-install-keyboard)))

(define (al-is-keyboard-installed?)
  (number->boolean (%al-is-keyboard-installed?)))

(define (al-get-keyboard-state)
  (let* ((ks (make-bytevector (sizeof types-keyboard-state)))
         (pointer (bytevector->pointer ks)))
    (%al-get-keyboard-state pointer)
    (wrap-allegro-keyboard-state pointer)))

(define (al-key-down? keyboard-state keycode)
  (number->boolean
   (%al-key-down? (unwrap-allegro-keyboard-state keyboard-state)
                  keycode)))

(define (al-keycode-to-name keycode)
  (pointer->string (%al-keycode-to-name keycode)))

(define (al-set-keyboard-leds leds)
  (number->boolean (%al-set-keyboard-leds leds)))

(define (al-get-keyboard-event-source)
  (wrap-allegro-event-source (%al-get-keyboard-event-source)))
