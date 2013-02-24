(define-module (allegro system)
  #:use-module (system foreign)
  #:use-module (allegro utils)
  #:export (al-install-system
            al-init
            al-uninstall-system
            al-is-system-installed?
            al-get-allegro-version
            al-set-exe-name
            al-set-app-name
            al-set-org-name
            al-get-app-name
            al-get-org-name))

;; Foreign function bindings
(define-foreign %al-install-system
  uint8 "al_install_system" (list int '*))

(define-foreign al-uninstall-system
  void "al_uninstall_system" '())

(define-foreign %al-is-system-installed?
  uint8 "al_is_system_installed" '())

(define-foreign al-get-allegro-version
  uint32 "al_get_allegro_version" '())

(define-foreign %al-get-standard-path
  '* "al_get_standard_path" (list int))

(define-foreign %al-set-exe-name
  void "al_set_exe_name" (list '*))

(define-foreign %al-set-app-name
  void "al_set_app_name" (list '*))

(define-foreign %al-set-org-name
  void "al_set_org_name" (list '*))

(define-foreign %al-get-app-name
  '* "al_get_app_name" '())

(define-foreign %al-get-org-name
  '* "al_get_org_name" '())

(define-foreign %al-get-system-config
  '* "al_get_system_config" '())

(define-foreign %al-register-assert-handler
  void "al_register_assert_handler" (list '*))

;; Wrappers
(define (al-init)
  (number->boolean (%al-install-system (%al-get-allegro-version) %null-pointer)))

(define (al-is-system-installed?)
  (number->boolean (%al-is-system-installed?)))

(define (al-set-exe-name name)
  (%al-set-exe-name (string->pointer name)))

(define (al-set-app-name name)
  (%al-set-app-name (string->pointer name)))

(define (al-set-org-name name)
  (%al-set-org-name (string->pointer name)))

(define (al-get-app-name)
  (pointer->string (%al-get-app-name)))

(define (al-get-org-name)
  (pointer->string (%al-get-org-name)))

(al-define al-install-system
         bool "al_install_system" (list int pointer))
