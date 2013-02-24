#! /usr/bin/guile -s
!#

(use-modules (htmlprag)
             (sxml xpath)
             (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 regex)
             (system foreign))

(define-record-type al-func
  (make-al-func name return c-name params)
  al-func?
  (name al-func-name)
  (return al-func-return)
  (c-name al-func-c-name)
  (params al-func-params))

(define (al-func-valid? func)
  (not (string=? (al-func-return func) "unknown")))

(define (format-al-func-params params)
  (define (build-params-list)
    (let ((param-list (reduce (lambda (l p)
                                (string-append l " " p))
                              "" params)))
      (string-append "(list " param-list ")")))

  (if (string=? (car params) "void")
      "'()"
      (build-params-list)))

(define (print-al-func func)
  (format (current-output-port)
          "~A(define-foreign %~A\n  ~A ~s ~A)\n"
          (if (al-func-valid? func) "" "INVALID ")
          (al-func-name func)
          (al-func-return func)
          (al-func-c-name func)
          (format-al-func-params (al-func-params func))))

(define regex "al_[[:alnum:]_]+")

(define types '(("^bool" . "uint8")
                ("^int" . "int")
                ("^float" . "float")
                ("^double" . "double")
                ("^uint32_t" . "uint32")
                ("^const char *" . "'*")
                ("^char const *" . "'*")
                ("^void" . "void")
                ("^ALLEGRO_" . "'*")))

(define (make-procedure-name c-name)
  (regexp-substitute/global #f "_" c-name 'pre "-" 'post))

(define (parse-parameter param)
  (let* ((param (string-trim-both param))
         (type (find (lambda (type)
                       (regexp-match? (string-match (car type) param)))
                     types)))
    (if type
        (cdr type)
        "unknown")))

(define (generate-binding declaration)
  (define (parse-parameters params-string)
    (map parse-parameter (string-split params-string #\,)))

  (let ((match (string-match regex declaration)))
    (when (regexp-match? match)
      (let ((prefix (string-trim-both (match:prefix match)))
            (suffix (string-trim (string-trim-right (match:suffix match) #\)) #\())
            (substring (string-trim-both (match:substring match))))
        (make-al-func (make-procedure-name substring)
                      (parse-parameter prefix)
                      substring
                      (parse-parameters suffix))))))

(call-with-input-file "/home/dave/Code/guile-allegro5/seed/refman/system.html"
  (lambda (port)
    (let* ((declarations ((sxpath '(// pre code)) (html->sxml port)))
          (funcs (map (lambda (func)
                        (generate-binding (cadr func)))
                      declarations)))
      (for-each (lambda (func)
                  (when (al-func? func)
                    (print-al-func func)
                    (newline)))
                funcs))))
