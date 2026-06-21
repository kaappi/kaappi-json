;;; (kaappi json) — JSON parser and serializer
;;;
;;; Mapping:
;;;   JSON object  → alist  (("key" . value) ...)
;;;   JSON array   → list   (v1 v2 ...)
;;;   JSON string  → string
;;;   JSON number  → number (exact integer or inexact float)
;;;   JSON true    → #t
;;;   JSON false   → #f
;;;   JSON null    → null  (the symbol 'null)

(define-library (kaappi json)
  (import (scheme base) (scheme char) (scheme write))
  (export json-read json-read-string
          json-write json-write-string
          json-null json-null?)
  (begin

    (define json-null 'null)
    (define (json-null? v) (eq? v 'null))

    ;; ---------------------------------------------------------------
    ;; Reader
    ;; ---------------------------------------------------------------

    (define (json-read-string str)
      (let ((port (open-input-string str)))
        (let ((val (json-read port)))
          (close-input-port port)
          val)))

    (define (json-read . args)
      (let ((port (if (pair? args) (car args) (current-input-port))))
        (skip-ws port)
        (read-value port)))

    (define (skip-ws port)
      (let loop ()
        (let ((ch (peek-char port)))
          (when (and (not (eof-object? ch))
                     (or (char=? ch #\space) (char=? ch #\tab)
                         (char=? ch #\newline) (char=? ch #\return)))
            (read-char port)
            (loop)))))

    (define (read-value port)
      (skip-ws port)
      (let ((ch (peek-char port)))
        (cond
          ((eof-object? ch) (error "json-read: unexpected end of input"))
          ((char=? ch #\") (read-json-string port))
          ((char=? ch #\{) (read-object port))
          ((char=? ch #\[) (read-array port))
          ((char=? ch #\t) (read-literal port "true" #t))
          ((char=? ch #\f) (read-literal port "false" #f))
          ((char=? ch #\n) (read-literal port "null" json-null))
          ((or (char=? ch #\-) (char-numeric? ch))
           (read-number port))
          (else (error "json-read: unexpected character" ch)))))

    (define (expect-char port expected)
      (let ((ch (read-char port)))
        (when (or (eof-object? ch) (not (char=? ch expected)))
          (error "json-read: expected" expected "got" ch))))

    ;; --- Strings ---

    (define (read-json-string port)
      (expect-char port #\")
      (let ((out (open-output-string)))
        (let loop ()
          (let ((ch (read-char port)))
            (cond
              ((eof-object? ch) (error "json-read: unterminated string"))
              ((char=? ch #\") (get-output-string out))
              ((char=? ch #\\)
               (let ((esc (read-char port)))
                 (cond
                   ((eof-object? esc) (error "json-read: unterminated escape"))
                   ((char=? esc #\") (write-char #\" out))
                   ((char=? esc #\\) (write-char #\\ out))
                   ((char=? esc #\/) (write-char #\/ out))
                   ((char=? esc #\b) (write-char #\backspace out))
                   ((char=? esc #\f) (write-char #\x000C out))
                   ((char=? esc #\n) (write-char #\newline out))
                   ((char=? esc #\r) (write-char #\return out))
                   ((char=? esc #\t) (write-char #\tab out))
                   ((char=? esc #\u)
                    (let ((cp (read-hex4 port)))
                      (if (and (>= cp #xD800) (<= cp #xDBFF))
                          ;; High surrogate — read low surrogate
                          (begin
                            (expect-char port #\\)
                            (expect-char port #\u)
                            (let ((lo (read-hex4 port)))
                              (let ((full (+ #x10000
                                             (* (- cp #xD800) #x400)
                                             (- lo #xDC00))))
                                (write-char (integer->char full) out))))
                          (write-char (integer->char cp) out))))
                   (else (error "json-read: unknown escape" esc)))
                 (loop)))
              (else (write-char ch out) (loop)))))))

    (define (read-hex4 port)
      (let loop ((i 0) (acc 0))
        (if (= i 4)
            acc
            (let ((ch (read-char port)))
              (cond
                ((eof-object? ch) (error "json-read: incomplete \\u escape"))
                (else (loop (+ i 1) (+ (* acc 16) (hex-val ch)))))))))

    (define (hex-val ch)
      (cond
        ((and (char>=? ch #\0) (char<=? ch #\9))
         (- (char->integer ch) (char->integer #\0)))
        ((and (char>=? ch #\a) (char<=? ch #\f))
         (+ 10 (- (char->integer ch) (char->integer #\a))))
        ((and (char>=? ch #\A) (char<=? ch #\F))
         (+ 10 (- (char->integer ch) (char->integer #\A))))
        (else (error "json-read: invalid hex digit" ch))))

    ;; --- Numbers ---

    (define (read-number port)
      (let ((out (open-output-string))
            (is-float #f))
        (let loop ()
          (let ((ch (peek-char port)))
            (cond
              ((eof-object? ch) #f)
              ((or (char-numeric? ch) (char=? ch #\-) (char=? ch #\+))
               (read-char port) (write-char ch out) (loop))
              ((or (char=? ch #\.) (char=? ch #\e) (char=? ch #\E))
               (set! is-float #t)
               (read-char port) (write-char ch out) (loop))
              (else #f))))
        (let ((s (get-output-string out)))
          (or (string->number s)
              (error "json-read: invalid number" s)))))

    ;; --- Objects ---

    (define (read-object port)
      (expect-char port #\{)
      (skip-ws port)
      (if (and (not (eof-object? (peek-char port)))
               (char=? (peek-char port) #\}))
          (begin (read-char port) '())
          (let loop ((acc '()))
            (skip-ws port)
            (let ((key (read-json-string port)))
              (skip-ws port)
              (expect-char port #\:)
              (let ((val (read-value port)))
                (skip-ws port)
                (let ((next (read-char port)))
                  (cond
                    ((char=? next #\}) (reverse (cons (cons key val) acc)))
                    ((char=? next #\,) (loop (cons (cons key val) acc)))
                    (else (error "json-read: expected , or }" next)))))))))

    ;; --- Arrays ---

    (define (read-array port)
      (expect-char port #\[)
      (skip-ws port)
      (if (and (not (eof-object? (peek-char port)))
               (char=? (peek-char port) #\]))
          (begin (read-char port) '())
          (let loop ((acc '()))
            (let ((val (read-value port)))
              (skip-ws port)
              (let ((next (read-char port)))
                (cond
                  ((char=? next #\]) (reverse (cons val acc)))
                  ((char=? next #\,) (loop (cons val acc)))
                  (else (error "json-read: expected , or ]" next))))))))

    ;; --- Literals ---

    (define (read-literal port expected result)
      (let ((len (string-length expected)))
        (let loop ((i 0))
          (when (< i len)
            (let ((ch (read-char port)))
              (when (or (eof-object? ch)
                        (not (char=? ch (string-ref expected i))))
                (error "json-read: expected" expected))
              (loop (+ i 1))))))
      result)

    ;; ---------------------------------------------------------------
    ;; Writer
    ;; ---------------------------------------------------------------

    (define (json-write-string val)
      (let ((port (open-output-string)))
        (json-write val port)
        (get-output-string port)))

    (define (json-write val . args)
      (let ((port (if (pair? args) (car args) (current-output-port))))
        (write-value val port)))

    (define (write-value val port)
      (cond
        ((string? val) (write-json-str val port))
        ((eq? val #t) (display "true" port))
        ((eq? val #f) (display "false" port))
        ((json-null? val) (display "null" port))
        ((integer? val) (display val port))
        ((number? val) (write-float val port))
        ((vector? val) (write-array (vector->list val) port))
        ((list? val)
         (if (and (pair? val) (pair? (car val)) (string? (caar val)))
             (write-object val port)
             (write-array val port)))
        ((null? val) (display "{}" port))
        (else (error "json-write: unsupported type" val))))

    (define (write-json-str s port)
      (display #\" port)
      (let ((len (string-length s)))
        (let loop ((i 0))
          (when (< i len)
            (let ((ch (string-ref s i)))
              (cond
                ((char=? ch #\") (display "\\\"" port))
                ((char=? ch #\\) (display "\\\\" port))
                ((char=? ch #\newline) (display "\\n" port))
                ((char=? ch #\return) (display "\\r" port))
                ((char=? ch #\tab) (display "\\t" port))
                ((char=? ch #\backspace) (display "\\b" port))
                ((char=? ch #\x000C) (display "\\f" port))
                ((char<? ch #\space) (write-unicode-escape ch port))
                (else (write-char ch port))))
            (loop (+ i 1)))))
      (display #\" port))

    (define (write-unicode-escape ch port)
      (display "\\u" port)
      (let ((cp (char->integer ch)))
        (display (hex-char (quotient cp 4096)) port)
        (display (hex-char (remainder (quotient cp 256) 16)) port)
        (display (hex-char (remainder (quotient cp 16) 16)) port)
        (display (hex-char (remainder cp 16)) port)))

    (define (hex-char n)
      (integer->char (+ (if (< n 10) (char->integer #\0) (- (char->integer #\a) 10)) n)))

    (define (write-float val port)
      (let ((s (number->string val)))
        (display s port)
        (when (not (let loop ((i 0))
                     (cond ((= i (string-length s)) #f)
                           ((or (char=? (string-ref s i) #\.)
                                (char=? (string-ref s i) #\e)
                                (char=? (string-ref s i) #\E)) #t)
                           (else (loop (+ i 1))))))
          (display ".0" port))))

    (define (write-object alist port)
      (display #\{ port)
      (let loop ((pairs alist) (first #t))
        (when (pair? pairs)
          (unless first (display #\, port))
          (write-json-str (car (car pairs)) port)
          (display #\: port)
          (write-value (cdr (car pairs)) port)
          (loop (cdr pairs) #f)))
      (display #\} port))

    (define (write-array lst port)
      (display #\[ port)
      (let loop ((items lst) (first #t))
        (when (pair? items)
          (unless first (display #\, port))
          (write-value (car items) port)
          (loop (cdr items) #f)))
      (display #\] port))))
