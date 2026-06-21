(import (scheme base) (scheme write) (kaappi json))

(define pass 0)
(define fail 0)

(define (check name expected actual)
  (if (equal? expected actual)
      (begin (set! pass (+ pass 1))
             (display "  PASS: ") (display name) (newline))
      (begin (set! fail (+ fail 1))
             (display "  FAIL: ") (display name) (newline)
             (display "    expected: ") (write expected) (newline)
             (display "    got:      ") (write actual) (newline))))

;; --- Read tests ---

(display "=== json-read-string ===") (newline)

;; Primitives
(check "string" "hello" (json-read-string "\"hello\""))
(check "integer" 42 (json-read-string "42"))
(check "negative" -7 (json-read-string "-7"))
(check "float" #t (and (number? (json-read-string "3.14"))
                        (> (json-read-string "3.14") 3.13)
                        (< (json-read-string "3.14") 3.15)))
(check "true" #t (json-read-string "true"))
(check "false" #f (json-read-string "false"))
(check "null" 'null (json-read-string "null"))

;; Strings with escapes
(check "escape newline" "a\nb" (json-read-string "\"a\\nb\""))
(check "escape tab" "a\tb" (json-read-string "\"a\\tb\""))
(check "escape quote" "a\"b" (json-read-string "\"a\\\"b\""))
(check "escape backslash" "a\\b" (json-read-string "\"a\\\\b\""))
(check "escape slash" "a/b" (json-read-string "\"a\\/b\""))
(check "unicode escape" "A" (json-read-string "\"\\u0041\""))

;; Arrays
(check "empty array" '() (json-read-string "[]"))
(check "int array" '(1 2 3) (json-read-string "[1, 2, 3]"))
(check "string array" '("a" "b") (json-read-string "[\"a\", \"b\"]"))
(check "mixed array" #t
  (let ((v (json-read-string "[1, \"two\", true, null]")))
    (and (= (car v) 1)
         (equal? (cadr v) "two")
         (eq? (caddr v) #t)
         (json-null? (cadddr v)))))
(check "nested array" '((1 2) (3 4)) (json-read-string "[[1,2],[3,4]]"))

;; Objects
(check "empty object" '() (json-read-string "{}"))
(check "simple object"
  '(("name" . "Alice") ("age" . 30))
  (json-read-string "{\"name\": \"Alice\", \"age\": 30}"))
(check "nested object"
  '(("user" . (("id" . 1) ("name" . "Bob"))))
  (json-read-string "{\"user\": {\"id\": 1, \"name\": \"Bob\"}}"))
(check "object with array"
  '(("tags" . ("a" "b" "c")))
  (json-read-string "{\"tags\": [\"a\", \"b\", \"c\"]}"))

;; Whitespace
(check "leading ws" 42 (json-read-string "  42  "))
(check "multiline" '(("a" . 1))
  (json-read-string "{\n  \"a\": 1\n}"))

;; Scientific notation
(check "exponent" #t (number? (json-read-string "1e10")))
(check "neg exponent" #t (number? (json-read-string "1.5e-3")))

;; --- Write tests ---

(display "=== json-write-string ===") (newline)

(check "write string" "\"hello\"" (json-write-string "hello"))
(check "write int" "42" (json-write-string 42))
(check "write true" "true" (json-write-string #t))
(check "write false" "false" (json-write-string #f))
(check "write null" "null" (json-write-string 'null))
(check "write empty array" "[]" (json-write-string '()))
;; Note: '() is empty list → "[]" since it's not a pair-of-pairs (alist)

;; Escape sequences
(check "write escape quote" "\"a\\\"b\"" (json-write-string "a\"b"))
(check "write escape newline" "\"a\\nb\"" (json-write-string (string #\a #\newline #\b)))
(check "write escape backslash" "\"a\\\\b\"" (json-write-string "a\\b"))

;; Arrays
(check "write int array" "[1,2,3]" (json-write-string '(1 2 3)))
(check "write string array" "[\"a\",\"b\"]" (json-write-string '("a" "b")))
(check "write vector" "[1,2,3]" (json-write-string #(1 2 3)))

;; Objects
(check "write object"
  "{\"name\":\"Alice\",\"age\":30}"
  (json-write-string '(("name" . "Alice") ("age" . 30))))
(check "write nested"
  "{\"user\":{\"id\":1}}"
  (json-write-string '(("user" . (("id" . 1))))))
(check "write object with array"
  "{\"tags\":[\"a\",\"b\"]}"
  (json-write-string '(("tags" . ("a" "b")))))
(check "write object with null"
  "{\"value\":null}"
  (json-write-string '(("value" . null))))

;; --- Round-trip tests ---

(display "=== Round-trip ===") (newline)

(define (round-trip json-str)
  (json-write-string (json-read-string json-str)))

(check "rt string" "\"hello\"" (round-trip "\"hello\""))
(check "rt int" "42" (round-trip "42"))
(check "rt object"
  "{\"a\":1,\"b\":\"two\"}"
  (round-trip "{\"a\": 1, \"b\": \"two\"}"))
(check "rt array" "[1,2,3]" (round-trip "[1, 2, 3]"))
(check "rt nested"
  "{\"data\":[{\"id\":1},{\"id\":2}]}"
  (round-trip "{\"data\": [{\"id\": 1}, {\"id\": 2}]}"))
(check "rt booleans" "[true,false,null]" (round-trip "[true, false, null]"))

(newline)
(display "=== Results: ")
(display pass) (display " passed, ")
(display fail) (display " failed ===")
(newline)
(when (> fail 0) (exit 1))
