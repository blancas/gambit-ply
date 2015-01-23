;; +---------------------------------------------------------------------+
;; |  GAMBIT PLY                                                         |
;; |  A high-productivity function library for Gambit-C.                 |
;; |                                                                     |
;; |  Copyright 2012 Armando Blancas                                     |
;; +---------------------------------------------------------------------+


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                STRINGS AND REGULAR EXPRESSIONS                      |
;; |                                                                     |
;; +---------------------------------------------------------------------+

(include "plylib.scm")
(include "irregex.scm")


;; Tests if c is a regular-expression meta-character.
(define :re-meta?
  (let ((re-chars (string->list ".\\+*?^[]$(){}=!<>|:-")))
    (lambda (c)
      (:member? c re-chars))))


;; Escapes a regular expression to be used as a literal string.
(define (:escape-re re)
  (letrec ((E (lambda (lst)
                (cond ((null? lst) (quote ()))
                      ((:re-meta? (car lst)) (cons #\\ (cons (car lst) (E (cdr lst)))))
                      (else (cons (car lst) (E (cdr lst))))))))
    (list->string (E (string->list re)))))


;; Returns obj as a string of the given width, right justified.
(define (:strw obj width)
  (letrec ((W (lambda (s)
                (if (< (string-length s) width)
                  (W (string-append " " s))
                  s))))
    (W (object->string obj))))


;; Strips leading whitespace.
(define :ltrim
  (let ((lead (string->irregex "^[ \t\r\n]*")))
    (lambda (s)
      (irregex-replace lead s ""))))


;; Strips trailing whitespace.
(define :rtrim
  (let ((trail (string->irregex "[ \t\r\n]*$")))
    (lambda (s)
      (irregex-replace trail s ""))))


;; Strips leading and trailing whitespace.
(define (:trim s)
  (:ltrim (:rtrim s)))


;; Tests if the string str starts with the substring sub.
(define (:starts-with sub str)
  (let ((s (string-append "^" (:escape-re sub))))
    (if (irregex-search s str) #t #f)))


;; Tests if the string str ends with the substring sub.
(at-expand-time
(define (:ends-with sub str)
  (let ((s (string-append (:escape-re sub) "$")))
    (if (irregex-search s str) #t #f)))
)

;; Tests if the string matches a regular expression.
(at-expand-time
(define (:match re str)
  (irregex-match re str))
)

;; Searches a regular expression in a string.
(define (:search re str)
  (let ((m (irregex-search re str)))
    (and m (irregex-match-start-index m))))

;; Searches a substring in a string.
(define (:search-str sub str)
  (let ((m (irregex-search (:escape-re sub) str)))
    (and m (irregex-match-start-index m))))


;; Extracts all matches of a regular expression in a string.
(define (:extract re str)
  (irregex-extract re str))


;; Replaces a match of a regular expression.
(define (:replace re new str)
  (irregex-replace re str new))


;; Replaces a substring in a string.
(define (:replace-str old new str)
  (irregex-replace (:escape-re old) str new))


;; Replaces all matches of a regular expression.
(define (:replace-all re txt str)
  (irregex-replace/all re str txt))


;; Replaces all matches of a substring.
(define (:replace-allstr old new str)
  (irregex-replace/all (:escape-re old) str new))


;; Breaks a string into parts with a regular expression.
(define (:split re str)
  (irregex-split re str))


;; Breaks a string into parts with a substring.
(define (:split-str str . args)
  (cond ((null? args) 
         (irregex-split " " str))
        ((null? (cdr args)) 
         (irregex-split (:escape-re (car args)) str))
        (else 
          (let* ((sub (car args))
                 (cnt (cadr args))
                 (lst (irregex-split (:escape-re sub) str)))
            (:snoc (:take cnt lst) (:str-join sub (:drop cnt lst)))))))


;; Breaks a string into lines of text.
(define :lines
  (let ((nl (string->irregex "\r?\n")))
    (lambda (s)
      (irregex-split nl s))))


;; Collapses lines of text.
(define (:unlines lst)
  (:foldr1 (lambda (x y) (string-append  x "\n" y)) lst))


;; Breaks a string into words.
(define :words
  (let ((ws (string->irregex "\\s")))
    (lambda (s)
      (irregex-split ws s))))


;; Collapses words into a string.
(define (:unwords lst)
  (:foldr1 (lambda (x y) (string-append  x " " y)) lst))


;; Converts a wildcard expression into a regular expression.
(define (:wild->regex wild)
  (let ((re (->> wild 
                 (:replace-allstr "-" "\\-")
                 (:replace-allstr "." "\\.")
                 (:replace-allstr "^" "\\^")
                 (:replace-allstr "$" "\\$")
                 (:replace-allstr "(" "\\(")
                 (:replace-allstr ")" "\\)")
                 (:replace-allstr "{" "\\{")
                 (:replace-allstr "}" "\\}")
                 (:replace-allstr "*" ".*")
                 (:replace-allstr "?" "."))))
    (:str "^" re "$")))
