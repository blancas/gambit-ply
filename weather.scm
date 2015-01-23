;; +-------------------------------------------------------------+
;; | weather - Weather forecast from Google Weather.             |
;; |                                                             |
;; | Usage:                                                      |
;; | $ weather <zip code> | <city>                               |
;; +-------------------------------------------------------------+

(include "plylib/ply-sxml.scm")

(define g-weather "www.google.com/ig/api?weather=")
(define palo-alto "94306")
(define forecast "//xml_api_reply/weather/*[self::forecast_conditions]")

(define (print-info sxml)
  (let ((city (:at sxml 2 2 2 1 1 1 1))
        (date (:at sxml 2 2 2 5 1 1 1)))
    (println city)
    (println "Forecast date: " date)))

(define (print-current sxml)
  (let ((current (:at sxml 2 2 3 1 1 1 1))
        (temp    (:at sxml 2 2 3 2 1 1 1))
        (humid   (:at sxml 2 2 3 4 1 1 1))
        (wind    (:at sxml 2 2 3 6 1 1 1)))
    (println "Current: " current "; Temp: " temp " F; " humid "; " wind)))

(define (print-forecast rec)
  (let ((day  (:at rec 1 1 1 1))
        (low  (:at rec 2 1 1 1))
        (high (:at rec 3 1 1 1))
        (desc (:at rec 5 1 1 1)))
    (println day ", " low ", " high ", " desc)))

(define (main args)
  (let* ((place (if (null? args) palo-alto (:str-join "+" args)))
         (text (:get-url (:str g-weather place)))
         (sxml (:str->sxml text)))
    (print-info sxml)
    (print-current sxml)
    (:dolist (rec (:xpath-get forecast sxml))
      (print-forecast rec))))

(main (cdr (command-line)))
