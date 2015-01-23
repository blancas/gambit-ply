;; +---------------------------------------------------------------------+
;; |  GAMBIT PLY                                                         |
;; |  A high-productivity function library for Gambit-C.                 |
;; |                                                                     |
;; |  Copyright 2012 Armando Blancas                                     |
;; +---------------------------------------------------------------------+


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                          SSAX-SXML                                  |
;; |                                                                     |
;; +---------------------------------------------------------------------+

(include "ply-io.scm")

(include "ssax-sxml/libs/input-parse.scm")
(include "ssax-sxml/libs/look-for-str.scm")
(include "ssax-sxml/libs/srfi-13-local.scm")
(include "ssax-sxml/libs/util.scm")
(include "ssax-sxml/libs/gambit/common.scm")
(include "ssax-sxml/libs/gambit/myenv.scm")
(include "ssax-sxml/libs/gambit/parse-error.scm")
(include "ssax-sxml/html-prag/htmlprag.scm")
(include "ssax-sxml/ssax/char-encoding.scm")
(include "ssax-sxml/ssax/ssax-code.scm")
(include "ssax-sxml/multi-parser/id/access-remote.scm")
(include "ssax-sxml/multi-parser/id/http.scm")
(include "ssax-sxml/multi-parser/id/mime.scm")
(include "ssax-sxml/multi-parser/id/srfi-12.scm")
(include "ssax-sxml/sxml-tools/serializer.scm")
(include "ssax-sxml/sxml-tools/sxml-tools.scm")
(include "ssax-sxml/sxml-tools/sxpath.scm")
(include "ssax-sxml/sxml-tools/sxpathlib.scm")
(include "ssax-sxml/sxml-tools/sxpath-ext.scm")
(include "ssax-sxml/sxml-tools/xpath-parser.scm")
(include "ssax-sxml/sxml-tools/txpath.scm")


;; Overrides that allow best-effor parsing.
;; (define (parser-error . args) (cdr args)) -> ssax-sxml/sxml-tools/xpath-parser.scm
;; (define (cerr . args) #f)                 -> ssax-sxml/libs/gambit/myenv.scm


;; Parses an XML file into SXML.
(define (:file->sxml file)
  (with-exception-catcher
    (lambda (e) (quote ()))
    (lambda () (ssax:xml->sxml (open-input-file file) '()))))


;; Parses a string into SXML.
(define (:str->sxml str)
  (call-with-input-string str
    (lambda (port)
      (ssax:xml->sxml port '()))))


;; Apply an XPATH on an SXML tree.
(define (:xpath-get path sxml)
  (let ((xpath (sxpath path)))
    (if (procedure? xpath)
      (xpath sxml)
      (quote ()))))
