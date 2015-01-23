;; +---------------------------------------------------------------------+
;; |  GAMBIT PLY                                                         |
;; |  A high-productivity function library for Gambit-C.                 |
;; |                                                                     |
;; |  Copyright 2012 Armando Blancas                                     |
;; +---------------------------------------------------------------------+


;; +---------------------------------------------------------------------+
;; |                                                                     |
;; |                              I/O                                    |
;; |                                                                     |
;; +---------------------------------------------------------------------+

(include "ply-string.scm")


;; Calls proc for each char read from stdin.
(define (:call-with-stdin proc)
  (let loop ((c (read-char)))
    (:when (not (eqv? c #!eof))
      (proc c)
      (loop (read-char)))))


;; Reads the file fname into a string.
(define (:slurp fname)
  (call-with-input-file fname
    (lambda (port)
      (let ((text (read-line port #f)))
        (if (equal? text #!eof) "" text)))))


;; Writes a string into the file fname.
(define (:spit fname str)
  (call-with-output-file fname
    (lambda (port) 
      (write-substring str 0 (string-length str) port))))


;; Reads all the text from an input port.
(define (:slurp-port port)
  (let ((text (read-line port #f)))
    (close-port port)
    text))


;; Writes a string into an output port.
(define (:spit-port port str)
  (display str port)
  (force-output port))


;; Skips the headers and reads the body of an HTTP response as text.
(define (:slurp-http port)
  (let loop ((line (read-line port)))
    (:when (> (:count line) 1)
      (loop (read-line port))))
  (let ((text (read-line port #f)))
    (close-input-port port)
    text))


;; Returns the headers of an HTTP response in a list.
(define (:get-headers port)
  (let ((line (:rtrim (read-line port))))
    (if (:empty? line)
      (quote ())
      (cons line (:get-headers port)))))


;; Scans HTTP headers for the content type.
(define (:content-type head)
  (cond ((null? head) 
         "text/plain")
        ((:starts-with "Content-Type:" (car head)) 
         (car (:extract "\\w*/\\w*" (car head))))
        (else 
         (:content-type (cdr head)))))


;; Scans HTTP headers for the content length.
(define (:content-length head)
  (cond ((null? head) 
         "")
        ((:starts-with "Content-Length:" (car head)) 
         (car (:extract "\\d*" (car head))))
        (else 
         (:content-length (cdr head)))))


;; Transfers total bytes from ports in to out.
(define (:transfer total in out . arg)
  (let* ((block (if (null? arg) 4096 (car arg)))
         (vect (make-u8vector block)))
    (let loop ((count 0)
               (bytes (read-subu8vector vect 0 block in)))
      (:when (and (< count total) (positive? bytes))
        (write-subu8vector vect 0 bytes out)
        (loop (+ count bytes)
              (read-subu8vector vect 0 block in))))))


;; Writes the body of an HTTP response into file fname.
(define (:spit-http fname port)
  (let ((resp (:rtrim (read-line port)))
        (head (:get-headers port)))
    (cond ((not (:ends-with "200 OK" resp))
           (:spit fname (:str resp (:nl) head (:slurp-port port))) #f)
          ((:starts-with "text" (:content-type head))
           (:spit fname (:slurp-port port)) #t)
          (else
           (let ((len (or (string->number (:content-length head)) 0)))
             (call-with-output-file fname
               (lambda (file) (:transfer len port file)))) #t))))


;; Opens a TCP I/O port on a URL.
(define (:open-url url . opt)
  (define (open-port srv)
    (if (:search ":[0-9]+" url)
      (open-tcp-client srv)
      (let ((port (if (null? opt) 80 (car opt))))
        (open-tcp-client (list server-address: srv port-number: port buffering: #f)))))
  (define (get-cmd parts)
    (:str "GET /" (cadr parts) " HTTP/1.1\r\n"
          "Host: " (car parts) "\r\n"
          "Connection: close\r\n\r\n"))
  (let* ((parts (:split-str (:replace "https?://" "" url) "/" 1))
         (srv (car parts))
         (cmd (get-cmd parts))
         (tcp (open-port srv)))
    (:spit-port tcp cmd)
    tcp))

  
;; Returns the text of a URL.
(define (:get-url url . opt)
  (let ((tcp (apply :open-url url opt)))
    (:slurp-http tcp)))


;; Writes a URL resource into file fname.
(define (:spit-url fname url . opt)
  (let ((tcp (apply :open-url url opt)))
    (:spit-http fname tcp)))


;; Tests if fname is a directory.
(at-expand-time
(define (:directory? fname)
  (and (file-exists? fname) 
       (eqv? (file-type fname) 'directory)))
)

;; Tests if fname is a regular file.
(define (:file? fname)
  (and (file-exists? fname) 
       (eqv? (file-type fname) 'regular)))


;; Tests if a file is writable.
(define :writable?
  (let ((mask (string->number "200" 8)))
    (lambda (fname)
      (not (zero? (bitwise-and (file-mode fname) mask))))))


;; Tests if a file is read-only.
(define :read-only?
  (let ((mask (string->number "600" 8))
        (ro (string->number "400" 8)))
    (lambda (fname)
      (= (bitwise-and (file-mode fname) mask) ro))))


;; Evaluates body for each file in a directory.
(define-macro (:dodir bind . body)
  (let ((file (car bind))
        (path (cadr bind))
        (root (gensym))  ; root dir
        (psym (gensym))  ; directory port
        (ssym (gensym))  ; string from the port
        (loop (gensym))) ; name of loop
    `(let* ((,root (path-strip-trailing-directory-separator ,path))
           (,psym (open-directory ,root)))
       (let ,loop ((,ssym (read ,psym)))
         (:when (string? ,ssym)
           (let ((,file (:str ,root "/" ,ssym)))
             ,@body
             (,loop (read ,psym)))))
       (close-input-port ,psym))))

;; Evaluates body for each file in a filespec.
(define-macro (:dofiles bind . body)
  (let ((file (car bind))
        (path (cadr bind))
        (dsym (gensym))  ; directory part
        (fsym (gensym))  ; filespec part
        (psym (gensym))  ; directory port
        (ssym (gensym))  ; string from the port
        (loop (gensym))) ; name of loop
    `(if (:directory? ,path)
       (:dodir (,file ,path) ,@body)
       (let* ((,dsym (path-directory ,path))
              (,fsym (-> ,path path-strip-directory :wild->regex string->irregex))
              (,psym (open-directory ,dsym)))
         (let ,loop ((,ssym (read ,psym)))
           (:when (string? ,ssym)
             (:when (irregex-match ,fsym ,ssym)
               (let ((,file (:str ,dsym ,ssym)))
                 ,@body))
             (,loop (read ,psym))))
         (close-input-port ,psym)))))


;; Evaluates body for each file in a directory tree.
(define-macro (:dodtree bind . body)
  (let ((file (car bind))
        (path (cadr bind))
        (dsym (gensym))  ; dir function
        (asym (gensym))  ; arg to dir function
        (psym (gensym))  ; directory port
        (ssym (gensym))  ; string from the port
        (loop (gensym))) ; name of loop
    `(letrec ((,dsym (lambda (,asym)
                       (let ((,psym (open-directory ,asym)))
                         (let ,loop ((,ssym (read ,psym)))
                           (:when (string? ,ssym)
                             (let ((,file (:str ,asym "/" ,ssym)))
                               (if (:directory? ,file)
                                 (,dsym ,file)
                                 (begin ,@body)))
                             (,loop (read ,psym))))
                         (close-input-port ,psym)))))
       (,dsym (path-strip-trailing-directory-separator ,path)))))


;; Evaluates body for each file in a filespec and subdirectories.
(define-macro (:doftree bind . body)
  (let ((file (car bind))
        (path (cadr bind))
        (dsym (gensym))  ; directory part
        (fsym (gensym))  ; filespec part
        (func (gensym))  ; dir function
        (asym (gensym))  ; arg to dir function
        (psym (gensym))  ; directory port
        (ssym (gensym))  ; string from the port
        (loop (gensym))) ; name of loop
    `(if (:directory? ,path)
       (:dodtree (,file ,path) ,@body)
       (letrec ((,dsym (path-directory ,path))
                (,fsym (-> ,path path-strip-directory :wild->regex string->irregex))
                (,func (lambda (,asym)
                         (let ((,psym (open-directory ,asym)))
                           (let ,loop ((,ssym (read ,psym)))
                             (:when (string? ,ssym)
                               (let ((,file (:str ,asym ,ssym)))
                                 (if (:directory? ,file)
                                   (,func (:str ,file "/"))
                                   (:when (irregex-match ,fsym ,ssym)
                                     (begin ,@body))))
                               (,loop (read ,psym))))
                           (close-input-port ,psym)))))
         (,func ,dsym)))))


;; Runs tests from the given file.
(define (:run-tests file)
  (eval '(include "ply-sxml.scm"))
  (set! :total##_ 0)
  (set! :passed##_ 0)
  (set! :failed##_ 0)
  (:dolist (f (call-with-input-string (:slurp file) read-all))
    (let ((t (eval f)))
      (:when (Test##_? t)
        (set! :test-flag##_ #t)
        (:run-test t)
        (:inc! :total##_)
        (if :test-flag##_
          (:inc! :passed##_) 
          (:inc! :failed##_)))))
  (println file "  total: " :total##_ 
    "\tPassed: " :passed##_ "\tFailed: " :failed##_))
