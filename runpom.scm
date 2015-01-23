;; +-------------------------------------------------------------+
;; | runpom - Runs a program off a POM.                          |
;; |                                                             |
;; | The main class goes in a <main> property; run this command  |
;; | in the same directory as the POM.                           |
;; |                                                             |
;; | Usage:                                                      |
;; | $ runpom arg1 arg2 ...                                      |
;; +-------------------------------------------------------------+

;; This needs to produce, save and parse the effective pom.

(include "plylib/ply-sxml.scm")

(define (get-main)
  (let* ((sxml (:file->sxml "pom.xml"))
         (main (:xpath-get "//project/properties/main/text()" sxml)))
    (if (null? main) "" (car main))))

(define (get-deps file)
  (let ((sxml (:file->sxml file)))
    (:xpath-get "//project/dependencies/*" sxml)))

(define (get-super path)
  (if (file-exists? path)
    (append (get-super (:str "../" path)) (get-deps path))
    (quote ())))

(define (make-path dep repo)
  (let ((gid (:str-join "/" (:split "\\." (:2nd (assq 'groupId dep)))))
        (aid (:2nd (assq 'artifactId dep)))
        (vid (:2nd (assq 'version dep))))
    (:str repo "/" gid "/" aid "/" vid "/" aid "-" vid ".jar")))

(define (jar-paths deps repo)
  (if (null? deps)
    (quote ())
    (cons (make-path (cdar deps) repo) (jar-paths (cdr deps) repo))))

(define (make-classpath)
  (let* ((repo (:str (getenv "HOME") "/.m2/repository"))
         (jars (jar-paths (get-super "pom.xml") repo)))
    (:str "target/classes:" (:str-join ":" jars))))

(define (main args)
  (let* ((cp (make-classpath))
         (cls (get-main))
         (cmd (:str "java -cp " cp " " cls " " (:str-join " " args))))
    (shell-command cmd)))

(main (cdr (command-line)))
