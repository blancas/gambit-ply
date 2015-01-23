;; +-------------------------------------------------------------+
;; | runrepl - Runs the Clojure REPL off a POM.                  |
;; |                                                             |
;; | An optional source classpath goes in a <repl> property;     |
;; | run this command on the same directory as the POM.          |
;; |                                                             |
;; | Usage:                                                      |
;; | $ runrepl                                                   |
;; +-------------------------------------------------------------+

(include "plylib/ply-sxml.scm")

(define (get-deps file)
  (let ((sxml (:file->sxml file)))
    (:xpath-get "//project/dependencies/*" sxml)))

(define (repl-src file)
  (let* ((sxml (:file->sxml file))
         (main (:xpath-get "//project/properties/repl/text()" sxml)))
    (if (null? main) "" (car main))))

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
         (jars (jar-paths (get-super "pom.xml") repo))
         (src (repl-src "pom.xml")))
    (:str "target/classes:src/main/clojure:" src (:str-join ":" jars))))

(let* ((cp (make-classpath))
       (cmd (:str "java -cp " cp " jline.ConsoleRunner clojure.main")))
  (shell-command cmd))
