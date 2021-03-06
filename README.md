# Gabmit Ply

I wanted to be more productive with Gambit Scheme and thought
I may add functions available in Clojure and newLISP or in
some of their libraries.

## Features

* A prelude module with commonly used functions;
* XML parsing and XPATH search;
* I/O for strings; local and remote files;
* String-manipulation.

## Sample Usage

```scheme
;; +-------------------------------------------------------------+
;; | hits - Counts the hits in a Google search.                  |
;; |                                                             |
;; | Usage:                                                      |
;; | $ hits word1 word2 ...                                      |
;; +-------------------------------------------------------------+

(include "plylib/ply-io.scm")

(define pattern "About [\\d,]+")
(define server  "www.google.com/search?hl=en&q=")

(define (main args)
  (let* ((url (:str server (:str-join "+" args)))
         (lst (:extract pattern (:get-url url))))
    (:unless (null? lst)
      (println (car lst)))))

(main (cdr (command-line)))
```

## Documentation

TBW

## License

Copyright © 2012 Armando Blancas.

This software is published under the [MIT License](http://opensource.org/licenses/MIT).

## Acknowledgements

Gambit Ply uses IRREGEX and SSAX-SXML, which are included
for convenience. The rights and credit for those libraries
belong to their respective authors.

`IrRegular Expressions`

Copyright (c) 2005-2012 Alex Shinn.  All rights reserved.
BSD-style license: http://synthcode.com/license.txt

`SSAX-SXML`
By Dmitry Lizorkin

The software is vailable on [sourceforge](http://ssax.sourceforge.net/).
Documentation at [okmij.org](http://okmij.org/ftp/Scheme/xml.html).
