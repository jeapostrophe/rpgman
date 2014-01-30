#lang racket/base
(require net/url)

(define tabs
  '("Race"
    "Class"
    "Item"
    "Monster"
    "EpicDestiny"
    "ParagonPath"
    "Ritual"
    "Feat"
    "Power"
    "Glossary"
    "Deity"
    "Trap"
    "Background"
    "Companion"
    "Disease"
    "Poison"
    "Terrain"
    "Theme"))
(define all-url-s
  "http://wizards.com/dndinsider/compendium/CompendiumSearch.asmx/ViewAll?&Tab=~a")

(module+ main
  (require racket/runtime-path
           racket/file
           racket/port)
  (define-runtime-path here ".")
  (define dest-dir (build-path here "ddi"))
  (make-directory* dest-dir)

  (for ([t (in-list tabs)])
    (define tab.xml (build-path dest-dir (format "~a.xml" t)))
    (unless (file-exists? tab.xml)
      (define tab-url-s (format all-url-s t))
      (define tab-url (string->url tab-url-s))
      (define-values (st hd data) (http-sendrecv/url tab-url))
      (display-to-file (port->bytes data) tab.xml))))
