#lang racket/base
(require net/url
         racket/match
         racket/list
         xml/path
         xml)

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

(define (se-path*/list+tags p xe)
  (filter pair? (se-path*/list p xe)))

(module+ main
  (require racket/runtime-path
           racket/file
           racket/port)
  (define-runtime-path here ".")
  (define dest-dir (build-path here "ddi"))
  (make-directory* dest-dir)

  (for ([t (in-list tabs)])
    (printf "~a\n" t)

    (define tab.xml (build-path dest-dir (format "~a.xml" t)))
    (unless (file-exists? tab.xml)
      (define tab-url-s (format all-url-s t))
      (define tab-url (string->url tab-url-s))
      (define-values (st hd data) (http-sendrecv/url tab-url))
      (display-to-file (port->bytes data) tab.xml))

    (define xs (file->string tab.xml))
    (define xe (parameterize ([collapse-whitespace #t]
                              [xexpr-drop-empty-attributes #t])
                 (string->xexpr xs)))

    (match-define
     `((Table ,(== t)) (Total ,(app string->number expected-total)))
     (se-path*/list+tags '(Data Totals Tab) xe))

    (define rs
      (se-path*/list+tags '(Data Results) xe))
    (define actual-total
      (length rs))

    (unless (= expected-total actual-total)
      (error 'ddi "~a: count mismatch: ~e vs ~e"
             t expected-total actual-total))

    (printf "\t~a\n" actual-total)))
