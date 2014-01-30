#lang racket/base
(require net/url
         racket/runtime-path
         racket/match
         racket/list
         racket/file
         racket/port
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

(define-runtime-path throttle.time ".throttle")
(define (http-sendrecv/url+throttle rate-s u)
  (define p throttle.time)
  (unless (file-exists? p)
    (write-to-file -inf.0 p))
  (define last (file->value p))
  (sync (alarm-evt (+ last (* 1000 rate-s))))
  (write-to-file (current-inexact-milliseconds) p
                 #:exists 'replace)
  (http-sendrecv/url u))

(module+ main
  (define RATE 10)
  (define-runtime-path here ".")
  (define dest-dir (build-path here "ddi"))
  (make-directory* dest-dir)

  (define all 0)
  (for ([t (in-list tabs)])
    (printf "~a\n" t)

    (define tab.xml (build-path dest-dir (format "~a.xml" t)))
    (unless (file-exists? tab.xml)
      (define tab-url-s (format all-url-s t))
      (define tab-url (string->url tab-url-s))
      (define-values (st hd data) (http-sendrecv/url+throttle RATE tab-url))
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

    (printf "\t~a\n" actual-total)
    (set! all (+ all actual-total))

    (define tab.db (build-path dest-dir (format "~a.db" t)))
    (make-directory* tab.db)
    (printf "\t\t")
    (for ([r (in-list rs)]
          [i (in-range 10)])
      (match-define `(,_ " " (ID ,ID) . ,_) r)
      (define ID.entry (build-path tab.db ID))
      (unless (file-exists? ID.entry)
        (printf "~a = ~a, " i ID)))
    (printf "\n"))

  (printf "Database Size: ~a\n" all)
  (printf "Seconds between Requests: ~a\n"
          (real->decimal-string
           (/ (* 60 60 24 28) all))))
