#lang racket/base
(require net/url
         racket/runtime-path
         racket/match
         racket/path
         racket/string
         racket/list
         racket/file
         racket/port
         net/uri-codec
         xml/path
         xml)

(define tabs
  '("Race" "Class" "Item" "Monster" "EpicDestiny" "ParagonPath"
    "Ritual" "Feat" "Power" "Glossary" "Deity" "Trap" "Background"
    "Companion" "Disease" "Poison" "Terrain" "Theme"))
(define all-url-s
  "http://wizards.com/dndinsider/compendium/CompendiumSearch.asmx/ViewAll?&Tab=~a")
(define entry-url-s
  "http://wizards.com/dndinsider/compendium/~a.aspx?id=~a")
(define top-login-url-s
  "http://wizards.com/dndinsider/compendium/login.aspx")
(define static-url-s
  "http://wizards.com/dndinsider/compendium/~a")

(define (se-path*/list+tags p xe)
  (filter pair? (se-path*/list p xe)))

(define-runtime-path throttle.time ".throttle")
(define (http-sendrecv/url+throttle #:method [method #"GET"]
                                    #:data [data #f]
                                    #:headers [headers empty]
                                    rate-s u)
  (define p throttle.time)
  (unless (file-exists? p)
    (write-to-file -inf.0 p))
  (define last (file->value p))
  (sync (alarm-evt (+ last (* 1000 rate-s))))
  (write-to-file (current-inexact-milliseconds) p
                 #:exists 'replace)
  (http-sendrecv/url #:method method
                     #:data data
                     #:headers headers
                     u))

(define (http-sendrecv/url+throttle+login email password cookie rate u)
  (define-values (st hd data)
    (http-sendrecv/url+throttle
     #:headers
     (list cookie)
     rate u))

  (cond
    [(and (regexp-match #rx"302" st)
          (ormap (λ (h) (regexp-match #rx"^Location: (.*?)$" h)) hd))
     =>
     (match-lambda
      [(list _ (app bytes->string/utf-8 login-url-s))
       (define login-url (string->url top-login-url-s))
       (define-values (st hd data)
         (http-sendrecv/url+throttle 0 login-url))
       (define data-xe (string->xexpr (port->string data)))

       (define cookie-bs
         (second
          (ormap (λ (h) (regexp-match #rx"^Set-Cookie: (.*?);.*$" h)) hd)))

       (define is
         (filter
          (λ (xe)
            (and (list? xe)
                 (eq? 'input (first xe))))
          (se-path*/list '(html body form) data-xe)))

       (match-define
        `(input ((id "__VIEWSTATE") (name ,viewstate-name) (type "hidden")
                 (value ,viewstate-value)))
        (first is))
       (match-define
        `(input ((id "__EVENTVALIDATION") (name ,ev-name) (type "hidden")
                 (value ,ev-value)))
        (second is))

       (let ()
         (define login-data
           (alist->form-urlencoded
            (list (cons (string->symbol viewstate-name) viewstate-value)
                  (cons (string->symbol ev-name) ev-value)
                  (cons 'email email)
                  (cons 'password password)
                  (cons 'InsiderSignin "Sign In"))))
         (define-values (st hd data)
           (http-sendrecv/url+throttle
            #:method "POST"
            #:data login-data
            #:headers (list "Content-Type: application/x-www-form-urlencoded")
            0 login-url))

         ;; xxx this is it

         (displayln st)
         (for-each displayln (filter (λ (h) (regexp-match #rx"Set-Cookie:" h)) hd))
         (displayln (port->string data))

         (exit 0))])]
    [(regexp-match #rx"200" st)
     (values st hd data)]
    [else
     (error 'http-sendrecv/url+throttle+login
            "Unknown response: ~e" (vector st hd (port->bytes data)))]))

(module+ main
  (define RATE 2)
  (define-runtime-path here ".")
  (define dest-dir (build-path here "ddi"))
  (define static-dir (build-path dest-dir "static"))
  (make-directory* dest-dir)
  (make-directory* static-dir)

  (define EMAIL (first (file->lines (build-path here ".ddi_email"))))
  (define PASSWORD (first (file->lines (build-path here ".ddi_pass"))))
  (define COOKIE (file->string (build-path here ".ddi_cookie")))

  (collapse-whitespace #t)
  (xexpr-drop-empty-attributes #t)

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
    (define xe (string->xexpr xs))

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

    (printf " ~a\n" actual-total)
    (set! all (+ all actual-total))

    (define tab.db (build-path dest-dir (format "~a.db" t)))
    (make-directory* tab.db)
    (for ([r (in-list rs)]
          ;; (in-range 1)
          [i (in-naturals)])
      (with-handlers ([exn:fail?
                       (λ (x)
                         ((error-display-handler) (exn-message x) x))])
        (match-define `(,_ " " (ID ,ID) . ,_) r)
        (define ID.entry (build-path tab.db ID))
        (unless (file-exists? ID.entry)
          (define ID.url-s
            (format entry-url-s
                    ;; "skill" and "companion" are necessary for like
                    ;; 7 entries from Glossary and Familiars
                    (string-downcase t)
                    ID))
          (printf "  ~a/~a. ~a\n" i actual-total ID.url-s)
          (define ID.url
            (string->url ID.url-s))

          (define-values (st hd data)
            (http-sendrecv/url+throttle+login EMAIL PASSWORD COOKIE RATE ID.url))

          (display-to-file (port->bytes data) ID.entry))

        (let ()
          (local-require sxml/html
                         sxml)
          (define xe (html->xexp (file->string ID.entry)))

          (define statics
            (append ((sxpath '(// link @ href *text*)) xe)
                    ((sxpath '(// img @ src *text*)) xe)))

          (for ([ms (in-list statics)])
            (define-values
              (s url-s)
              (if (regexp-match #rx"^http:" ms)
                (values (string-join
                         (map path/param-path
                              (url-path (string->url ms)))
                         "/")
                        ms)
                (values ms (format static-url-s ms))))
            (define static.pth (build-path static-dir s))
            (unless (file-exists? static.pth)
              (printf "~a\n" s)
              (make-directory* (path-only static.pth))
              (define static.url (string->url url-s))

              (define-values (st hd data)
                (http-sendrecv/url+throttle+login
                 EMAIL PASSWORD COOKIE RATE
                 static.url))
              (display-to-file (port->bytes data) static.pth)))))))

  (printf "Database Size: ~a\n" all)
  (printf "Seconds between Requests: ~a\n"
          (real->decimal-string
           (/ (* 60 60 24 28) all))))