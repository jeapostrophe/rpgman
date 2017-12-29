#!/usr/bin/env racket
#lang racket/base
(require racket/runtime-path
         racket/pretty
         math/base
         racket/list
         racket/match
         racket/format
         racket/string
         racket/set
         db
         sxml
         text-table)
(define-runtime-path ddi "ddi")

(define (file->sxml f)
  (call-with-input-file f
    (λ (ip) (ssax:xml->sxml ip '()))))

(define (string-list->list s)
  (map string-trim (string-split s ",")))
(define (string-list->set s)
  (list->set (map string->symbol (string-list->list s))))
(define (sj . l) (string-join l))

(define okay-source-books
  (set
   ;; Original Book
   #;"Monster Manual"
   ;; Supposedly these two books are great
   "Monster Manual 3"
   "Monster Vault"))
(define (monster-initialize!)
  (define db (sqlite3-connect #:database (build-path ddi "Monster.sqlite")
                              #:mode 'create))
  (unless (table-exists? db "Monsters")
    (query-exec db
                (sj
                 "CREATE TABLE Monsters ("
                 "ID integer PRIMARY KEY,"
                 "Name text NOT NULL,"
                 "Level integer,"
                 "GroupRole text NOT NULL,"
                 "CombatRole text NOT NULL"
                 ");"))
    (define stmt (sj "INSERT INTO Monsters"
                     "(ID, Name, Level, GroupRole, CombatRole)"
                     "VALUES ($1, $2, $3, $4, $5)"))

    (define mon-xml (file->sxml (build-path ddi "Monster.xml")))
    (match-define `(*TOP* ,_ (Data (Results . ,mons) (Totals . ,_))) mon-xml)
    (for ([m (in-list mons)])
      (match-define `(Monster (ID ,id) (Name ,name) (Level ,(app string->number lvl))
                              (GroupRole ,gr) (CombatRole ,cr) (SourceBook ,sb))
        m)
      (define sbs (list->set (string-list->list sb)))
      (unless (set-empty? (set-intersect sbs okay-source-books))
        (query-exec db stmt id name lvl gr cr)))))

(define TotalXP-PHB-pg29
  (make-immutable-hasheq
   ;; Level TotalXP
   '([ 1       0]
     [ 2    1000]
     [ 3    2250]
     [ 4    3750]
     [ 5    5500]
     [ 6    7500]
     [ 7   10000]
     [ 8   13000]
     [ 9   16500]
     [10   20500]
     [11   26000]
     [12   32000]
     [13   39000]
     [14   47000]
     [15   57000]
     [16   69000]
     [17   83000]
     [18   99000]
     [19  119000]
     [20  143000]
     [21  175000]
     [22  210000]
     [23  255000]
     [24  310000]
     [25  375000]
     [26  450000]
     [27  550000]
     [28  675000]
     [29  825000]
     [30 1000000])))

(define ExperiencePointRewards-DMG-pg56
  (make-immutable-hasheq
   ;; Lvl  Standard Minion Elite Solo
   '([ 1   100    25   200    500]
     [ 2   125    31   250    625]
     [ 3   150    38   300    750]
     [ 4   175    44   350    875]
     [ 5   200    50   400   1000]
     [ 6   250    63   500   1250]
     [ 7   300    75   600   1500]
     [ 8   350    88   700   1750]
     [ 9   400   100   800   2000]
     [10   500   125  1000   2500]
     [11   600   150  1200   3000]
     [12   700   175  1400   3500]
     [13   800   200  1600   4000]
     [14  1000   250  2000   5000]
     [15  1200   300  2400   6000]
     [16  1400   350  2800   7000]
     [17  1600   400  3200   8000]
     [18  2000   500  4000  10000]
     [19  2400   600  4800  12000]
     [20  2800   700  5600  14000]
     [21  3200   800  6400  16000]
     [22  4150  1038  8300  20750]
     [23  5100  1275 10200  25500]
     [24  6050  1513 12100  30250]
     [25  7000  1750 14000  35000]
     [26  9000  2250 18000  45000]
     [27 11000  2750 22000  55000]
     [28 13000  3250 26000  65000]
     [29 15000  3750 30000  75000]
     [30 19000  4750 38000  95000]
     [31 23000  5750 46000 115000]
     [32 27000  6750 54000 135000]
     [33 31000  7750 62000 155000]
     [34 39000  9750 78000 195000]
     [35 47000 11750 94000 235000])))

(define (plan-encounters! player-count)
  (displayln
   (table->string
    (for/fold ([prev-xp 0]
               [tbl empty]
               #:result
               (cons (list "Level" "XP (Player)"
                           "XP (Party)"
                           "Encounters")
                     (reverse tbl)))
              ([level (in-range 1 30)])
      (match-define (list required-xp) (hash-ref TotalXP-PHB-pg29 (+ 1 level)))
      (define xp-left (- required-xp prev-xp))

      (define (standard-xp level)
        (first (hash-ref ExperiencePointRewards-DMG-pg56 level)))
      (define per-encounter-hard
        (ceiling
         (* (standard-xp (+ level 4))
            player-count)))

      (define xp-left-party (* player-count xp-left))

      (define (dole-encounters xp es)
        (define s (sum es))
        (unless (= s 100)
          (error 'dole-encounters "Encounters do not total 100%, s=~e" s))
        (for/list ([e (in-list es)])
          (ceiling (* xp (/ e 100)))))
      (define xp-encounters (dole-encounters xp-left-party '(15 20 15 25 25)))

      (define encounters
        (ceiling (/ xp-left-party per-encounter-hard)))
      
      (values
       required-xp
       (cons (list level xp-left xp-left-party encounters)
             tbl))))))

;; DMG 126-- has a table of magic item rewards

(module+ main
  (monster-initialize!)
  (plan-encounters! 4))
