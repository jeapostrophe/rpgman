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
         sxml)
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
   "Monster Vault"
   ;; This is recommended too
   "Demonomicon"))
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
        (query-exec db stmt id name lvl gr cr))))

  (disconnect db))

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
     [30 1000000]
     ;; Included so that there is some fighting at level 30
     [31 2000000])))

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

(define (random-list-ref l)
  (list-ref l (random (length l))))

(define (plan-encounters! player-count)
  (define db (sqlite3-connect #:database (build-path ddi "Monster.sqlite")
                              #:mode 'read-only))

  ;; Library
  (define (xp-rewards level)
    (or (hash-ref ExperiencePointRewards-DMG-pg56 level #f)
        (xp-rewards (sub1 level))))
  (define (monster-xp what level)
    (match-define (list standard minion elite solo)
      (xp-rewards level))
    (match what
      ['Solo solo]
      ['Elite elite]
      ['Minion minion]
      [(or 'Standard 'Skirmisher 'Controller 'Artillery 'Soldier 'Brute 'Lurker)
       standard]
      [(? list?)
       (apply max
              (for/list ([w (in-list what)])
                (monster-xp w level)))]))
  (define (mob-xp t)
    (match-define (vector how-many what level) t)
    (* how-many (monster-xp what level)))
  (define (template-xp t)
    (for/sum ([t (in-list t)]) (mob-xp t)))

  (define (what->sql-where what)
    (match what
      [(or 'Solo 'Elite 'Minion)
       (~a "GroupRole LIKE '%"what"%'")]
      [(or 'Skirmisher 'Controller 'Artillery 'Soldier 'Brute 'Lurker)
       (~a "CombatRole LIKE '%"what"%'")]
      [(list a b)
       (~a "(" (what->sql-where a) " OR " (what->sql-where b) ")")]))

  (define (plan-mob disp t)
    (let/ec return
      (match-define (vector how-many what level) t)
      (define xp-budget (mob-xp t))

      (define available-mobs
        (query-rows db
                    (sj "SELECT "
                        "ID, Name, Level, GroupRole, CombatRole "
                        "FROM Monsters "
                        "WHERE Level <= $1 "
                        "AND " (what->sql-where what))
                    level))
      (when (empty? available-mobs)
        (return #f))

      (struct mob-info (id name level actual-cost how-many-to-have effective-cost score)
        #:prefab)
      (define all-mob-infos
        (for/list ([m (in-list available-mobs)])
          (match-define (vector id name level (app string->symbol what) _) m)
          (define actual-cost (monster-xp what level))
          (define how-many-make-that-fract (/ xp-budget actual-cost))
          (define how-many-make-that-int (floor how-many-make-that-fract))
          (define how-many-to-have
            how-many
            #;
            (if (zero? how-many-make-that-int) 1
                how-many-make-that-int))
          (define effective-cost (* actual-cost how-many-to-have))
          (define score (abs (- xp-budget effective-cost)))
          (mob-info id name level actual-cost how-many-to-have effective-cost score)))
      (define ranked-mob-infos
        (sort all-mob-infos <= #:key mob-info-score))
      (define best-score
        (mob-info-score (first ranked-mob-infos)))
      (define best-mob-infos
        (takef ranked-mob-infos (λ (mi) (= (mob-info-score mi) best-score))))
      (define sol (random-list-ref best-mob-infos))
      (let ()
        (match-define (mob-info id name level _ how-many-to-have effective-cost _) sol)
        (disp (~a how-many-to-have "x "name" (Lvl "level", "id")"))
        effective-cost)))

  (define (plan-mobs disp mobs)
    (let/ec return
      (for/sum ([m (in-list mobs)])
        (or (plan-mob disp m) (return #f)))))

  ;; Main

  (define level-sep (~a (make-string 80 #\-) "\n"))
  (define (col-sep v)
    (define s (~a v))
    (~a s (make-string (- 10 (string-length s)) #\space)))
  (display (~a (col-sep "Level") "\n"))
  (display level-sep)

  (for/fold ([prev-xp 0])
            ([level (in-range 1 31)])
    (match-define (list required-xp) (hash-ref TotalXP-PHB-pg29 (+ 1 level)))
    (define xp-left (- required-xp prev-xp))

    (define (standard-xp level)
      (first (hash-ref ExperiencePointRewards-DMG-pg56 level)))
    (define per-encounter-hard
      (ceiling
       (* (standard-xp (+ level 4))
          player-count)))

    (define xp-left-party (* player-count xp-left))

    ;; DMG 58 -- These are described there.

    ;; XXX Incorporate the "simple substitutions", in particular it says
    ;; 4 Minions = 1 Standard
    ;; 1 Elite   = 2 Standard
    ;; 1 Solor   = 5 Standard
    (define encounter-templates
      (list
       (vector "Battlefield Control"
               (list (vector 1 'Controller (+ level 5))
                     (vector 5 'Skirmisher (+ level 1))))
       (vector "Commander and Troops"
               (list (vector 1 '(Controller Soldier) (+ level 6))
                     (vector 3 '(Brute Soldier) (+ level 1))
                     (vector 2 'Artillery (+ level 1))))
       (vector "Dragon's Den"
               (list (vector 1 'Solo (+ level 3))))
       (vector "Dragon's Den"
               (list (vector 1 'Solo (+ level 1))
                     (vector 1 'Elite (+ level 0))))
       (vector "Double Line"
               (list (vector 3 '(Brute Soldier) (+ level 2))
                     (vector 1 'Controller (+ level 4))
                     (vector 1 '(Artillery Lurker) (+ level 4))))
       (vector "Double Line"
               (list (vector 3 '(Brute Soldier) level)
                     (vector 2 'Artillery (+ level 1))
                     (vector 1 'Controller (+ level 2))
                     (vector 1 'Lurker (+ level 2))))
       (vector "Wolf Pack" (list (vector 3 'Skirmisher (+ level 7))))
       (vector "Wolf Pack" (list (vector 4 'Skirmisher (+ level 5))))
       (vector "Wolf Pack" (list (vector 6 'Skirmisher (+ level 2))))))

    (display (~a (col-sep level) xp-left-party "\n"))
    (define how-many-encounters
      (ceiling (/ xp-left-party per-encounter-hard)))
    (for/fold ([xp-so-far 0])
              ([i (in-naturals 1)]
               [e (in-range how-many-encounters)]
               [t-id*mobs (in-list (shuffle encounter-templates))])

      (define (try t-id*mobs)
        (match-define (vector template-id mobs) t-id*mobs)
        (define mob-disps empty)
        (define target-xp (template-xp mobs))
        (define this-xp
          (plan-mobs
           (λ (s) (set! mob-disps (cons s mob-disps)))
           mobs))
        (if this-xp
          (values
           template-id
           target-xp
           this-xp
           (reverse mob-disps))
          (try (random-list-ref encounter-templates))))
      (define-values (template-id target-xp this-xp mob-disps)
        (try t-id*mobs))

      (define xp-after-this (+ this-xp xp-so-far))

      (display "\n")
      (display (~a (col-sep "") (col-sep xp-after-this)
                   "Reward: " "Magic-" (+ level e) "\n"))
      (display (~a (col-sep "") (col-sep "") "= "template-id "\n"))
      (for ([s (in-list mob-disps)])
        (display (~a (col-sep "") (col-sep "") s "\n")))
      ;; DMG 126 -- has a table of magic item rewards that
      ;; is like this, except that it starts at (+ level e
      ;; 1) and only gives four per level.
      (display (~a (col-sep "") (col-sep "") this-xp" ("target-xp ")\n"))

      xp-after-this)
    (display level-sep)

    required-xp)

  (disconnect db))

;; XXX Rewards --- It ends up that there will be about 5 (sometimes 6)
;; magic items of every level. Given that it is spec'd for a 4 player
;; party, this means each character will get about one per level. The
;; best thing to do then is to have each player pre-plan what they
;; would get if they got something of that level, so you can easily
;; figure it out post-battle.

;; XXX Gauntlet --- I imagine some sort of "encounter point" system
;; where if you win you get one and if you lose you lose one. You can
;; spend one at the end of a battle to get the reward. You can spend
;; one at the start of a battle to get advantage/initiative/set up the
;; map/etc. You can receive one at the start of a battle to let the
;; other side have those things. In between battles, you face a number
;; of skill challenges that result in gaining or paying them.

;; === Advice 1 (from somewhere)
;; 1. Add hazards to maps
;; 2. Use monsters from MM3, Monster Vault, and Demonomicon (why?)
;; 3. Divide monster hit points by 2. Multiply their damage by 1.5.

;; === [[https://rpg.stackexchange.com/questions/42147/how-to-speed-up-4e-combat][Advice 2]]
;; 1. Limit turn time (+1 token within time, -1 token out of time)
;; 2. Roll attack and damage together
;; 3. Use average damage
;; 4. Change initiative to be like Descent (all monsters, then all
;;    PCs)
;; 5. Increase damage to same ratios as Lvl1 ---
;;    http://dmg42.blogspot.com.au/2012/02/boot-on-face-of-level-1-damage-forever.html

;; === [[https://rpg.stackexchange.com/questions/15591/standard-monster-tactics-for-4e-dd-combat][Advice 3]]
;; 1. Focus Fire on 1 player
;; 2. Always target the weakest available monster
;; 3. Sometimes ignored being marked
;; 4. Flank players
;; 5. Make good use of controllers
;; 6. Protect weak monsters
;; 7. Use terrain (columns provide cover but don't block your attacks)
;; 8. Win initiative
;; 9. Retreat & reinforce the monsters
;; 10. Encounter Building
;;     1. Frontline & Backline --- Keep busy & Hard hitters
;;     2. Monster Synergy --- Complement monsters
;;     3. Terrain Synergy --- Go heavy
;;     4. Control is Good --- 30% blockers, 20% damage, 50% control
;;     5. Don't use Pre-MM3 Solos --- Much more complex and
;;        interesting
;;     6. Use Higher Level Monsters when Possible --- 4 Lvl+1 is
;;        harder than 5 Lvl

;; Use a Kingdom-Death-like system of "Hunt then Showdown then
;; Settlement"?
;; - If you succeed on the Hunt, then you get to lay out arena and you
;;   get to ambush, have initiative, etc.
;; - If you succeed in the Showdown, then you get a magic item,
;;   otherwise not

;; https://www.reddit.com/r/DnD/comments/7mokrn/combatboardgamedescentlikecompetitive_dd/

;; Just use Dungeon Delve?

;; Collaborative stage building --- based on largest monster, choose
;; tile set and go back and forth placing them. Maybe put down one big
;; stage, then characters, then terrain. Stuff like switch and then
;; what switch does

;; Incentivize not turtling --- small map and treasure on field

;; D&D tech tree --- instead of levels, use tech points that
;; corresponds to same stuff as D&D but with scaling so that you get
;; enough tech points post 10 to multi class


(module+ main
  (monster-initialize!)
  (plan-encounters! 4))
