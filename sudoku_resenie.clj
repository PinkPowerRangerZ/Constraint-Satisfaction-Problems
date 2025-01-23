(ns user (:use clojure.test))
(ns user (:require [clojure.string :as s]))

(def x #{1 2 3 4 5 6 7 8 9})
x
(defn transform "transformiraj go sekoj element vo domen na vrednosti za toj element" [lst]
  (cond
    (empty? lst) ()
    (list? (first lst)) (cons (transform (first lst)) (transform (rest lst)))
    (= (first lst) 0) (cons x (transform (rest lst)))
    :else
    (cons (into #{} (list (first lst))) (transform (rest lst)))

    ))
(def initial  '((0 2 5 0 0 1 0 0 0) (1 0 4 2 5 0 0 0 0)(0 0 6 0 0 4 2 1 0) (0 5 0 0 0 0 3 2 0)(6 0 0 0 2 0 0 0 9) (0 8 7 0 0 0 0 6 0)(0 9 1 5 0 0 6 0 0) ( 0 0 0 0 7 8 1 0 3) (0 0 0 6 0 0 5 9 0)))
(def sudoku (transform '((0 2 5 0 0 1 0 0 0) (1 0 4 2 5 0 0 0 0)(0 0 6 0 0 4 2 1 0) (0 5 0 0 0 0 3 2 0)(6 0 0 0 2 0 0 0 9) (0 8 7 0 0 0 0 6 0)(0 9 1 5 0 0 6 0 0) ( 0 0 0 0 7 8 1 0 3) (0 0 0 6 0 0 5 9 0))))
sudoku

(transform '((5) ( 3  5) (4)))


(into #{} (list (first '(1 2 3))))


;izbrisi element num od set
(defn delete [num set]
  (cond
    (= (first set) num) (rest set)
    :else
    (cons (first set) (delete num (rest set)))
    )
  )


(delete 2 '#{1 2 3})

;vrakja tocno ako num pripagja na lst
(defn member [lst num]
  (cond
    (empty? lst) false
    (= num (first lst)) true
    :else (member (rest lst) num)
  ))
(member '(1 2 3) 2)


;izbrisi go elementot number od podlistata so reden proj index
(defn process [number index lst]
  (cond
    (empty? lst) ()
    (and (= index 0)(member (first lst) number)) (cons (delete number (first lst)) (rest lst))
    :else (cons (first lst) (process number (- index 1) (rest lst)))
  ))
(process 3 0 '((1 2 3) (1 2 3) (1 2 3)))


;ovoj del e za definiraneje na redovite, kolonite i blokovite

(def rows (partition-all 9 (range 0 81)))
rows

(def columns(for [x '(0 1 2 3 4 5 6 7 8)] (range x 81 9) ))
columns

(def y 0)
y

;definicija na vrednostite koi treba da se dodadat na prviot element od blokot za da se dobijat site elementi od blokot
(def block_adds '(0 1 2 9 10 11 18 19 20))
; imakji go pocetokot na blokot vrati gi site indexi od toj blok
(defn block [start adds]
  (cond
    (empty? adds) ()
    :else (cons (+ start (first adds) ) (block start (rest adds)))
    )
  )
(block 0 block_adds)

;definiraj gi pocetocite na blokot
(def block_starts '(0 3 6 27 30 33 54 57 60))
block_starts

;konecno funkcija koja gi gi dava site blokovi
(defn define_blocks [starts]
  (cond
    (empty? starts) ()
    :else (cons (block (first starts) block_adds) (define_blocks (rest starts)))
    )

  )
(def blocks (define_blocks block_starts))
blocks

(defn find [num lst]
   (cond
     (empty? lst) ()
     (member (first lst) num) (first lst)
     :else (find num (rest lst)
   )  )

  )

(defn append [lst]
  (list (first lst) 'token'))
(append '(1))

;vratu true ako lst ima samo eden element
(defn samo_eden[lst]
  (empty? (rest lst)))

;za daden index najdi gi site indexi od negoviot blok, kolona i red
(defn najdi_impacted_indices[index]
(delete index (into #{} (concat (concat (find index rows) (find index columns)) (find index blocks)))))
(najdi_impacted_indices 2)


; izbrisi go brojot broj od redot, kolonata i blokot na dodelenata vrednost
(defn brisi [broj sevkupno_resenie impacted_lst]
  (cond
    (empty? impacted_lst) sevkupno_resenie
    :else (brisi broj (process broj (first impacted_lst) sevkupno_resenie) (rest impacted_lst))
    )
  )


;za site indexi shto se imaat vekje dodeleno vrednost (mnozestvo ima samo eden) kje gi izbrise tie vrednosti od soodvetnata kolona red i blok
(defn resavaj [sevkupno_resenie vekje_izbrisani lst index]
  (cond
    (empty? lst) (list sevkupno_resenie vekje_izbrisani)
    (and (not (member vekje_izbrisani index))(samo_eden (first lst))) (resavaj (brisi(first (first lst)) sevkupno_resenie (najdi_impacted_indices index)) (cons index vekje_izbrisani) (rest lst) (+ index 1))
    :else (resavaj sevkupno_resenie vekje_izbrisani (rest lst) (+ index 1))
    )
  )

;proveri dali site ogranichuvanjata (nema ista vrednost vo red, kolona, blok) koi proizleguvaat od mnozestvata so samo eden element se vekje propagirani
(defn site_edinecni_propagirani [lst vekje_izbrisani index]
  (cond
    (empty? lst) true
    (and (samo_eden (first lst)) (not (member vekje_izbrisani index))) false
    (and (samo_eden (first lst)) (member vekje_izbrisani index)) (site_edinecni_propagirani (rest lst) vekje_izbrisani (+ index 1))
    :else (site_edinecni_propagirani (rest lst) vekje_izbrisani (+ index 1))

  ))

;ovde primer vrakja true zatoa sto zatoa sto indexot na (1) e vekje vo izbrisanite (0)
(site_edinecni_propagirani '((1)(2 3)(3 4)(8 5)) '(0 5) 0)

;ovde primer vrakja false zatoa sto pgranicuvanjata od 2 ne se propagirani
(site_edinecni_propagirani '((1)(2)(3 4)(8 2 5)) '(0) 0)


; kazi dali vrednosta e soodvetna da se postavi na taa pozicija (dali vo redot kolonata ili blokot ne postoi mnozestvo sto ja sodrzi)
(defn soodvetna_vrednost? [value lst impacted index]
  (cond
    (empty? lst) true
    (member impacted index) (and (not (member (first lst) value)) (soodvetna_vrednost? value (rest lst) impacted (+ index 1)))
    :else (soodvetna_vrednost? value (rest lst) impacted (+ index 1))
    )
  )

; qko postoi vrednost od mnozestvoto vrednosti za daden index koja e kompatibilna so mnozestvata na zasegnatite indexi( ne postoi vo mnozestavata za redot kolonata blokot) togas vrati ja nea, vo sprotivno vrati -1
(defn sodrzi_soodvetna [vrednosti sevkupno_resenie impacted]
  (cond
    (empty? vrednosti) -1
    (soodvetna_vrednost? (first vrednosti) sevkupno_resenie impacted 0) (first vrednosti)
    :else (sodrzi_soodvetna (rest vrednosti) sevkupno_resenie impacted)
    )
  )
; primer ovde 3 ne e sodrzana vo zasegnatite mnozestva (1 2) i zatoa e soodvetna
(list(sodrzi_soodvetna '( 2 3) '((1 2 3)(4 2 5)(7 8 9)) '(1 2)))


;pravi dodela na nekoja vrednost koja ispolnuva uslovi
(defn dodelichka [lst sevkupno_resenie index vekje_izbrisani]
  (cond
    (empty? lst) ()
    (not= -1 (sodrzi_soodvetna (first lst) sevkupno_resenie (najdi_impacted_indices index))) (cons (list (sodrzi_soodvetna (first lst) sevkupno_resenie (najdi_impacted_indices index))) (rest lst))
    :else (cons (first lst) (dodelichka (rest lst) sevkupno_resenie (+ index 1) vekje_izbrisani))
  ))





;proveruva dali e vozmozno da se napravi dodela i ako da ja pravi istata
(defn napravi_dodela [lst sevkupno_resenie index vekje_izbrisani]
  (cond
    (empty? lst) ()
    (not= -1 (sodrzi_soodvetna (first lst) sevkupno_resenie (najdi_impacted_indices index))) (cons (list (sodrzi_soodvetna (first lst) sevkupno_resenie (najdi_impacted_indices index)))(rest lst))
    :else (cons (first lst) (dodelichka (rest lst) sevkupno_resenie (+ index 1) vekje_izbrisani))
  ))


;vrakja true ako za bilo koe mnozesto moze da se napravi dodela koja nema da gi narushuva ogranicuvanjata
(defn vozmozna_dodela [lst sevkupno_resenie index vekje_izbrisani]
  (cond
    (empty? lst) false
    (not= -1 (sodrzi_soodvetna (first lst) sevkupno_resenie (najdi_impacted_indices index))) true
    :else (vozmozna_dodela (rest lst) sevkupno_resenie (+ index 1) vekje_izbrisani)

  ))

(vozmozna_dodela '((1)(2 3)( 1 3 6)( 1 4 5)) '((1)(2)( 1 3 6)( 1 4 5)) 0 '())


; dodeka ima dodeleni vrednosti gi propagira ogranicuvanjata, koga nema i e mozna dodela ja pravi dodelata i se taka dodeka nema nepropagirani i ne e vozmozna dodela
(defn nadezno [sevkupno_resenie vekje_izbrisani]
  (cond
    (site_edinecni_propagirani sevkupno_resenie vekje_izbrisani 0) sevkupno_resenie
    (not (site_edinecni_propagirani sevkupno_resenie vekje_izbrisani 0))(let [cekor (resavaj sevkupno_resenie vekje_izbrisani sevkupno_resenie 0)] (nadezno (first cekor) (second cekor)))
    :else (nadezno (napravi_dodela sevkupno_resenie sevkupno_resenie 0 vekje_izbrisani) vekje_izbrisani)

  ))






(defn make_vectors [lst]
  (into [](map (fn[x] (into [] x) ) lst)))

(def sudoku_1 (transform '((0 0 7 4 9 1 6 0 5) (2 0 0 0 6 0 3 0 9)(0 0 0 0 0 7 0 1 0) (0 5 8 6 0 0 0 0 4)( 0 0 3 0 0 0 0 9 0) (0 0 6 2 0 0 1 8 7)(9 0 4 0 7 0 0 0 2 ) ( 6 7 0 8 3 0 0 0 0) (8 1 0 0 4 5 0 0 0))))

(def sudoku_3 (transform '((0 4 5 0 0 1 0 0 0) (1 0 7 3 5 0 0 0 0)(0 0 6 0 0 4 2 0 0) (0 0 0 0 0 0 3 2 0)(6 0 0 0 2 0 0 0 0) (0 0 1 0 0 0 0 6 0)(0 9 0 0 0 0 6 0 0) ( 0 0 0 0 7 8 0 0 3) (0 0 0 6 0 0 5 0 0))))
;(def sudoku_3 (transform '((0 0 5 0 0 0 0 0 0) (1 0 0 0 0 0 0 0 0)(0 0 6 0 0 0 0 0 0) (0 0 0 0 0 0 0 2 0)(5 0 0 0 0 0 0 0 0) (0 0 4 0 0 0 0 0 0)(0 3 0 0 0 0 0 0 0) ( 0 0 0 0 7 0 0 0 0) (0 0 0 0 0 0 8 0 0))))
(def sudoku_4 (transform '((0 4 5 0 0 1 0 0 0) (1 0 7 3 5 0 0 0 0)(0 0 6 0 0 4 2 0 0) (0 0 8 0 0 0 3 2 0)(6 0 0 0 2 0 0 0 0) (0 0 1 0 3 0 0 6 0)(0 9 0 0 0 0 6 0 0) ( 0 0 0 0 7 8 0 0 3) (0 0 0 6 0 0 5 0 0))))
sudoku_1
; go dava resenieto vo potrebniot format
(defn solve [initial]
  (make_vectors (partition-all 9 (map (fn[x] (into #{} x)) (nadezno (flatten initial) '())))))


(solve sudoku_1)




; VIZUELIZACIJA NA SUDOKU
; gi ostavam so mnogu padding sekoj od elementite za da se osiguram deka kje bidat ok i koga kje imame golemi mnozestva, iako znam deka izgleda malku grdo koga imame precizno reshenie
(defn visualize-sudoku [sudoku]
  (println "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  (println "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  (doseq [row (range 9)]
    (let [dodaj_prazni (format "|| %20s %20s %20s  || %20s %20s %20s  || %20s %20s %20s  ||"
                               (get-in sudoku [row 0]) (get-in sudoku [row 1]) (get-in sudoku [row 2])
                               (get-in sudoku [row 3]) (get-in sudoku [row 4]) (get-in sudoku [row 5])
                               (get-in sudoku [row 6]) (get-in sudoku [row 7]) (get-in sudoku [row 8]))]
      (println dodaj_prazni)
      (println "")
      )
    (when (or (= row 5) (= row 2))
      (println "-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
      (println "-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")))
  (println "-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  (println "-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  )
(visualize-sudoku (solve sudoku_3))



