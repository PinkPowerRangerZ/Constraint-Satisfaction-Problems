(ns user (:use clojure.test))
(defn suit [card]
  (second card)
  )
(suit "4H")



;treba da go popravam rank i rank_1 za 10
(def konverzija { \T  10, \J  11, \Q  12, \K  13, \A  14})
(def konverzija_1 { \T  10, \J  11, \Q  12, \K  13, \A  1})
(konverzija \K)
(defn rank[card]
  (cond
    (contains? konverzija (first card)) (konverzija (first card))
    :else (Integer/parseInt (apply str (reverse(rest(reverse card)))))))

(rank "3H")


(apply str (reverse(rest(reverse "10H"))))


;go mapira asot vo 1
(defn rank_1[card]
  (cond
    (contains? konverzija_1 (first card)) (konverzija_1 (first card))
    :else (Integer/parseInt(.toString(first card))))
  )
(rank_1 "Kh")


(rank "1h")

;dolzina na listata
(defn len [lst]
  (cond
    (empty? lst) 0
    :else (+ 1 (len (rest lst)))
  ))

;vrakja tocno ako najgolemata frekfencija e ista so num
(defn tocno [num lst]
  (= (reduce max (vals (frequencies (map (fn[x] (rank x))lst)))) num)
  )

;dali najcestata frekfencija e 2?
(defn pair? [hand]
  (tocno 2 hand)

)
(pair? '("1H" "2H" "2H" "2H"))



(defn three-of-a-kind? [hand]
  (tocno 3 hand)
  )
(three-of-a-kind? '("1H" "1H" "2H" "1H" "1H"))



(defn four-of-a-kind? [hand]
  (tocno 4 hand)
  )


;za sortirana lista vrakja dali e posledovatelna
(defn proveri_posledovatelnost [lst]
      (cond
    (empty? (rest lst)) true
    (not= (+ (first lst) 1) (second lst)) false
    :else (proveri_posledovatelnost (rest lst))
    ))

(proveri_posledovatelnost '(5 1 2 3))

;ako go mapirame asot na 1
(defn posledovatelni_1[lst]
(proveri_posledovatelnost (sort (map (fn[x] (rank_1 x)) lst) ))
  )
;ako go mapirame asot na 14
(defn posledovatelni[lst]
(proveri_posledovatelnost (sort (map (fn[x] (rank x)) lst) ))
  )

(posledovatelni '("AH" "2H" "3H" "4H" "5H"))



(sort (map (fn[x] (rank x)) ["KG" "QG" "AG" "10H" "JH"]) )

(posledovatelni ["KG" "QG" "AG" "10H" "JH"])


(posledovatelni '("1H" "2H" "3H" "5H" "5H"))

;vrakja true ako site se od ista boja
(defn same_color [lst]
  (= 1 (len(frequencies (map (fn[x] (suit x)) lst)))))
(same_color '("1H" "1H" "1H" "3H" "2f"))

;vrakja true ako imame kombinacija 3+2
(defn full-house? [hand]
  (let [frekfencii (into #{}(vals (frequencies (map (fn[x] (rank x))hand))))] (and (contains? frekfencii 2)(contains? frekfencii 3)))

  )
(full-house? '("1H" "1H" "1H" "2H" "2H"))


;vrakaj true ako se posledovatelni
(defn flush?[lst]
  ;mislam deka funcijata e definirana greshka vo domashnoto
  ;po 10toto branje ne moze da bide taka kako shto e definirana vo flush
  (same_color lst)

  )

;vrakja true ako ima dva para
(defn two-pairs? [hand]
  (let [frekfencii (into [] (vals (frequencies (map (fn[x] (rank x))hand))))] (and (contains? frekfencii 2)(= 3 (len frekfencii)))))
(two-pairs? '("1H" "1H" "3H" "3H" "4H"))


;vrakja true ako se posledovatelni
(defn straight? [hand]
  (or (posledovatelni_1 hand) (posledovatelni hand))
  )



(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))




(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0
    )

  )






;sortiranje

(defn najdi_frekfencii [hand]
  (frequencies (map (fn[x] (rank x)) hand))
  )

;vrakja lista od site mozni frekfencii sortirani opagjacki
(defn get_list_freq [hand]

(reverse (sort(into #{}(vals(najdi_frekfencii hand))))))

;ture ako elementot ima frekvencija n vo freq
(defn ima_frekfencija_n [freq element n]

  (= n (freq (rank element))
))

;vrakja lista od elementi cija vrednost se pojavuva so frekfencija n
(defn najdi_lista_n_cesti [freq n hand]
(filter (fn[x] (ima_frekfencija_n freq x n)) hand))

;samo pomosna fukncija da se najdat podlistite za slednata fuknkcija
(defn najdi_podlisti_1 [freq lst_frq hand]
  (cond
  (empty? lst_frq) ()
  :else (cons (najdi_lista_n_cesti freq (first lst_frq) hand) (najdi_podlisti_1 freq (rest lst_frq) hand)))
  )


;gi naogja podlistite spored frekfencii
(defn najdi_podlisti [hand]

(najdi_podlisti_1 (najdi_frekfencii hand) (get_list_freq hand)  hand))




(najdi_podlisti '("3h" "4h" "3h" "AH" "5H"))

;pomosnite gi sortiraat elementite od podnizite
(defn  pomosna_14 [element]
 (reverse(sort(into () (into #{}(map (fn[x] (rank x)) element))))))

(defn  pomosna_1 [element]
 (reverse(sort(into () (into #{}(map (fn[x] (rank_1 x)) element))))))





(najdi_podlisti '("2H" "3S" "6C" "5D" "4D"))


;vrakaj true ako listata e podredena
(defn podredena [lst]
  (cond
  (empty? (rest lst)) true
  (>= (first lst)(second lst)) (podredena (rest lst))
  :else false

    )
  )

(podredena '(5 4 3 2 1))

;ako se raboti za mapiranje na asot vo 1
(defn resenie_1 [hand]
(flatten(map (fn[x] (pomosna_1 x)) (najdi_podlisti hand))))

;ako se raboti za mapiranje na asot vo 14 sortiranite podlisti se izramnuvaat
(defn resenie_14 [hand]
  (flatten(map (fn[x] (pomosna_14 x)) (najdi_podlisti hand))))


(defn kickers [hand]
  (cond
  (and (straight? hand) (podredena (resenie_1 hand)))  (resenie_1 hand)
    :else (resenie_14 hand)
    )
  )

(apply str '(\1 \0))

(kickers  ["2H" "3S" "6C" "5D" "1D"])
(kickers  ["5H" "AD" "5C" "7D" "AS"])
(kickers  ["AH" "2D" "3C" "4D" "5S"])
(kickers ["KG" "QG" "AG" "10H" "JH"])





(defn higher-kicker? [ls1 ls2]
  (cond
  (empty? ls1) false
  (> (first ls1) (first ls2)) true
  (< (first ls1) (first ls2)) false
  :else (higher-kicker? (rest ls1) (rest ls2))
  )
  )



(higher-kicker? '(8 5 2) '(8 4 3))





(defn beats? [hand1 hand2]
  (cond
    (>(value hand1)(value hand2)) true
    (and (=(value hand1)(value hand2)) (higher-kicker? (kickers hand1) (kickers hand2))) true
    :else nil
  )
  )
(straight? ["KG" "QG" "AG" "10H" "JH"])



(value ["KG" "QG" "AG" "10H" "JH"])

(beats? ["KG" "QG" "AG" "10H" "JH"] ["5H" "AD" "5C" "7D" "AS"])



(defn proba  [& hands]
  (first (first hands)))
(proba '(4 5 6))


(kickers ["KG" "QG" "AG" "10H" "JH"])

(= nil (beats? ["KG" "QG" "AG" "10H" "JH"] ["KG" "QG" "AG" "10H" "JH"]))
(higher-kicker? (kickers ["KG" "QG" "AG" "10H" "JH"]) (kickers["KG" "QG" "AG" "10H" "JH"]))

;vrakja true ako se ednakvi
(defn ednakvi [hand1 hand2]
  (cond
    (and (= (beats? hand1 hand2) nil) (= (beats? hand2 hand1) nil)) true
    :else false

  ))
(ednakvi ["KG" "QG" "AG" "10H" "JH"] ["KG" "QG" "AG" "10H" "JH"])

;naogja najdobra hand
(defn najdi_najdobri [hands lst_najdobri]
  (cond
    (empty? hands) lst_najdobri
    (beats? (first hands) (first lst_najdobri)) (najdi_najdobri (rest hands) (list (first hands)))
    (ednakvi (first hands) (first lst_najdobri)) (najdi_najdobri (rest hands) (cons (first hands) lst_najdobri))
    :else (najdi_najdobri (rest hands) lst_najdobri)

  ))

(najdi_najdobri '(["KG" "QG" "AG" "10H" "JH"] ["KG" "QG" "AG" "10H" "JH"] ["5H" "AD" "5C" "7D" "AS"]["1G" "QG" "AG" "10H" "JH"] ["5H" "AD" "5C" "7D" "2S"]) '(["KG" "QG" "AG" "10H" "JH"]) )

(list ["KG" "QG" "AG" "10H" "JH"])


;vrakja pobednik
(defn winning-hand [ & hands]
  (cond
    (empty? hands) nil
    :else (najdi_najdobri  hands (list (first  hands)))
  ))
(winning-hand ["KG" "QG" "AG" "10H" "JH"] ["5H" "AD" "5C" "7D" "AS"]["1G" "QG" "AG" "10H" "JH"] ["5H" "AD" "5C" "7D" "2S"]) '(["KG" "QG" "AG" "10H" "JH"]["KG" "QG" "AG" "10H" "JH"])
(winning-hand ["7h" "3g" "7h" "7h" "7h"] ["3h" "3g" "3h" "7h" "7h"]["7h" "3g" "7h" "2h" "7h"])





(defn blabla [& hands]
  (first hands))
(blabla [3] [4] [5])





