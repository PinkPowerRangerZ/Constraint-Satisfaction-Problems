(ns user (:use clojure.test))
; gi rotira prackite
(defn rotiraj [lst]
  (cons (first (reverse lst)) (reverse (rest (reverse lst))) ))
(rotiraj '[1 4 2 3])

;proveruva dali dve pracki mozat da bidat del od sistemot
(defn kompatibilna [konf1 konf2]
  (cond
  (empty? konf1) true
  (= (first konf1) (first konf2)) false
  :else (kompatibilna (rest konf1) (rest konf2)))

  )


(last [1 2 3])
(kompatibilna '(1 2 3) '(3 7 5))


;proveruva dali dadena pracka e kompatibilna so tie shto i prethodat
(defn comp_w_first_n [pracka lst index]
  (cond
    (= index 0) true
    (kompatibilna pracka (first lst)) (comp_w_first_n pracka (rest lst) (- index 1))
    :else false
  ))
(comp_w_first_n '(7 7 7) '((4 5 6) (9 2 2) (1 2 3) (0 0 0)) 2)

(comp_w_first_n [1 2 2 2 5]  (vals pocetna) 1)




;clen
(defn member [lst element]
  (cond
    (empty? lst) false
    (= (first lst) element) true
    :else (member (rest lst) element)

  ))
;go dava ntiot element
(defn dadi_nth [lst n]
  (cond
    (= n 0) (first lst)
    :else (dadi_nth (rest lst) (- n 1))

  ))
(dadi_nth '((1)(4)(0)) 2)


; naogja kompatiblina kofiguracija ako takva postoi, vo sprotivno nil
(defn najdi_kompatibilna [momentalna lst index vrtenja probani]
  (cond
    ;(= momentalna pocetna) nil
    (= 5 vrtenja) nil
    (and (comp_w_first_n momentalna lst index) (not (member (dadi_nth probani index) momentalna))) momentalna
    :else (najdi_kompatibilna (rotiraj momentalna) lst index (+ vrtenja 1) probani)
  ))

(najdi_kompatibilna '(1 2 3) '((4 2 2)(1 2 3)) 1 0 '())



; dodava_vo nta podlista
(defn dodadi_vo_nta_podlista [element lst n]
  (cond
    (= n 0) (cons (cons element (first lst)) (rest lst))
    :else (cons (first lst) (dodadi_vo_nta_podlista element (rest lst) (- n 1)))
  ))
(dodadi_vo_nta_podlista '(a b c) '(((1) (3))((4) (4))(0) ) 1)


; ja smenuva sostojaba za pracka so daden index

(defn smeni [lst index so_element]
  (cond
    (= index 0) (cons so_element (rest lst) )
    :else (cons (first lst) (smeni (rest lst) (- index 1) so_element))

  ))


;ovde e cela funkcionalnost kako argumenti gi prima sostojbata na prackite vo momentot, index na momentalnata pracka, site isprobani konf za sekoja pracka do sega, i pocetnata sostojba na site pracki
;pocetnata sostojba na prackite ni treba za deterministichki da naogjame komaptibilna pracka sekogas
(defn solve [pracki_atm index_atm tried initial]

(cond
  ;nema nitu edna neisprobana kombinacija za pracka 0
  ;(= index_atm 1) (println tried)
  (and (= index_atm 0) (= nil (najdi_kompatibilna (initial index_atm) pracki_atm 0 0 tried) )) nil

  ;povolna kofiguracija i za poslednata pracka
  (and (= index_atm 4) (not= nil (najdi_kompatibilna (initial index_atm) pracki_atm 4 0 tried) )) (smeni pracki_atm index_atm (najdi_kompatibilna (initial index_atm) pracki_atm index_atm 0 tried))


  ;za momentalnata pracka nema povolna konfiguracija, vrati se nazad
  ;verojtno treba da se ischiste tried
  (= (najdi_kompatibilna (initial index_atm) pracki_atm index_atm 0 tried) nil) (solve pracki_atm (- index_atm 1) tried initial)

  ;za momentalnata pracka ima povolna konfiguracija, smeni ja sostojbata vo pracki i tried za ovoj index i prodolzi ponatamu


  ;:else  (dodadi_vo_nta_podlista (najdi_kompatibilna (pocetna index_atm) pracki_atm index_atm 0 tried) tried index_atm)
  ;:else (println (najdi_kompatibilna (pocetna index_atm) pracki_atm index_atm 0 tried))
  :else   (solve (smeni pracki_atm index_atm (najdi_kompatibilna (initial index_atm) pracki_atm index_atm 0 tried) ) (+ 1 index_atm)
                                                                                        (dodadi_vo_nta_podlista (najdi_kompatibilna (initial index_atm) pracki_atm index_atm 0 tried) tried index_atm) initial)
)
  )


;ova e samo pomoshna fuknkcija
(defn solution [initial]
  (solve (vals initial) 0 '(()()()()()) initial))

;---------------VIZUELIZACIJA--------
(defn colorize [n]
  (cond
    (= n 1) "\u001b[31m" ; red
    (= n 2) "\033[38;5;208m" ;orange
    (= n 3) "\u001b[33m" ; yellow
    (= n 5) "\u001b[34m" ; blue
    (= n 4) "\u001b[32m" ; green
    (= n "") "\u001b[0m" ; reset
    ))
(def boja { 1 "ORANGE", 2 " RED  ", 3 "YELLOW", 4 " GREEN", 5 " BLUE "})
(boja 1)

(defn print-matrix [matrix]
  (doseq [row matrix]
    (doseq [col row]
      (print (str (colorize col) (boja col) "\t")))
    (println (colorize ""))))

(print-matrix (solution pocetna_1))




