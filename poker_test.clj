(ns user (:use clojure.test))


(deftest test_suit
  (is (= \H (suit "4H")))
  (is (= \S (suit "4S")))
  (is (= \D (suit "4D")))
  (is (= \C (suit "4C")))
  (is (= \H (suit "5H")))
  )

(deftest test_rank
  (is (= 13 (rank "KH")))
  (is (= 14 (rank "AS")))
  (is (= 10 (rank "10D")))
  (is (= 4 (rank "4C")))
  (is (= 5 (rank "5H")))
  )

(deftest test_pair
  (is (= true (pair? '("1H" "2H" "2H"))))
  (is (= false (pair? '("1H" "2H" "2H" "2H"))))
  )

(deftest test_three
  (is (= true (three-of-a-kind? '("1H" "2H" "2H" "2H"))))
  (is (= false (three-of-a-kind? '("1H" "2H" "2H" "3H"))))
  )

(deftest test_four
  (is (= true (four-of-a-kind? '("1H" "2H" "2H" "2H" "2J"))))
  (is (= false (four-of-a-kind? '("1H" "2H" "2H" "3H"))))
  )


(deftest test_flush
  (is (= true (flush? '("1H" "2H" "3H" "5H" "5H"))))
  (is (= false (flush? '("1H" "2H" "3D" "5H" "5H")))))

(deftest test_full_house
  (is (= true (full-house? '("1H" "1H" "1H" "5H" "5H"))))
  (is (= false (full-house? '("1H" "2H" "3D" "5H" "5H")))))

deftest test__two_pairs
  (is (= true (two-pairs? '("1H" "1H" "3H" "5H" "5H"))))
  (is (= false (two-pairs? '("1H" "2H" "3D" "5H" "5H"))))

(deftest test_straight
  (is (= true (straight? '("1H" "2H" "3H" "4H" "5H"))))
  (is (= false (straight? '("4H" "2H" "3D" "5H" "5H")))))

(deftest test_straight_flush
  (is (= true (straight-flush? '("1H" "2H" "3H" "4H" "5H"))))
  (is (= false (straight-flush? '("4H" "2H" "3D" "5H" "5H")))))

(deftest test_value
  (is (= 8 (value '("1H" "2H" "3H" "4H" "5H"))))
  (is (= 7 (value '("1H" "4H" "4H" "4H" "4H"))))
  (is (= 6 (value '("2H" "2H" "5H" "5H" "5H"))))
  (is (= 3 (value '("2H" "2D" "2D" "4H" "5H"))))
  (is (= 1 (value '("2H" "2D" "AD" "4H" "5H"))))
  )

(deftest test_kickers
  (is (= '(13 12 11 1)  (kickers ["KG" "QG" "AG" "10H" "JH"])))
  (is (= '(5 4 3 2 1) (kickers ["AH" "2D" "3C" "4D" "5S"])))
  (is (= '(14 5 7) (kickers ["5H" "AD" "5C" "7D" "AS"])))
  (is (= '(6 5 4 3 2) (kickers ["2H" "3S" "6C" "5D" "4D"])))
  (is (= '(6 5 3 2 1) (kickers ["2H" "3S" "6C" "5D" "1D"]))))

(deftest test_hk
  (is (= false (higher-kicker? '(8 5 2) '(8 7 3))))
  (is (= true (higher-kicker? '(8 5 2) '(8 4 3)))))
(deftest test_beats
  (is (= true (beats? ["KG" "QG" "AG" "10H" "JH"] ["5H" "AD" "5C" "7D" "AS"])))
  (is (= false (beats? ["KG" "QG" "AG" "10H" "JH"] ["KG" "QG" "AG" "10H" "JH"] )))
  )

(deftest test_winning_hand
  (is (= '(["KG" "QG" "AG" "10H" "JH"] ["KG" "QG" "AG" "10H" "JH"]) (winning-hand ["KG" "QG" "AG" "10H" "JH"] ["5H" "AD" "5C" "7D" "AS"]["1G" "QG" "AG" "10H" "JH"] ["5H" "AD" "5C" "7D" "2S"]) '(["KG" "QG" "AG" "10H" "JH"]["KG" "QG" "AG" "10H" "JH"])))
  (is (= '(["7h" "3g" "7h" "7h" "7h"] ["7h" "3g" "7h" "7h" "7h"]) (winning-hand ["7h" "3g" "7h" "7h" "7h"] ["3h" "3g" "3h" "7h" "7h"]["7h" "3g" "7h" "2h" "7h"])))
  (is (= '(["AH" "2H" "3H" "4H" "5H"] ["AH" "2H" "3H" "4H" "5H"] ["AH" "2H" "3H" "4H" "5H"]) (winning-hand ["AH" "2H" "3H" "5G" "5H"] ["AH" "2H" "3H" "4H" "5H"] ["AH" "2H" "3H" "4H" "5H"] ["AH" "2H" "3H" "4H" "5H"])))
  (is (= '(["AH" "2H" "3H" "4C" "5H"]) (winning-hand ["AD" "2H" "7D" "5G" "5H"] ["AH" "2D" "4H" "4H" "5H"] ["AH" "2H" "3H" "4C" "5H"] ["AH" "2H" "8C" "4C" "5H"])))
  (is (= '(["10H" "2H" "3H" "4C" "5H"])   (winning-hand ["AD" "2H" "7D" "5D" "5D"] ["AH" "2D" "4H" "4H" "5H"] ["10H" "2H" "3H" "4C" "5H"] ["AH" "2H" "8C" "4C" "5H"])))
  )




