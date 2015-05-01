(ns npuzzles.genetic-test
  (:require [clojure.test :refer :all]
            [npuzzles.puzzle :as puzzle :refer :all]
            [npuzzles.genetic :as genetic :refer :all]))

(deftest opposite-dir-test
	(testing "opposite-dir")
		(def opposite-dir #'genetic/opposite-dir)
		(is (= (opposite-dir :up) :down))
		(is (= (opposite-dir :down) :up))
		(is (= (opposite-dir :left) :right))
		(is (= (opposite-dir :right) :left)))

(deftest gen-chromosome
	(testing "gen-chromosome")
		(def gen-chromosome #'genetic/gen-chromosome)
		(is (= 3 (count (gen-chromosome 3)))))