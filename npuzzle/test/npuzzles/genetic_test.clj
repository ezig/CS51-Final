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

(deftest gen-chromosome-test
	(testing "gen-chromosome")
		(def gen-chromosome #'genetic/gen-chromosome)
		(is (= 3 (count (gen-chromosome 3))))
		(is (every? #(and (< % 1) (> % 0)) (gen-chromosome 5))))

(deftest gen-population-test
	(test "gen-population")
		(def gen-population #'genetic/gen-population)
		(is (= 0 (count (gen-population 0 30))))
		(is (= 3 (count (gen-population 3 30))))
		(is (= 100 (count ((gen-population 100 100) 0)))))

(deftest mutate-test
	(test "mutation")
		(def gen-chromosome #'genetic/gen-chromosome)
		(def mutate #'genetic/mutate)
		(is (= 5 (count (mutate (gen-chromosome 5)))))
		(is (= 5 (count (filter #(and (< % 1) (> % 0)) (gen-chromosome 5))))))

(deftest mutate-test
	(test "mutation")
		(def rand-crossover #'genetic/rand-crossover)
		(def gen-chromosome #'genetic/gen-chromosome)
		(is (every? #(and (< % 1) (> % 0)) (rand-crossover (gen-chromosome 5) (gen-chromosome 5)))))