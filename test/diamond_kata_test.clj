(ns diamond-kata-test
  (:require [clojure.string :as str]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer :all]
            [diamond-kata :refer :all]))

(defn- gen-char [alphabet]
  (gen/elements alphabet))

(defn- lines-count-equals-lines-length [diamond-impl alphabet]
  (prop/for-all [chr (gen-char alphabet)]
                (let [d (diamond diamond-impl alphabet chr)
                      l (count d)]
                  (every? #(= (.length %) l) d))))

(defn- vertical-symmetry [diamond-impl alphabet]
  (prop/for-all [chr (gen-char alphabet)]
                (let [d (diamond diamond-impl alphabet chr)]
                  (= d (reverse d)))))

(defn- horizontal-symmetry [diamond-impl alphabet]
  (prop/for-all [chr (gen-char alphabet)]
                (let [d (diamond diamond-impl alphabet chr)]
                  (every? #(= % (apply str (reverse %))) d))))

(defn- half-line-structure [line chr]
  (let [ci (str/index-of line chr)]
    (if ci
      {:left (subs line 0 ci)
       :char-position ci
       :right (subs line (inc ci))}
      {:left line
       :char-position ci
       :right ""})))

(defn- upper-left-structure [alphabet diamond]
  (let [half-point (int (Math/ceil (/ (count diamond) 2)))]
    (take half-point
          (map #(-> (half-line-structure (subs %2 0 half-point) (alphabet %1))
                    (assoc :line %1 :length half-point))
               (iterate inc 0)
               diamond))))

(defn- single-char-in-string? [s c]
  (every? #(= c %) s))

(defn- upper-left [diamond-impl alphabet osc isc]
  (prop/for-all [chr (gen-char alphabet)]
                (let [d (diamond diamond-impl alphabet chr osc isc)
                      ul (upper-left-structure alphabet d)]
                  (every? #(and (= (:length %) (+ (:char-position %) (:line %) 1))
                                (single-char-in-string? (:left %) osc)
                                (single-char-in-string? (:right %) isc))
                          ul))))

(def test-diamond-impl (->Impl1))
(def test-alphabet [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z])

(defspec it-s-a-square 100 (lines-count-equals-lines-length test-diamond-impl test-alphabet))

(defspec it-s-a-vertical-palindrom 100 (vertical-symmetry test-diamond-impl test-alphabet))

(defspec it-s-a-horizontal-palindrom 100 (horizontal-symmetry test-diamond-impl test-alphabet))

(defspec upper-left-props 100 (upper-left test-diamond-impl test-alphabet \_ \-))
