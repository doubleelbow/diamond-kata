(ns com.doubleelbow.diamond-kata
  (:gen-class))

(defprotocol Diamond
  (dmnd [this alphabet chr isc osc]))

(defn- index-of-if-not-int [alphabet elt]
  (if (integer? elt)
    elt
    (.indexOf alphabet elt)))

(defn- outside-spaces [alphabet current last]
  (let [current-index (index-of-if-not-int alphabet current)]
   (- (.indexOf alphabet last) current-index)))

(defn- inside-spaces [alphabet current]
  (let [current-index (index-of-if-not-int alphabet current)]
   (- (* 2 current-index) 1)))

(defn- raw-line [alphabet index last]
  (let [current (alphabet index)]
    {:char current
     :outside (outside-spaces alphabet index last)
     :inside (inside-spaces alphabet index)}))

(defn- upper [alphabet to-char]
  (let [index-range (range (inc (.indexOf alphabet to-char)))]
    (map #(raw-line alphabet % to-char) index-range)))

(defn- line [raw-line osc isc]
  (apply str
         (concat
          (repeat (:outside raw-line) osc)
          [(:char raw-line)]
          (when (pos? (:inside raw-line))
            (concat (repeat (:inside raw-line) isc) [(:char raw-line)]))
          (repeat (:outside raw-line) osc))))

(deftype Impl1 []
  Diamond
  (dmnd [this alphabet chr osc isc]
    (let [u (map #(line % osc isc) (upper alphabet chr))
          d (rest (reverse u))]
      (concat u d))))

(defn- up-to [alphabet chr]
  (take (inc (.indexOf alphabet chr)) alphabet))

(defn- chars-by-line [alphabet chr]
  (let [top (up-to alphabet chr)
        bottom (rest (reverse top))]
    (concat top bottom)))

(defn- line-with-chars [alphabet chr]
  (let [right-side (up-to alphabet chr)
        left-side (reverse (rest right-side))]
    (apply str (concat left-side right-side))))

(defn- signum [a]
  (if (= 0 a)
    a
    (if (< 0 a) 1 -1)))

(defn- spaceify [alphabet line chr osc isc]
  (apply str
         (map #(condp = (signum (- (.indexOf alphabet %) (.indexOf alphabet chr)))
                 0 chr
                 1 osc
                 -1 isc)
              line)))

(deftype Impl2 []
  Diamond
  (dmnd [this alphabet chr osc isc]
    (let [h (line-with-chars alphabet chr)
          v (chars-by-line alphabet chr)]
      (map #(spaceify alphabet h % osc isc) v))))

(defn diamond
  ([diamond-impl alphabet to-char]
   (diamond diamond-impl alphabet to-char "_" "-"))
  ([diamond-impl alphabet to-char outside-space-char inside-space-char]
   (dmnd diamond-impl alphabet to-char outside-space-char inside-space-char)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(comment
  (def alphabet [\a \b \c \d \e \f \g])

  (outside-spaces alphabet \a \c)

  (inside-spaces alphabet \a)

  (raw-line alphabet 0 \c)

  (upper alphabet \c)

  (line {:char \b :outside 1 :inside 1} "_" "-")
 
  (diamond (->Impl2) alphabet \g))
