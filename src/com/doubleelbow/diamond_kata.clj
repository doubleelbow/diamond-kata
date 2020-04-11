(ns com.doubleelbow.diamond-kata
  (:gen-class))

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
    {::char current
     ::outside (outside-spaces alphabet index last)
     ::inside (inside-spaces alphabet index)}))

(defn- upper [alphabet to-char]
  (let [index-range (range (inc (.indexOf alphabet to-char)))]
    (map #(raw-line alphabet % to-char) index-range)))

(defn- line [raw-line osc isc]
  (apply str
         (concat
          (repeat (::outside raw-line) osc)
          [(::char raw-line)]
          (when (pos? (::inside raw-line))
            (concat (repeat (::inside raw-line) isc) [(::char raw-line)]))
          (repeat (::outside raw-line) osc))))

(defn diamond
  ([alphabet to-char]
   (diamond alphabet to-char "_" "-"))
  ([alphabet to-char outside-space-char inside-space-char]
   (let [u (map #(line % outside-space-char inside-space-char) (upper alphabet to-char))
         d (rest (reverse u))]
     (concat u d))))

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

  (line {::char \b ::outside 1 ::inside 1} "_" "-")

  (diamond alphabet \g))
