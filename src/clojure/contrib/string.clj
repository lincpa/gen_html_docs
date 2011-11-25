(ns clojure.contrib.string
 (:refer-clojure :exclude (take replace drop butlast partition
                           contains? get repeat reverse partial)))

(defmacro dochars 
  "bindings => [name string]

  Repeatedly executes body, with name bound to each character in
  string.  Does NOT handle Unicode supplementary characters (above
  U+FFFF)."
  [bindings & body]
  (assert (vector bindings))
  (assert (= 2 (count bindings)))
  ;; This seems to be the fastest way to iterate over characters.
  `(let [#^String s# ~(second bindings)]
     (dotimes [i# (.Length s#)]
       (let [~(first bindings) (nth s# i#)]
         ~@body))))


(defn replace-re
  "Replaces all matches of re with replacement in s."
  [re replacement #^String s]
  (.Replace (re-matcher re s) replacement))

(defn #^String escape
  "Returns a new String by applying cmap (a function or a map) to each
   character in s.  If cmap returns nil, the original character is
   added to the output unchanged."
  [cmap #^String s]
  (let [buffer (System.Text.StringBuilder. (.Length s))]
    (dochars [c s]
      (if-let [r (cmap c)]
        (.Append buffer r)
        (.Append buffer c)))
    (.ToString buffer)))

(defn as-str
  "Like clojure.core/str, but if an argument is a keyword or symbol,
  its name will be used instead of its literal representation.

  Example:
     (str :foo :bar)     ;;=> \":foo:bar\"
     (as-str :foo :bar)  ;;=> \"foobar\" 

  Note that this does not apply to keywords or symbols nested within
  data structures; they will be rendered as with str.

  Example:
     (str {:foo :bar})     ;;=> \"{:foo :bar}\"
     (as-str {:foo :bar})  ;;=> \"{:foo :bar}\" "
  ([] "")
  ([x] (if (instance? clojure.lang.Named x)
         (name x)
         (str x)))
  ([x & ys]
     ((fn [#^StringBuilder sb more]
        (if more
          (recur (. sb  (append (as-str (first more)))) (next more))
          (str sb)))
      (new StringBuilder #^String (as-str x)) ys)))
