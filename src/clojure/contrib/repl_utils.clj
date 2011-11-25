(ns 
  #^{:author "Chris Houser, Christophe Grand, Stephen Gilardi, Michel Salim",
     :doc "Utilities meant to be used interactively at the REPL"}
  clojure.contrib.repl-utils)
 ;; (:import ;; (System.IO File StreamReader)
           ;; (java.io File LineNumberReader InputStreamReader PushbackReader)
           ;; (java.lang.reflect Modifier Method Constructor)
;;           (clojure.lang RT Compiler Compiler$C))
;;  (:require [clojure.contrib.string :as s]))
;;  (:use [clojure.contrib.seq :only (indexed)]
;;        [clojure.contrib.javadoc.browse :only (browse-url)]))


(defn get-source
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.
  
  Example: (get-source 'filter)"
  [x]
 ;; (when-let [v (resolve x)]
 ;;   (when-let [filepath (:file (meta v))]
 ;; ]]    (str filepath))))
      ;; (when-let [strm (.getResourceAsStream (RT/baseLoader) filepath)]
      ;;  (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
      ;;    (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
      ;;    (let [text (StringBuilder.)
      ;;          pbr (proxy [PushbackReader] [rdr]
      ;;                (read [] (let [i (proxy-super read)]
     ;;                            (.append text (char i))
     ;;                            i)))]
     ;;       (read (PushbackReader. pbr))
     ;;       (str text)))))))
)