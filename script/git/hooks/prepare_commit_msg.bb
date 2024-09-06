#!/usr/bin/env bb
(ns prepare-commit-msg
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
(def pats
  ["#\ndiff "
   "# Everything below it will be ignored.\ndiff "])

(defn- search-pat
  [s pat]
  [(str/index-of s pat)
   (str/index-of pat "diff")])

(defn wrap-diff
  [s]
  (if-let [[idx offset] (->> pats
                             (map #(search-pat s %))
                             (some (fn [[idx _ :as tup]]
                                     (when (some? idx) tup))))]
    (let [before (subs s 0 (+ offset idx))
          after (subs s (+ offset idx))
          new-parts [before
                     "#+begin_src diff\n"
                     after
                     "#+end_src\n"]]
      (str/join new-parts))
    s))
(defn wrap-diff-portion
  [file]
  (->> (slurp file)
       wrap-diff
       (spit file)))
(defn -main [& [file & rst]]
  (wrap-diff-portion file))
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
