#!/usr/bin/env bb
(ns prepare-commit-msg
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def max-file-lines 300)
(def head-lines 100)
(def tail-lines 50)
(def max-line-chars 500)

(defn truncate-line
  [line]
  (if (> (count line) max-line-chars)
    (str (subs line 0 max-line-chars)
         " [... " (- (count line) max-line-chars) " more characters]")
    line))

(defn truncate-file-diff
  [s]
  (let [lines (str/split s #"\n" -1)
        n (count lines)]
    (->> (if (> n max-file-lines)
           (concat (take head-lines lines)
                   [(str "[... " (- n head-lines tail-lines) " lines omitted ...]")]
                   (take-last tail-lines lines))
           lines)
         (map truncate-line)
         (str/join "\n"))))

(defn truncate-diff
  [s]
  (->> (str/split s #"(?m)^(?=diff --git )")
       (map truncate-file-diff)
       str/join))

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
          after (truncate-diff (subs s (+ offset idx)))
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
