#!/usr/bin/env bb
(ns prepare-commit-msg
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
(defn wrap-diff
  [s]
  (if-let [idx (str/index-of s "#\ndiff ")]
    (let [before (subs s 0 (+ 2 idx))
          after (subs s (+ 2 idx))
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
