#!/usr/bin/env bb
(ns find-long-lines
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
(def max-line-length 100)
(def token-threshold 60)
(def tab-width 8)
(defn expand-tabs [line]
  (str/replace line "\t" (apply str (repeat tab-width \space))))
(defn truncate-long-urls
  [line]
  (if (re-find (re-pattern (str "https?://\\S{" token-threshold "}")) line)
    ""
    line))
(defn truncate-long-paths
  "Truncate lines that have long paths in them."
  [line]
  (if (re-find (re-pattern (str "[A-Za-z0-9-_./]{" token-threshold "}")) line)
    ""
    line))
(defn truncate-long-contiguous-text
  [line]
  (if (re-find (re-pattern (str "\\S{" token-threshold "}")) line)
    ""
    line))
(defn truncate-org-metadata [line]
  (let [l (str/lower-case line)
        ignored-prefixes ["#+html_head: "
                          "#+header: "
                          "#+begin_src "]
        ignorable (some #(str/starts-with? l %) ignored-prefixes)]
    (if ignorable
      ""
      line)))
(defn truncate-org-table-rows [line]
  (if (and
       (str/starts-with? line "| ")
       (str/ends-with? line " |"))
    ""
    line))
(defn truncate-org-links [line]
  (str/replace line #"\[\[[^\]]+\]\[([^\]]+)\]\]" "$1"))
(defn truncate-quoted-strings
  [line]
  (let [rgx #(re-pattern (str % "([^" % "]{" token-threshold ",})" %))]
    (-> line
        (str/replace (rgx "\"") "")
        (str/replace (rgx "'") ""))))
(defn check-line
  [filename index line]
  (->> line
       expand-tabs
       truncate-long-urls
       truncate-long-paths
       truncate-long-contiguous-text
       truncate-org-metadata
       truncate-org-table-rows
       truncate-org-links
       truncate-quoted-strings
       (#(when (< max-line-length (count %))
           (println (format "%s:%s:[%d] %s"
                            filename
                            (inc index)
                            (count (expand-tabs line))
                            line))))))
(defn check-lines
  [filename]
  (with-open [rdr (io/reader filename)]
    (doseq [[index line] (map-indexed vector (line-seq rdr))]
      (check-line filename index line))))
(defn -main [& args]
  (run! check-lines args))
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
