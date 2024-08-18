#!/usr/bin/env bb

;; Find long lines in Org mode files. We perform some transformations to check
;; the "final" line length (see "check-long-line").

(ns find-long-lines
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Though we set 100 here, ideally a line should be around 80 characters under
;; normal circumstances.
(def max-line-length 100)

(defn expand-tabs [line]
  (str/replace line "\t" (apply str (repeat 8 \space))))

(defn truncate-org-links [line]
  (str/replace line #"\[\[[^\]]+\]\[([^\]]+)\]\]" "$1"))

(defn truncate-org-other [line]
  (let [l (str/lower-case line)
        ignored-prefixes ["#+html_head: "
                          "#+header: "]
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

(defn truncate-long-urls
  "Some lines contains unwieldy URL links. Ignore such lines if the URL is
  roughly 60+ characters long."
  [line]
  (if (re-find #"https?://\S{60}" line)
    ""
    line))

(defn truncate-tangle-paths
  "Truncate lines that have tangle paths that/look/like/this. We don't care if
  such lines go over the 80-char limit."
  [line]
  (if (re-find #":tangle \S+$" line)
    ""
    line))

(defn truncate-long-paths
  "Truncate lines that have long paths in them."
  [line]
  (if (re-find #"[A-Za-z0-9-_./]{60}" line)
    ""
    line))

(defn truncate-long-contiguous-text
  "Sometimes a line contains some sort of data which cannot be broken up into
  multiple lines. Typically these are things like Docker image names with an
  included SHA-256 digest. Ignore lines with such contiguous text because there
  is no sensible way of breaking up such text (as they lack interspersed
  whitespace)."
  [line]
  (if (re-find #"\S{60}" line)
    ""
    line))

(defn truncate-quoted-strings
  "Ignore strings that are quoted and rather long. This usually happens for log
  statements and error messages, which we want to keep on a single line to help
  things like grep (i.e., line-oriented) search."
  [line]
  (-> line
      (str/replace #"\"([^\"]{60,})\"" "")
      (str/replace #"'([^']{60,})'" "")))

(defn check-long-line
  "Print the line if it exceeds max-line-length. Perform transformations
  before doing the length check."
  [filename index line]
  (->> line
       expand-tabs
       truncate-org-links
       truncate-org-table-rows
       truncate-org-other
       truncate-long-urls
       truncate-long-paths
       truncate-tangle-paths
       truncate-long-contiguous-text
       truncate-quoted-strings
       (#(when (< max-line-length (count %))
           (println (format "%s:%s:[%d] %s" filename (inc index) (count line) line))))))

(defn check-long-lines
  "Print out all lines that are over the max-line-length."
  [filename]
  (with-open [rdr (io/reader filename)]
    (doseq [[index line] (map-indexed vector (line-seq rdr))]
      (check-long-line filename index line))))

(defn -main [& args]
  (run! check-long-lines args))

;; Equivalent to Python's __name__ == "__main__" pattern; this only gets invoked
;; if we execute the file from the command line, not when we load it into a
;; REPL.
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
