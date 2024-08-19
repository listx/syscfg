#!/usr/bin/env bb

;; Find long lines in Org mode files. We perform some transformations to check
;; the "final" line length (see "check-long-line").

(ns find-long-lines
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn truncate-emphasis-markers
  "Some lines have words with emphasis markers in them. Detect such markers and
  remove them. This is a guesstimate as it has no knowledge of Org. It also does
  not bother with forward slashes (italics) because they are often used for file
  paths, and we do want to count the slashes for them."
  [line]
  (-> line
      (str/replace #"\b=([A-Za-z0-9-_ ]+)=\b" "$1")
      (str/replace #"\b~([A-Za-z0-9-_ ]+)~\b" "$1")
      (str/replace #"\b\*([A-Za-z0-9-_ ]+)\*\b" "$1")
      (str/replace #"\b_([A-Za-z0-9- ]+)_\b" "$1")))

(defn check-long-line
  "Print the line if it exceeds 80 chars (81 or more). Perform transformations
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
       truncate-emphasis-markers
       (#(when (< 80 (count %))
           (println (str filename ":" (inc index) ": " line))))))

(defn check-long-lines
  "Print out all lines that are over 80 chars in length."
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