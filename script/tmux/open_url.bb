#!/usr/bin/env bb
(ns open-url
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell]]
            [clojure.string :as str]))
(defn extract-urls
  "Return the URLs in TEXT, the last one first, without duplicates."
  [text]
  (->> (re-seq #"https?://\S+" text)
       (map #(str/replace % #"[\])},.:;]+$" ""))
       reverse
       distinct))

(defn run-shell-arg
  "Quote S as one argument in a command for tmux's run-shell."
  [s]
  (str "'"
       (-> s
           (str/replace "'" "'\\''")
           (str/replace "#" "##"))
       "'"))

(defn pick-url
  "Let the user pick one of URLS with fzf. Return nil if they cancel."
  [urls]
  (let [{:keys [exit out]} (shell {:in (str/join "\n" urls)
                                   :out :string
                                   :continue true}
                                  "fzf" "--no-sort" "--prompt=open url> ")]
    (when (zero? exit)
      (str/trim out))))

(defn open-url
  [url]
  (let [opener (if (fs/which "xdg-open") "xdg-open" "open")]
    (shell "tmux" "run-shell" "-b"
           (str opener " " (run-shell-arg url) " >/dev/null 2>&1"))))

(defn -main [& _args]
  (let [urls (extract-urls (:out (shell {:out :string}
                                        "tmux" "capture-pane" "-p" "-J")))]
    (if (empty? urls)
      (shell "tmux" "display-message" "No URLs on screen")
      (when-let [url (pick-url urls)]
        (open-url url)))))
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
