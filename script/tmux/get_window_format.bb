#!/usr/bin/env bb
(ns get-window-format
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell]]
            [cheshire.core :as json]
            [clojure.string :as str]))
(defn get-window-type
  [window-name
   pane-current-command]
  (cond
    (str/starts-with? window-name "mnw->") :manually-named
    (or (str/starts-with? window-name "ssh")
        (= pane-current-command "ssh")) :ssh
    (= pane-current-command "zsh") :zsh
    (= pane-current-command "emacsclient") :emacs
    (= pane-current-command "claude") :claude
    :else :other-command))

(defn- get-tmux-pane-pwd-cached
  [window-id
   pane-id
   pane-current-path]
  (-> (shell {:out :string
              :err :string}
             "tmux_pane_pwd_cached.sh"
             window-id
             pane-id
             pane-current-path)
      :out
      str/trimr))

(defn- read-claude-sessions
  []
  (let [dir (fs/expand-home "~/.claude/sessions")]
    (when (fs/directory? dir)
      (keep #(try (json/parse-string (slurp (str %)))
                  (catch Exception _ nil))
            (fs/glob dir "*.json")))))

(defn claude-session-name
  "Return the name of the session in SESSIONS that runs in tmux pane PANE-ID."
  [sessions pane-id]
  (some (fn [{:strs [tmux name]}]
          (when (and tmux (str/ends-with? tmux (str "." pane-id)))
            name))
        sessions))

(defn get-modified-window-name
  [window-type
   window-name
   window-id
   pane-id
   pane-current-command
   pane-current-path
   host-short
   pane-title]
  (let [pwd #(get-tmux-pane-pwd-cached window-id pane-id pane-current-path)]
    (case window-type
      :manually-named (subs window-name 5)
      :ssh (if (str/starts-with? window-name "ssh") window-name "ssh")
      :zsh (pwd)
      :emacs (format "e [%s]" (if (or (str/blank? pane-title)
                                      (= host-short pane-title))
                                (pwd)
                                pane-title))
      :claude (format "c [%s]" (or (claude-session-name
                                    (read-claude-sessions)
                                    pane-id)
                                   (pwd)))
      pane-current-command)))

(defn get-window-style
  [window-type
   is-current-window?]
  (case window-type
    :manually-named
    (if is-current-window?
      {:style1 "#[bold bg=cyan fg=#343c48]"
       :style2 "#[bold bg=brightcyan fg=#343c48]"}
      {:style1 "#[bg=black fg=cyan]"
       :style2 "#[bg=black fg=cyan]"})

    :ssh
    (if is-current-window?
      {:style1 "#[bold bg=blue fg=#343c48]"
       :style2 "#[bold bg=brightblue fg=#343c48]"}
      {:style1 "#[bg=black fg=blue]"
       :style2 "#[bg=black fg=blue]"})

    :zsh
    (if is-current-window?
      {:style1 "#[bold bg=yellow fg=#343c48]"
       :style2 "#[bold bg=brightyellow fg=#343c48]"}
      {:style1 "#[bg=black fg=yellow]"
       :style2 "#[bg=black fg=yellow]"})

    (if is-current-window?
      {:style1 "#[bold bg=green fg=#343c48]"
       :style2 "#[bold bg=brightgreen fg=#343c48]"}
      {:style1 "#[bg=black fg=green]"
       :style2 "#[bg=black fg=green]"})))

(defn get-window-name-format
  [pane-current-command
    pane-current-path
    window-name
    window-id
    pane-id
    window-flags
    window-panes
    is-current-window
    host-short
    & [pane-title]]
  (let [window-flags-str (if (= "" window-flags)
                           " "
                           window-flags)
        window-weight (dec (Integer/parseInt window-panes))
        window-weight-str (if (pos? window-weight)
                            (str window-weight)
                            " ")
        window-type (get-window-type window-name pane-current-command)
        current-window? (= "1" is-current-window)
        {:keys [style1 style2]} (get-window-style window-type current-window?)
        name (get-modified-window-name window-type
                                       window-name
                                       window-id
                                       pane-id
                                       pane-current-command
                                       pane-current-path
                                       host-short
                                       pane-title)]
    (format "%s%s%s%s %s #[default]"
            style1
            window-flags-str
            window-weight-str
            style2
            name)))

(defn -main [& args]
  (when-not (<= 9 (count args) 10)
    (println (format "Need 9 or 10 arguments; got %d." (count args)))
    (System/exit 1))
  (println (apply get-window-name-format args)))
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
