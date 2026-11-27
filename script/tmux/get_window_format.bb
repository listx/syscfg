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
    (or (str/starts-with? window-name "ssh")
        (= pane-current-command "ssh")) :ssh
    (= pane-current-command "zsh") :zsh
    (= pane-current-command "emacsclient") :emacs
    (= pane-current-command "claude") :claude
    (= pane-current-command "tig") :tig
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

(defn find-repo-root
  "Return the root of the Git repo that DIR is in, or nil."
  [dir]
  (some #(when (fs/exists? (fs/path % ".git"))
           (str %))
        (take-while some? (iterate fs/parent (fs/path dir)))))

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
      :tig (format "t [%s]" (get-tmux-pane-pwd-cached
                             window-id
                             pane-id
                             (or (find-repo-root pane-current-path)
                                 pane-current-path)))
      pane-current-command)))

(defn get-window-style
  [window-type
   is-current-window?]
  (case window-type
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

    :emacs
    (if is-current-window?
      {:style1 "#[bold bg=white fg=#343c48]"
       :style2 "#[bold bg=brightwhite fg=#343c48]"}
      {:style1 "#[bg=black fg=white]"
       :style2 "#[bg=black fg=white]"})

    :claude
    (if is-current-window?
      {:style1 "#[bold bg=red fg=#343c48]"
       :style2 "#[bold bg=brightred fg=#343c48]"}
      {:style1 "#[bg=black fg=red]"
       :style2 "#[bg=black fg=red]"})

    :tig
    (if is-current-window?
      {:style1 "#[bold bg=blue fg=#343c48]"
       :style2 "#[bold bg=brightblue fg=#343c48]"}
      {:style1 "#[bg=black fg=blue]"
       :style2 "#[bg=black fg=blue]"})

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
