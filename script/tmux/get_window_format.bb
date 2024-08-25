#!/usr/bin/env bb
(ns get-window-format
  (:require [clojure.string :as str]
            [babashka.process :refer [shell]]))
(defn get-window-type
  [window-name
   pane-current-command]
  (cond
    (str/starts-with? window-name "mnw->") :manually-named
    (str/starts-with? window-name "ssh") :ssh
    (= pane-current-command "zsh") :zsh
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
(defn get-modified-window-name
  [window-type
   window-name
   window-id
   pane-id
   pane-current-command
   pane-current-path]
  (case window-type
    :manually-named (subs window-name 5)
    :ssh window-name
    :zsh (get-tmux-pane-pwd-cached window-id pane-id pane-current-path)
    pane-current-command))
(defn get-window-style
  [window-type
   is-current-window?]
  (case window-type
    :manually-named
    (if is-current-window?
      {:style1 "#[bold bg=cyan fg=black]"
       :style2 "#[bold bg=brightcyan fg=black]"}
      {:style1 "#[bg=black fg=cyan]"
       :style2 "#[bg=black fg=cyan]"})

    :ssh
    (if is-current-window?
      {:style1 "#[bold bg=blue fg=black]"
       :style2 "#[bold bg=brightblue fg=black]"}
      {:style1 "#[bg=black fg=blue]"
       :style2 "#[bg=black fg=blue]"})

    :zsh
    (if is-current-window?
      {:style1 "#[bold bg=yellow fg=black]"
       :style2 "#[bold bg=brightyellow fg=black]"}
      {:style1 "#[bg=black fg=yellow]"
       :style2 "#[bg=black fg=yellow]"})

    (if is-current-window?
      {:style1 "#[bold bg=green fg=black]"
       :style2 "#[bold bg=brightgreen fg=black]"}
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
    is-current-window]
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
                                       pane-current-path)]
    (format "%s%s%s%s %s #[default]"
            style1
            window-flags-str
            window-weight-str
            style2
            name)))
(defn -main [& args]
  (when (not= 8 (count args))
    (println (format "Need exactly 8 arguments; got %d." (count args)))
    (System/exit 1))
  (println (apply get-window-name-format args)))
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
