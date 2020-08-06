(use-package elscreen
  :config
  (load "elscreen" "ElScreen" t)
  (elscreen-start)
  ; Hide elscreen numbers from the mode line.
  (setq elscreen-display-screen-number nil)
  ; Hide '[<->]' button for tab navigation.
  (setq elscreen-tab-display-control nil)
  ; Hide '[X]' button in the tab to kill the tab.
  (setq elscreen-tab-display-kill-screen nil))

(provide 'l-elscreen)
