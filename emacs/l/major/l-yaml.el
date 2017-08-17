(use-package yaml-mode
  :config
  (autoload 'yaml-mode "yaml-mode" "A major mode for YAML" t)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (evil-define-key 'insert yaml-mode-map (kbd "<tab>") 'kakapo-tab)
  (add-hook 'yaml-mode-hook 'l/yaml-setup))

(defun l/yaml-setup ()
  (kakapo-mode)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq evil-shift-width 2)
  (modify-syntax-entry ?_ "w")
  ; Disable YAML <backspace> binding.
  (define-key yaml-mode-map (kbd "<backspace>") nil))

(provide 'l-yaml)
