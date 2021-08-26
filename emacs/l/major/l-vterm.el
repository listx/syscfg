(use-package vterm
  :ensure t
  :config
  (add-hook 'vterm-mode-hook 'l/vterm-mode-hook)
  (add-hook 'vterm-mode-hook 'l/hide-trailing-whitespace))

(defun l/vterm-mode-hook ()
  ; Unfortunately we have to rebind some obvious hotkeys here because they are
  ; overriden elsewhere. Somehow, evil-collection does not take precedence over
  ; them.
  (evil-define-key 'insert vterm-mode-map (kbd "C-r") 'vterm-send-C-r)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t") 'vterm-send-C-x)
  (evil-define-key 'insert vterm-mode-map (kbd "C-x") 'vterm-send-C-x)
  (evil-define-key 'insert vterm-mode-map (kbd "RET") 'vterm-send-return)
  (evil-define-key 'insert vterm-mode-map (kbd "DEL") 'vterm-send-backspace)

  )

(provide 'l-vterm)
