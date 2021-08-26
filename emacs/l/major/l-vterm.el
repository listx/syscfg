(use-package vterm
  :ensure t
  :config
  (add-hook 'vterm-mode-hook 'l/vterm-mode-hook)
  (add-hook 'vterm-mode-hook 'l/hide-trailing-whitespace))

(defun l/vterm-mode-hook ()
  (interactive)
  ; Our other configurations have bindings that take precedence over the
  ; vterm-mode-map. So we have to say, "no, when vterm is active, we have to
  ; send these keys into the underlying terminal application inside vterm, and
  ; refrain from interpreting them at the Evil/Emacs level". That's why we have
  ; these bindings below. The reason why we don't rebind *everything* is because
  ; anything that we don't explicitly bind at the Evil/Emacs level is already
  ; sent into vterm as-is.
  (evil-define-key 'insert vterm-mode-map
    ; ESC key. Sometimes we want to send the ESC key to vterm, and let vterm
    ; send the ESC key event to the underlying terminal app. Other times, we
    ; want to exit Evil's insert mode and re-eneter normal mode. These two
    ; scenarios each require a dedicated binding to disambiguate between these
    ; two differing intents.
    ;
    ; We make the default binding (plain "ESC" keypress) send ESC to vterm. This
    ; is because we assume that if we're in vterm we probably want to interact
    ; more closely with the underlying application than the outer Emacs session.

    ; Default intent: Send ESC to the underlying terminal application. If we're
    ; just running Zsh, this makes Zsh enter "vi" mode in Zsh.
    (kbd "<escape>") 'vterm-send-escape
    ; Alternative intent: Make "M-ESC" exit to Evil's normal state. This is an
    ; escape hatch to work around vterm-send-escape. For reasons we are too lazy
    ; to determine, we have to press this combination 2x in order for it to do
    ; our bidding (our first "ESC" probably gets eaten by another binding).
    (kbd "ESC <escape>") 'evil-normal-state

    ; Override default 'k' key that is bound to l/maybe-exit.
    (kbd "k")   'self-insert-command

    ; Override window movement keys.
    (kbd "C-j") 'vterm-send-C-j
    (kbd "C-k") 'vterm-send-C-k

    ; Override default "redo" binding.
    (kbd "C-r") 'vterm-send-C-r

    (kbd "C-t") 'vterm-send-C-x
    (kbd "C-x") 'vterm-send-C-x
    (kbd "RET") 'vterm-send-return
    (kbd "DEL") 'vterm-send-backspace))

(provide 'l-vterm)
