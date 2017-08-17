; Read uim.el.
(autoload 'uim-mode "uim" nil t)

; Set Hiragana input mode at activating uim.
(setq uim-default-im-prop '("action_anthy_utf8_hiragana"))

; Display candidates inline (near where the actual text is) instead of below the
; modeline.
(setq uim-candidate-display-inline t)

;한글입니다.
(set-fontset-font "fontset-default"
  'korean-ksc5601
  '("Baekmuk Gulim" . "unicode-bmp"))

;日本語です。
(set-fontset-font "fontset-default"
  'japanese-jisx0208
  '("IPAGothic" . "unicode-bmp"))

(defun l/uim-mode ()
  "Toggle UIM minor mode, and also toggle #'cofi/maybe-exit
keybinding as it conflicts with Anthy input."
  (interactive)
  (uim-mode)
  (if uim-mode
    (progn
      (define-key evil-insert-state-map "k" nil)
      (define-key evil-insert-state-map (kbd "RET") 'newline)
      (define-key evil-insert-state-map (kbd "<S-backspace>") 'backward-delete-char-untabify)
      (define-key evil-insert-state-map (kbd "DEL") 'backward-delete-char-untabify))
    (progn
      (define-key evil-insert-state-map "k" #'l/maybe-exit)
      (define-key evil-insert-state-map (kbd "RET") 'kakapo-ret-and-indent)
      (define-key evil-insert-state-map (kbd "<S-backspace>") 'kakapo-upline)
      (define-key evil-insert-state-map (kbd "DEL") 'kakapo-backspace))))

(provide 'l-uim)
