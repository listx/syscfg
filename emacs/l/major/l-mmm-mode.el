(use-package mmm-mode
  :config

  ; By default MMM colorizes the submode sections to a different background
  ; color. We get rid of this behavior because it is very distracting and ugly.
  (set-face-background 'mmm-default-submode-face nil)

  ; Load MMM mode whenever there is an appropriate file. The "appropriateness"
  ; is determined by the calls to mmm-add-mode-ext-class.
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 1)

  ; Literate Haskell - mmm-mode. Adopted from
  ; https://wiki.haskell.org/Literate_programming#Multi-mode_support_in_Emacs.

  (mmm-add-classes
    '((l/literate-haskell-code
      :submode haskell-mode
      :front "^\\\\begin{code}"
      :back "^\\\\end{code}")))

  (mmm-add-mode-ext-class 'latex-mode "\\.lhs\\'" 'l/literate-haskell-code)

  ; Re-fontify sub-mode portions when idle. The manual command for this is
  ; `mmm-parse-buffer'.
  (setq mmm-parse-when-idle 't))

(provide 'l-mmm-mode)
