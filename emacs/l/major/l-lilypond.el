(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook 'l/lilypond-setup)

(defun l/lilypond-setup ()
  (turn-on-font-lock))

(provide 'l-lilypond)
