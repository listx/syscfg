(use-package haskell-mode
  :config
  ; Adopted from http://sequence.complete.org/node/365.
  (remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ; Remove the hard-coded 'literate-haskell-mode' activation for `.lhs' files that
  ; haskell-mode comes with. In exchange, enable LaTeX mode whenever we open up a
  ; `.lhs' file. Using mmm-mode, we will activate `haskell-mode' in the code
  ; sections.
  (setq auto-mode-alist
    (remove (rassoc 'literate-haskell-mode auto-mode-alist) auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . latex-mode))
  (add-hook 'literate-haskell-mode-hook 'latex-mode)

  ; The "Haskell-Cabal" mode that comes built-in with haskell-mode needs some
  ; manual tooth-removal to get it to submit and behave.
  (evil-define-key 'insert haskell-cabal-mode-map (kbd "<tab>") 'kakapo-tab)
  (evil-define-key 'insert haskell-cabal-mode-map (kbd "DEL") 'kakapo-backspace)
  (add-hook 'haskell-cabal-mode-hook 'l/haskell-cabal-setup)
  (add-hook 'haskell-mode-hook 'l/haskell-setup))
  (use-package hlint-refactor
    :after (flycheck haskell))

(defun l/haskell-cabal-setup ()
  (kakapo-mode)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq evil-shift-width 2))

(defun l/haskell-setup ()
  (interactive)
  (progn
    (flycheck-mode)
    ; Enable automated HLint suggestion application.
    (hlint-refactor-mode)))

(defun l/hs-literate-begend ()
  (interactive)
  (end-of-line)
  (insert "\n")
  (delete-blank-lines)
  (insert "\n\\begin{code}\n\n\\end{code}\n")
  (forward-line -2)
  (evil-append nil))

(defun l/hs-literate-endbeg ()
  (interactive)
  (end-of-line)
  (insert "\n")
  (delete-blank-lines)
  (insert "\\end{code}\n\n\n\n\\begin{code}\n")
  (forward-line -3)
  (evil-append nil))

(provide 'l-haskell-mode)
