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

  ; For mode names that match the 'lang' in '#+begin_src lang', we don't need
  ; to provide an optional submode. But for those that don't match, we can do
  ; it like this:
  ;
  ;   (my-mmm-org-auto-class "fortran" 'f90-mode)
  ;   (my-mmm-org-auto-class "perl" 'cperl-mode)
  ;   (my-mmm-org-auto-class "shell" 'shell-script-mode)
  ;
  ; Adopted from http://jblevins.org/log/mmm.
  (defun l/mmm-org-auto-class (lang &optional submode)
    "Define a mmm-mode class for LANG in `org-mode' using SUBMODE.
    If SUBMODE is not provided, use `LANG-mode' by default."
    (let
      (
        (class (intern (concat "org-my-mmm-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^\\#\\+begin_src " lang "\n"))
        (back "^\\#\\+end_src$"))
      (mmm-add-classes (list (list class :submode submode :front front :back back)))
      (mmm-add-mode-ext-class 'org-mode nil class)))

  ; Add subclasses for #+begin_src blocks in org-mode.
  (mapc 'l/mmm-org-auto-class
    '(
    "awk"
    "bibtex"
    "c"
    "cpp"
    "css"
    "haskell"
    "html"
    "latex"
    "lisp"
    "makefile"
    "markdown"
    "python"
    "r"
    "ruby"
    "sql"
    "stata"
    "xml"))

  ; Re-fontify sub-mode portions when idle. The manual command for this is
  ; `mmm-parse-buffer'.
  (setq mmm-parse-when-idle 't))

(provide 'l-mmm-mode)
