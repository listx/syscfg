(add-hook 'sh-mode-hook 'l/sh-setup)

(defun l/sh-setup ()
  ; Disable checking for POSIX compliance.
  (setq flycheck-disabled-checkers '(sh-posix-bash))
  ; Auto-format shell files by fixing automatically-fixable things.
  (add-hook 'after-save-hook 'l/shellcheck-format nil t)
  ; Check with shellcheck. Flycheck uses shellcheck if it is installed.
  (flycheck-mode))

; This must be used as part of an *after-save-hook* (not before-save-hook)
; because we run shellcheck against the saved file (we could write the unsaved
; buffer to a separate file, save that, lint that, diff it, etc. to run this as
; part of a before-save-hook, but in this day and age of version control, saving
; files is the least of our concern). Plus, this avoids having to deal with
; temporary files.
(defun l/shellcheck-format ()
  (interactive)
  (let* (basedir filename patch)
    (setq basedir (file-name-directory buffer-file-truename))
    (setq basedir (if (string-match "^/:" basedir)
      ; Remove leading "/:" if found. This happens if we open a file with the
      ; "/:" disambiguating substring, as documented in
      ; https://www.gnu.org/software/emacs/manual/html_node/emacs/Quoted-File-Names.html#Quoted-File-Names.
      (substring basedir 2 nil)
      basedir))
    (setq filename (file-name-nondirectory buffer-file-truename))
    (setq patch (shell-command-to-string (concat
      "cd " basedir
      " && 2>/dev/null shellcheck -f diff " filename)))

    ; Only mutate the buffer if there is something to automatically fix.
    (when (not (string= "" patch))
      (progn
        ; Modify the file in-place, from outside Emacs.
        (shell-command (concat
          ; Change into the directory where the buffer (file) is located,
          ; because this makes the `-pnum' flag to the `patch' utility
          ; deterministic.
          "cd " basedir
          " && shellcheck -f diff " filename " | patch -p1"))
        (evil-edit nil)))))

(provide 'l-sh)
