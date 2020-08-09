(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-hook 'org-mode-hook 'l/org-mode-hook)
  (add-hook 'org-agenda-mode-hook 'l/org-agenda-mode-hook)
  (add-hook 'after-save-hook 'l/org-mode-save-hook)
  (setq org-todo-keywords
    '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))
  ; Write timestamp when a TODO changes to DONE.
  (setq org-log-done t)
  ; Write additional timestamp each time a task is re-DEADLINE-d. Useful for
  ; seeing which tasks are incurring time cost overruns.
  (setq org-log-redeadline (quote time))
  ; Write additional timestamp each time a task is re-SCHEDULED-d.
  (setq org-log-reschedule (quote time))
  ; Make agenda show 2 weeks instead of 1.
  (setq org-agenda-span 'fortnight)
  ; Mark a TODO item as DONE.
  (evil-define-key 'normal org-mode-map "T" 'org-todo)
  ; Prefer Emacs mode for agenda view, because it has so many keybindings.
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  ; List of directories to use for agenda files. Each directory is searched
  ; recursively.
  (let*
    ((files (mapcan
      (lambda (dir) (directory-files-recursively dir "\\.org$"))
      (split-string (getenv "L_ORG_AGENDA_DIRS"))))
     (exclude-patterns (split-string (getenv "L_ORG_AGENDA_EXCLUDE_PATTERNS")))
     (reduced
       (seq-reduce
         (lambda (fs exclude-pattern)
           (seq-filter
             (lambda (f)
               (not (string-match-p (regexp-quote exclude-pattern) f)))
             fs))
         exclude-patterns
         files)))
    (setq org-agenda-files reduced))

  ; Disable visual line mode for agenda view because otherwise, tags get shown on
  ; the next line because line length calculations are thrown off.
  ; https://superuser.com/a/531670
  (add-hook 'org-agenda-mode-hook
    (lambda ()
      (visual-line-mode -1)
      (toggle-truncate-lines 1)))

  ; org-agenda: Add weekly review view.
  ; https://emacs.stackexchange.com/a/8163/13006
  (setq org-agenda-custom-commands
    '(("w" "Weekly review"
      agenda ""
      (
        (org-agenda-span 'week)
        (org-agenda-start-with-log-mode '(closed clock state))
        (org-agenda-skip-function
          '(org-agenda-skip-entry-if 'nottodo 'done))))))

  ; Org-babel settings (for evaluating code blocks).
  (org-babel-do-load-languages
    'org-babel-load-languages
    '(
      (C . t)
      (clojure . t)
      (haskell . t)
      (java . t)
      (python . t)
      (shell . t)
      (sql . t)))

  ; Use cider for evaluating clojure source code blocks. This requires starting
  ; up cider first, with `M-x cider-jack-in' before doing C-c C-c.
  (setq org-babel-clojure-backend 'cider)

  ; Disable source-code evaluation prompt. The prompt is enabled by default for
  ; security reasons, but disable it because I'm not downloading other people's
  ; org-mode files and running babel on them.
  (setq org-confirm-babel-evaluate nil)

  ; Allow single/double quote marks in inline '='-delimited verbatim formatting.
  ; see https://lists.gnu.org/archive/html/emacs-orgmode/2014-04/msg00199.html
  (setq org-emphasis-regexp-components
    '(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n" "." 1))
  ; Do not convert TAB characters in source code blocks into spaces.
  (setq org-src-preserve-indentation t)

  ; Use Unicode bullet character for bullets and lists.
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
    (0 (compose-region (match-beginning 1) (match-end 1) "•")))))

  (setq org-publish-project-alist
    '(
      ("eh"
        :base-directory "~/prog/elementary-haskell/"
        :publishing-directory "~/prog/elementary-haskell/public_html"
        :publishing-function org-html-publish-to-html
        :section-numbers t
        :with-toc t
        :headline-levels 4
        :html-preamble t
        :html-head "<link rel=\"stylesheet\"
          href=\"css/style.css\"
          type=\"text/css\"/>"
      )
      ("gv"
        :base-directory "~/prog/gv/doc/"
        :publishing-directory "~/prog/gv/doc/public_html"
        :publishing-function org-html-publish-to-html
        :section-numbers t
        :with-toc t
        :headline-levels 4
        :html-preamble t
        :html-head "<link rel=\"stylesheet\"
          href=\"css/style.css\"
          type=\"text/css\"/>"
      )
      ("day"
        :base-directory "~/org/day"
        :publishing-directory "~/org/day/public_html"
        :publishing-function org-html-publish-to-html
        :section-numbers t
        :with-toc nil
        :html-preamble nil
        :html-postamble nil
        :html-head "<link rel=\"stylesheet\"
          href=\"css/style.css\"
          type=\"text/css\"/>")))

  (use-package org-download
    :after org
    :config
    (if (string= (l/theme-name) "arjen-grey")
      ; Customizations for arjen-grey theme and org-mode.
      (progn
        ; Colorize custom TODO-like keywords for org-mode.
        (setq org-todo-keyword-faces '(
          ("TODO" . org-warning)
          ("IN-PROGRESS" . "yellow")
          ("WAITING" . "purple")
          ("DONE" . org-done)
          ("CANCELED" . (:foreground "orange" :weight bold))))
        (set-face-attribute 'org-todo nil :foreground "indian red" :weight 'bold)
        (set-face-attribute 'org-level-1 nil :foreground "pink" :weight 'bold)
        (set-face-attribute 'org-level-2 nil :foreground "aquamarine" :weight 'bold)
        (set-face-attribute 'org-level-3 nil :weight 'bold)
        (set-face-attribute 'org-level-4 nil :inherit 'org-level-7 :weight 'bold)
        (set-face-attribute 'org-level-5 nil :foreground "light cyan" :weight 'bold)
        (set-face-attribute 'org-level-6 nil :foreground "RosyBrown2" :weight 'bold)
        (set-face-attribute 'org-level-7 nil :weight 'bold)
        (set-face-attribute 'org-level-8 nil :weight 'bold)))
    (setq org-download-screenshot-method "scrot -os %s")
    (defun l/org-download-method (link)
      (let*
        (
          (filename
            (file-name-nondirectory
              (car (url-path-and-query (url-generic-parse-url link)))))
          ;; Create folder name with current buffer name, and place in root dir
          (dirname (concat "./image/"
            (replace-regexp-in-string " " "_"
            (downcase (file-name-base buffer-file-name)))))
          (filename-with-timestamp (format "%s-%s.%s"
            (file-name-sans-extension filename)
            (format-time-string org-download-timestamp)
            (file-name-extension filename))))
        (make-directory dirname t)
        (expand-file-name filename-with-timestamp dirname)))
    (setq org-download-method 'l/org-download-method)))

(defun l/org-mode-hook ()
  (modify-syntax-entry ?_ "w")
  ; Disable default orgmode hotkeys that interfere with our global hotkeys
  ; defined elsewhere.
  (define-key org-mode-map [(tab)] nil)
  (define-key org-mode-map (kbd "<S-iso-lefttab>") nil)
  (define-key org-mode-map (kbd "<backtab>") nil))

(defun l/org-agenda-mode-hook ()
  ; Remap some hotkeys.
  (define-key org-agenda-mode-map "H" 'evil-next-buffer)
  (define-key org-agenda-mode-map "L" 'evil-prev-buffer)
  (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-goto)
  (define-key org-agenda-mode-map (kbd "<S-iso-lefttab>") (lambda () (interactive) (other-window -1)))
  (define-key org-agenda-mode-map [(tab)] 'other-window))

(defhydra hydra-org (:foreign-keys warn)
  "org"
  ("a" org-agenda "org-agenda" :exit t)
  ("b" org-cycle-list-bullet "org-cycle-bullet-type")
  ("e" org-roam-find-file "org-roam-find-file" :exit t)
  ("E" org-roam "org-roam" :exit t)
  ("i" org-download-screenshot "org-download-screenshot" :exit t)
  ("I" org-download-yank "org-download-yank" :exit t)
  ("j" org-roam-jump-to-index "org-roam-jump-to-index" :exit t)
  ("l" org-toggle-link-display "org-toggle-link-display")
  ("M" mmm-parse-buffer "turn on mmm-mode")
  ("m" mmm-mode "turn on mmm-mode")
  ("o" l/sort-done-closed "sort DONE/CLOSED headings" :exit t)
  ("s" org-beamer-export-to-pdf "save beamer to pdf")
  ("w" org-publish-current-project "publish current project")
  ("u" org-roam-insert "org-roam-insert" :exit t)
  ("q" nil "exit" :exit t))

(general-define-key
  :keymaps 'org-mode-map
  :states '(normal)
  "\"" 'hydra-org/body
  "j" 'org-insert-heading-after-current
  ; Cycle section visibility level.
  "l" 'org-cycle
  "L" 'org-global-cycle
  ; Evaluate source code block.
  "z" 'org-ctrl-c-ctrl-c)

(evil-define-key 'normal org-mode-map (kbd "M-i") 'org-insert-link)
(evil-define-key 'insert org-mode-map (kbd "M-i") 'org-insert-link)
(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-open-at-point)
(evil-define-key 'normal org-mode-map (kbd "M-n") 'org-shiftright)
; Heading-based navigation.
(evil-define-key 'normal org-mode-map (kbd "M-k")
  'outline-previous-visible-heading)
(evil-define-key 'normal org-mode-map (kbd "M-j")
  'outline-next-visible-heading)
; Move items around, including child nodes.
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-demote-subtree)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-promote-subtree)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-move-subtree-up)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-move-subtree-down)

(evil-define-key 'normal org-mode-map (kbd "<f12>") 'org-html-export-to-html)

; Fold/expand all headings globally one level (use C-TAB) for expanding/folding
; a particular level.
; The 'iso-lefttab' just means 'tab'. It probably has something to do with the
; keyboard layout also.
(evil-define-key 'normal org-mode-map (kbd "<C-S-iso-lefttab>") 'org-shifttab)

(evil-define-key 'normal org-mode-map (kbd "M-p") 'l/org-paste-src)

(defun l/org-paste-src ()
  (interactive)
  (evil-append nil)
  (forward-line 1)
  (beginning-of-line)
  (insert "#+begin_src\n")
  (l/paste-X-primary)
  (if (not (string= "\n" (string (char-before (point)))))
    (insert "\n"))
  (insert "#+end_src\n")
  (evil-normal-state)
  (forward-line -1)
  (message nil))

; Add a blank line after a #+end_src line, to suppress awkward coloring of the
; header containing that block when we fold it. Inspired by
; https://www.reddit.com/r/emacs/comments/749t8a/keep_a_blank_line_after_a_code_block_in_orgmode/dnwvmlc/.
(defun l/org-mode-save-hook ()
  (when (string= major-mode "org-mode")
    (save-excursion
      (while (re-search-forward "^#\\+end_src\n\\([^\n]\\)" nil t)
        (replace-match "\n\\1" nil nil nil 1)))))

(defun l/sort-done-closed ()
  "Replace entire buffer with output from morg.py."
  (interactive)
  (let
    (
      (output (shell-command-to-string
        (concat
          "~/life/torg.sh sort_done_closed --input-file "
          (buffer-name))))
    )
    (if (string= output (buffer-string))
      (message "NOP (torg output matches buffer)")
      (progn
        (setf (buffer-string) output)
        ; Re-indent the buffer (buffer is not re-indented automatically by
        ; org-mode).
        (org-indent-indent-buffer)))))

(provide 'l-org)
