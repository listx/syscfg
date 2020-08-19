(use-package general
  :config
  (setq general-default-keymaps '(evil-normal-state-map evil-visual-state-map))
  (setq general-default-prefix ",")

  ; Global bindings.
  (general-define-key
    "-" 'hydra-zoom/body
    "<tab>" 'hydra-window/body
    "<SPC>" 'magit-status
    ; Set line ending to UNIX.
    "\\" 'l/force-unix-line-endings

    "a" 'l/projectile-helm-ag
    "A" 'helm-ag

    "b" 'l/copy-file-name-to-clipboard

    ; Nox integration (comment/uncomment regions).
    "c" (lambda () (interactive) (l/addrem-comment t))
    "C" (lambda () (interactive) (l/addrem-comment nil))

    ; Kill buffer.
    "d" 'l/kill-this-buffer
    ; Kill current buffer without confirmation, even if modified.
    "D" 'l/kill-this-buffer!
    ; buffers list
    "e" 'helm-mini
    ; Find files (like dired, but better).
    "E" 'helm-find-files

    "f" 'helm-projectile
    "F" 'helm-projectile-switch-project

    "gj" 'git-gutter:revert-hunk
    "gn" 'git-gutter:next-hunk
    "gN" 'git-gutter:previous-hunk
    "gs" 'git-gutter:popup-hunk

    "G" 'helm-all-mark-rings
    "h" 'l/split-vertically

    "i" 'notmuch

    ; Literate haskell: write end/begin code blocks, and enter insert mode.
    ; TODO: make this just 1 funcion that does the right thing.
    "j" 'l/hs-literate-endbeg
    "J" 'l/hs-literate-begend

    "kj" 'hlint-refactor-refactor-at-point
    "kJ" 'hlint-refactor-refactor-buffer
    "kk" 'flycheck-list-errors
    "kn" 'flycheck-next-error
    "kN" 'flycheck-previous-error

    "m" 'hydra-magit/body
    ; New tab.
    "n" 'tab-new
    ; Ranger
    "o" 'ranger
    "q" 'l/quit-buffer
    ; Close current window, _even if modified_.
    "Q" (lambda () (interactive) (evil-quit t))

    ; Place selected region (or word-area under point in Normal mode) into
    ; buffer-wise search-and-replace interactive prompt.
    "s" 'l/replace-in-buffer
    "t" 'l/cycle-font
    ; Cycle through various themes.
    "T" 'l/cycle-theme

    ; See undo history in tree format (this will be opened in a new split
    ; window).
    "u" 'undo-tree-visualize

    "v" 'l/split-horizontally
    "w" 'save-buffer
    "W" 'l/save-buffer!

    ; Save and quit.
    "x" 'save-buffers-kill-emacs

    "y" (lambda () (interactive) (l/copy-for-slack nil))
    "Y" (lambda () (interactive) (l/copy-for-slack t))
    ; Put emacs into the background; only works in terminal mode.
    "Z" 'suspend-emacs))

(defun l/force-unix-line-endings ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))

; Adopted from
; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun l/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let
    (
      (filename
        (if (equal major-mode 'dired-mode)
          default-directory
          (buffer-file-name))))
    (progn
      (kill-new filename)
      (message "Clipboard: '%s'" filename))))

; Either close the current window, or if only one windw, use the ":q" Evil
; command; this simulates the ":q" behavior of Vim when used with tabs.
(defun l/quit-buffer ()
  "Vimlike ':q' behavior: close current window if there are split windows;
otherwise, close current tab."
  (interactive)
  (let
    ( (one-tab (= 1 (length (tab-bar-tabs))))
      (one-window (one-window-p)))
    (cond
      ; If current tab has split windows in it, close the current live
      ; window.
      ((not one-window) (delete-window) (balance-windows) nil)
      ; If there are multiple tabs, close the current one.
      ((not one-tab) (tab-bar-close-tab) nil)
      ; If there is only one tab, just try to quit (calling tab-bar-close-tab
      ; will not work, because if fails if there is only one tab).
      (one-tab
        (progn
          ; When closing the last frame of a graphic client, close everything we
          ; can. This is to catch graphical emacsclients that do not clean up
          ; after themselves.
          (if (display-graphic-p)
            (progn
              ; Minibuffers can create their own frames --- but they can linger
              ; around as an invisible frame even after they are deleted. Delete all
              ; other frames whenever we exit from a single visible daemon frame,
              ; because there is no point in keeping them around. If anything they
              ; can hinder detection of "is there a visible frame?" logic from the
              ; shell.
              (delete-other-frames)
              ; While we're at it, also close all buffers, because it's annoying to
              ; have things like Helm minibuffers and the like sitting around.
              (mapc
                'kill-buffer
                (seq-filter
                  (lambda (x)
                    (not (member x '(
                      "*Messages*"
                      ; Do not delete buffers that may be open which are for git
                      ; rebasing and committing.
                      "git-rebase-todo"
                      "COMMIT_EDITMSG"))))
                  (mapcar 'buffer-name (buffer-list))))))
          (evil-quit)) nil))))

(defun l/kill-this-buffer ()
  "Kill current buffer."
  (interactive)
  (if (or (mapcan (lambda (rgx) (string-match rgx (buffer-name)))
      ; List of regexes for buffer names that are "owned" by with-editor.
      '("^COMMIT_EDITMSG")))
    (with-editor-cancel t)
    (kill-this-buffer)))

(defun l/kill-this-buffer! ()
  "Kill current buffer even if it is modified."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

; Window-splitting functions.
(defun l/split-vertically ()
  "Split window verically."
  (interactive)
  (split-window-vertically)
  (balance-windows)
  (other-window 1))
(defun l/split-horizontally ()
  "Split window horizontally."
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1))

; http://stackoverflow.com/a/3217206/437583
(defun l/save-buffer! ()
  "Save current buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(provide 'l-general)
