;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(use-package! iedit
  :when (featurep! :completion vertico)
  :defer t
  :init
  ;; Fix conflict with embark.
  (setq iedit-toggle-key-default nil))

(setq user-full-name "Linus Arver"
      user-mail-address "linusarver@gmail.com")

(setq doom-theme 'zenburn)

;(after! (evil-org kakapo-mode)
;  (map! (:map (org-mode-map evil-org-mode-map)
;           :i [return] #'kakapo-ret-and-indent
;           :i "RET" #'kakapo-ret-and-indent)))

(setq org-directory "~/lo/note/")

(setq display-line-numbers-type nil)

(use-package! evil-escape
  :config
  (setq evil-escape-key-sequence "kj"))

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(map! :n "S" #'evil-snipe-s)
(map! :leader :desc "help" "H" help-map)
;(map! :leader :desc "notes" "N" help-map)

(map! :m "SPC" (cmd!! #'l/scroll-jump 10)
      :mn "DEL" (cmd!! #'l/scroll-jump -10))
(defun l/scroll-jump (cnt)
  "Scroll by CNT lines."
  (interactive "p")
  (forward-line cnt)
  (evil-scroll-line-to-center nil))
(map! :m "H" #'previous-buffer
      :m "L" #'next-buffer)

(map! :leader "h" #'l/split-vertically
      :leader "v" #'l/split-horizontally)
(defun l/split-vertically ()
  "Split window verically."
  (interactive)
  (split-window-vertically)
  (balance-windows))
(defun l/split-horizontally ()
  "Split window horizontally."
  (interactive)
  (split-window-horizontally)
  (balance-windows))
(map! :leader "q" #'l/quit-buffer)
(defun l/quit-buffer ()
  "Tries to escape the current buffer by closing it (or moving to a
non-auxiliary buffer if possible). Calls `l/gc-views' to handle any sort of
window management issues."
  (interactive)
  (let*
    (
      (original-bufname (buffer-name))
      (aux-buffer-rgx "^ *\*.+\*$")
      (is-aux-buffer (l/buffer-looks-like original-bufname '("^ *\*.+\*$")))
      (buffers (mapcar 'buffer-name (buffer-list)))
      (primary-buffers-count
        (length
          (seq-filter
            '(lambda (bufname) (not (string-match "^ *\*.+\*$" bufname)))
            buffers)))
      (primary-buffer-exists (> primary-buffers-count 0))
    )

    ; If we're on a magit-controlled buffer, do what magit expects and simulate
    ; pressing C-c C-c (with-editor-finish).
    (catch 'my-catch
      (progn
        (if (bound-and-true-p with-editor-mode)
          (if (buffer-modified-p)
            ; If there are any unsaved changes, either discard those changes or do
            ; nothing.
            (if (y-or-n-p "l/quit-buffer: Invoke (with-editor-cancel) to cancel the editing of this buffer?")
              (with-editor-cancel t)
              ; Use catch/throw to stop execution.
              (throw 'my-catch (message "l/quit-buffer: Aborting (doing nothing).")))
            (with-editor-finish t)))
        ; Close the current view (or exit the editor entirely), but only if we
        ; originally tried to close a non-"auxiliary" buffer. An "auxiliary"
        ; buffer is any buffer that is created in support of another major
        ; buffer. For example, if we open buffer "A", but then run `M-x
        ; describe-function' so that we're on a "*Help*" buffer, do NOT close
        ; the view (and exit emacs). In other words, such "auxiliary" buffers,
        ; when we want to quit from them, we merely want to just switch over to
        ; a primary (non-auxiliary) buffer.
        ;
        ; If we *only* have auxiliary buffers, then of course just quit.
        (if (and is-aux-buffer primary-buffer-exists)
          ; Cycle through previous buffers until we hit a primary
          ; (non-auxiliary) buffer.
          (progn
            (catch 'buffer-cycle-detected
              (while
                (string-match "^ *\*.+\*$" (buffer-name))
                ; Break loop if somehow our aux-buffer-rgx failed to account for all
                ; hidden/aux buffers and we are just looping over and over among the
                ; same list of actual auxiliary buffers.
                (if (string= original-bufname (buffer-name))
                  (throw 'buffer-cycle-detected
                    (message "l/quit-buffer: Buffer cycle detected among auxiliary buffers; invoking `l/gc-views'."))
                  (previous-buffer))))
              ; If we've broken the loop (due to a cycle), run (l/gc-views) as
              ; it is better than doing nothing.
              (l/gc-views))
          (l/gc-views))))))

; Either close the current window, or if only one windw, use the ":q" Evil
; command; this simulates the ":q" behavior of Vim when used with tabs to
; garbage-collect the current "view".
(defun l/gc-views ()
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
                  (lambda (bufname)
                    (not (l/buffer-looks-like bufname
                      '(
                      ; Do not delete buffers that may be open which are for git
                      ; rebasing and committing. This is in case these buffers
                      ; are open in other clients which may still be working on
                      ; these buffers.
                      "^COMMIT_EDITMSG"
                      "^git-rebase-todo"
                      ; This catches buffers like 'addp-hunk-edit.diff' which is
                      ; used during surgical edits of what to stage ('e' option
                      ; to the 'git add -p' command).
                      ".*hunk-edit.diff"
                      ; Don't delete system buffers buffers.
                      "^\*Messages\*"))))
                  (mapcar 'buffer-name (buffer-list))))))
          (evil-quit)) nil))))

(defun l/buffer-looks-like (bufname regexes)
  "Return t if the buffer name looks like any of the given regexes."
  (interactive)
  (eval (cons 'or (mapcar
    (lambda (rgx) (string-match rgx bufname)) regexes))))
;(after! evil-org
;  (map! (:map evil-org-mode-map
;           :mnv "TAB" #'other-window
;           :mnv "<backtab>" (cmd!! #'other-window -1))
;        :mnv "TAB" #'other-window
;        :mnv "<backtab>" (cmd!! #'other-window -1)))

(setq tab-bar-show t
      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-bar-separator "  ")
(map! :mi "C-l" #'tab-next
      :mi "C-h" #'tab-previous)
(map! :leader "h" #'l/split-vertically
      :leader "v" #'l/split-horizontally)
(map! :leader "N" #'tab-new)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
