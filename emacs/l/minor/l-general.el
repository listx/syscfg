(use-package general
  :config

  (general-create-definer l/leader-def
    :prefix ",")

  ; Global bindings.
  (l/leader-def
    :keymaps '(evil-normal-state-map evil-visual-state-map emacs)
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

    ; Spawn vterm (embedded terminal within emacs).
    "x" 'vterm

    "y" (lambda () (interactive) (l/copy-for-markdown nil))
    "Y" (lambda () (interactive) (l/copy-for-markdown t))
    ; Put current emacs frame into the background.
    "z" 'suspend-frame))

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

(defun l/quit-buffer ()
  "Tries to escape the current buffer by closing it (or moving to a
non-auxiliary buffer if possible). Calls `l/gc-views' to handle any sort of
window management issues."
  (interactive)
  (let*
    (
      (original-bufname (buffer-name))
      (aux-buffer-rgx "^ *\*.+\*$")
      (is-aux-buffer (l/buffer-looks-like original-bufname `(,aux-buffer-rgx)))
      (buffers (mapcar 'buffer-name (buffer-list)))
      (primary-buffers-count
        (length
          (seq-filter
            '(lambda (bufname) (not (string-match aux-buffer-rgx bufname)))
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
                (string-match aux-buffer-rgx (buffer-name))
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

(defun l/kill-this-buffer ()
  "Kill current buffer."
  (interactive)
  (if (bound-and-true-p with-editor-mode)
    (with-editor-cancel t)
    (kill-this-buffer)))

(defun l/kill-this-buffer! ()
  "Kill current buffer even if it is modified."
  (interactive)
  (set-buffer-modified-p nil)
  (l/kill-this-buffer))

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
