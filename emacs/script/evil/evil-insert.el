;;;; Insert state

(require 'evil-undo)
(require 'evil-states)
(require 'evil-repeat)
(require 'evil-visual)
(require 'evil-digraphs)

(evil-define-state insert
  "Insert state."
  :tag " <I> "
  :cursor (bar . 2)
  :message "-- INSERT --"
  :exit-hook (evil-cleanup-insert-state)
  (cond
   ((evil-insert-state-p)
    (add-hook 'pre-command-hook 'evil-insert-repeat-hook nil t)
    (unless evil-want-fine-undo
      (evil-start-undo-step)))
   (t
    (remove-hook 'pre-command-hook 'evil-insert-repeat-hook t)
    (setq evil-insert-repeat-info evil-repeat-info)
    (evil-set-marker ?^ nil t)
    (unless evil-want-fine-undo
      (evil-end-undo-step))
    (when evil-move-cursor-back
      (evil-adjust)))))

(defun evil-insert-repeat-hook ()
  "Record insertion keys in `evil-insert-repeat-info'."
  (setq evil-insert-repeat-info (last evil-repeat-info))
  (remove-hook 'pre-command-hook 'evil-insert-repeat-hook t))

(defun evil-cleanup-insert-state ()
  "Called when Insert state is about to be exited.
Handles the repeat-count of the insertion command."
  (when evil-insert-count
    (dotimes (i (1- evil-insert-count))
      (when evil-insert-lines
        (evil-insert-newline-below))
      (evil-execute-repeat-info (cdr evil-insert-repeat-info))))
  (when evil-insert-vcount
    (let ((line (nth 0 evil-insert-vcount))
          (col (nth 1 evil-insert-vcount))
          (vcount (nth 2 evil-insert-vcount)))
      (save-excursion
        (dotimes (v (1- vcount))
          (goto-char (point-min))
          (forward-line (+ line v))
          (when (or (not evil-insert-skip-empty-lines)
                    (not (integerp col))
                    (save-excursion
                      (end-of-line)
                      (>= (current-column) col)))
            (if (integerp col)
                (move-to-column col t)
              (funcall col))
            (dotimes (i (or evil-insert-count 1))
              (evil-execute-repeat-info
               (cdr evil-insert-repeat-info)))))))))

(defun evil-insert (count &optional vcount skip-empty-lines)
  "Switch to Insert state just before point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT-1 lines starting at the same column. If
SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of
the lines. This is the default behaviour for visual-state
insertion."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (when (evil-visual-state-p)
           (evil-visual-rotate 'upper-left)
           (when (memq (evil-visual-type) '(block line))
             (count-lines (evil-visual-beginning)
                          (evil-visual-end))))
         (evil-visual-state-p)))
  (if (and (called-interactively-p 'any)
           (evil-visual-state-p)
           (and (eq (evil-visual-type) 'line)))
      (evil-insert-line count vcount)
    (setq evil-insert-count count
          evil-insert-lines nil
          evil-insert-vcount (and vcount
                                  (> vcount 1)
                                  (list (line-number-at-pos)
                                        (current-column)
                                        vcount))
          evil-insert-skip-empty-lines skip-empty-lines)
    (evil-insert-state 1)))

(defun evil-append (count &optional vcount skip-empty-lines)
  "Switch to Insert state just after point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT-1 lines starting at the same column. If
SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of
the lines."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (when (evil-visual-state-p)
           (evil-visual-rotate (if (eq (evil-visual-type) 'block)
                                   'upper-right
                                 'upper-left))
           (when (memq (evil-visual-type) '(block line))
             (save-excursion
               ;; go to upper-left corner temporarily so
               ;; `count-lines' yields accurate results
               (evil-visual-rotate 'upper-left)
               (count-lines (evil-visual-beginning)
                            (evil-visual-end)))))))
  (if (and (called-interactively-p 'any)
           (evil-visual-state-p)
           (and (eq (evil-visual-type) 'line)))
      (evil-append-line count vcount)
    (unless (or (eolp) (evil-visual-state-p))
      (forward-char))
    (evil-insert count vcount skip-empty-lines)))

(defun evil-insert-resume (count)
  "Switch to Insert state at previous insertion point."
  (interactive "p")
  (when (evil-get-marker ?^)
    (goto-char (evil-get-marker ?^)))
  (evil-insert count))

(defun evil-insert-newline-above ()
  "Inserts a new line above point and places point in that line
w.r.t. indentation."
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (back-to-indentation))

(defun evil-insert-newline-below ()
  "Inserts a new line below point and places point in that line
w.r.t. indentation."
  (end-of-line)
  (newline)
  (back-to-indentation))

(defun evil-open-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode))
  (evil-insert-state 1))

(defun evil-open-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode))
  (evil-insert-state 1))

(defun evil-insert-line (count &optional vcount)
  "Switch to Insert state just before the first non-blank character
on the current line. The insertion will be repeated COUNT times."
  (interactive "p")
  (if evil-auto-indent
      (back-to-indentation)
    (beginning-of-line))
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount (and vcount
                                (> vcount 1)
                                (list (line-number-at-pos)
                                      #'evil-first-non-blank
                                      vcount)))
  (evil-insert-state 1))

(defun evil-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times."
  (interactive "p")
  (end-of-line)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount (and vcount
                                (> vcount 1)
                                (list (line-number-at-pos)
                                      #'end-of-line
                                      vcount)))
  (evil-insert-state 1))

(defun evil-insert-digraph (count digraph)
  "Insert the digraph DIGRAPH.
The insertion is repeated COUNT times."
  (interactive
   (let (count char1 char2 overlay string)
     (unwind-protect
         (progn
           (setq count (prefix-numeric-value current-prefix-arg)
                 overlay (make-overlay (point) (point)))
           ;; create overlay prompt
           (setq string "?")
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           ;; put cursor at (i.e., right before) the prompt
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (setq char1 (read-key))
           (setq string (string char1))
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (setq char2 (read-key)))
       (delete-overlay overlay))
     (list count (list char1 char2))))
  (let ((digraph (or (evil-digraph digraph)
                     ;; use the last character if undefined
                     (cadr digraph))))
    (dotimes (var count)
      (insert digraph))))

(defun evil-execute-in-normal-state ()
  "Execute the next command in Normal state."
  (interactive)
  (evil-normal-state)
  (if (eq this-command 'evil-execute-in-normal-state)
      (add-hook 'post-command-hook
                'evil-execute-in-normal-state nil t)
    (remove-hook 'post-command-hook
                 'evil-execute-in-normal-state t)
    (evil-insert-state)))

(defun evil-copy-from-above (arg)
  "Copy characters from preceding non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move backward."
  (interactive
   (cond
    ;; if a prefix argument was given, repeat it for subsequent calls
    ((and (null current-prefix-arg)
          (eq last-command 'evil-copy-from-above))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (insert (evil-copy-chars-from-line 1 (- arg))))

(defun evil-copy-from-below (arg)
  "Copy characters from following non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move forward."
  (interactive
   (cond
    ((and (null current-prefix-arg)
          (eq last-command 'evil-copy-from-below))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (insert (evil-copy-chars-from-line 1 arg)))

;; adapted from `copy-from-above-command' in misc.el
(defun evil-copy-chars-from-line (n num &optional col)
  "Return N characters from line NUM, starting at column COL.
NUM is relative to the current line and can be negative.
COL defaults to the current column."
  (interactive "p")
  (let ((col (or col (current-column))) prefix)
    (save-excursion
      (forward-line num)
      (when (looking-at "[[:space:]]*$")
        (if (< num 0)
            (skip-chars-backward " \t\n")
          (skip-chars-forward " \t\n")))
      (beginning-of-line)
      (move-to-column col)
      ;; if the column winds up in middle of a tab,
      ;; return the appropriate number of spaces
      (when (< col (current-column))
        (if (eq (preceding-char) ?\t)
            (let ((len (min n (- (current-column) col))))
              (setq prefix (make-string len ?\s)
                    n (- n len)))
          ;; if in middle of a control char, return the whole char
          (backward-char 1)))
      (concat prefix
              (buffer-substring (point)
                                (min (line-end-position)
                                     (+ n (point))))))))

;;; Completion

(evil-define-command evil-complete ()
  "Complete to the nearest preceding word.
Search forward if a match isn't found."
  :repeat change
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (dabbrev-expand nil)))

(evil-define-command evil-complete-line (&optional arg)
  "Complete a whole line."
  :repeat change
  (interactive "P")
  (let ((hippie-expand-try-functions-list
         '(try-expand-line
           try-expand-line-all-buffers)))
    (hippie-expand arg)))

(defun evil-paste-from-register (register)
  "Paste from REGISTER."
  (interactive
   (let ((overlay (make-overlay (point) (point)))
         (string "\""))
     (unwind-protect
         (progn
           ;; display " in the buffer while reading register
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (list (or evil-this-register (read-char))))
       (delete-overlay overlay))))
  (and (fboundp 'evil-paste-after)
       (evil-paste-after nil register)))

(provide 'evil-insert)

;;; evil-insert.el ends here
