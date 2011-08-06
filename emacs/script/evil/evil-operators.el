;;;; Operator-Pending state

(require 'evil-undo)
(require 'evil-states)
(require 'evil-visual)
(require 'evil-insert)

(require 'rect)

(evil-define-state operator
  "Operator-Pending state"
  :tag " <O> "
  :cursor evil-half-cursor
  :enable (evil-operator-shortcut-map operator motion normal))

(evil-define-keymap evil-operator-shortcut-map
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t
  (setq evil-operator-shortcut-map (make-sparse-keymap))
  (evil-refresh-local-keymaps))

;; the half-height "Operator-Pending cursor" cannot be specified
;; as a static `cursor-type' value, since its height depends on
;; the current font size
(defun evil-half-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (let (height)
    ;; make `window-line-height' reliable
    (redisplay)
    (setq height (window-line-height))
    (setq height (+ (nth 0 height) (nth 3 height)))
    ;; cut cursor height in half
    (setq height (/ height 2))
    (setq cursor-type (cons 'hbar height))
    ;; ensure the cursor is redisplayed
    (force-window-update (selected-window))
    (redisplay)))

(defmacro evil-define-operator (operator args &rest body)
  "Define an operator command OPERATOR.

\(fn OPERATOR (BEG END ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  (let ((move-point t)
        (keep-visual nil)
        (whole-lines nil)
        (motion nil)
        arg doc beg end interactive key keys overriding-type type)
    ;; collect BEG, END and TYPE
    (setq args (delq '&optional args)
          beg (or (pop args) 'beg)
          end (or (pop args) 'end)
          type (pop args)
          args (when type `(&optional ,type ,@args))
          type (or type 'type))
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :motion)
        (setq motion arg)
        (unless motion
          (setq motion 'undefined)))
       ((eq key :keep-visual)
        (setq keep-visual arg))
       ((eq key :move-point)
        (setq move-point arg))
       ((eq key :type)
        (setq overriding-type arg))
       (t
        (setq keys (append keys (list key arg))))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(evil-define-command ,operator (,beg ,end ,@args)
       ,@(when doc `(,doc))
       ,@keys
       :keep-visual t
       :suppress-operator t
       (interactive
        (let* ((orig (point))
               (,beg orig)
               (,end orig)
               (state evil-state)
               range ,type
               evil-inhibit-operator)
          (unwind-protect
              (setq evil-inhibit-operator-value nil
                    evil-this-operator this-command
                    range (evil-operator-range
                           ',args ',motion ',overriding-type)
                    ,beg (evil-range-beginning range)
                    ,end (evil-range-end range)
                    ,type (evil-type range)
                    range (append (evil-range ,beg ,end ,type)
                                  (progn ,@interactive)))
            (setq orig (point)
                  evil-inhibit-operator-value evil-inhibit-operator)
            (if ,keep-visual
                (when (evil-visual-state-p)
                  (evil-visual-expand-region))
              (when (evil-visual-state-p)
                (evil-normal-state))
              (when (region-active-p)
                (evil-active-region -1)))
            (if (or ,move-point
                    (evil-visual-state-p state))
                (evil-visual-rotate 'upper-left ,beg ,end ,type)
              (goto-char orig)))
          range))
       (unwind-protect
           (let ((evil-inhibit-operator evil-inhibit-operator-value))
             (unless (and evil-inhibit-operator
                          (evil-called-interactively-p))
               ,@body))
         (setq evil-inhibit-operator-value nil)))))

;; this is used in the `interactive' specification of an operator command
(defun evil-operator-range (&optional return-type motion type)
  "Read a motion from the keyboard and return its buffer positions.
The return value is a list (BEG END) or (BEG END TYPE),
depending on RETURN-TYPE. Instead of reading from the keyboard,
a predefined motion may be specified with MOTION. Likewise,
a predefined type may be specified with TYPE."
  (let ((range (evil-range (point) (point)))
        command count modifier)
    (evil-save-echo-area
      (cond
       ;; Visual selection
       ((evil-visual-state-p)
        (setq range (evil-range (evil-visual-beginning)
                                (evil-visual-end)
                                (evil-visual-type))))
       ;; active region
       ((region-active-p)
        (setq range (evil-range (region-beginning)
                                (region-end)
                                (or evil-this-type 'exclusive))))
       (t
        ;; motion
        (evil-save-state
          (unless motion
            (evil-change-state 'operator)
            ;; Make linewise operator shortcuts. E.g., "d" yields the
            ;; shortcut "dd", and "g?" yields shortcuts "g??" and "g?g?".
            (let ((keys (nth 2 (evil-extract-count (this-command-keys)))))
              (setq keys (listify-key-sequence keys))
              (dotimes (var (length keys))
                (define-key evil-operator-shortcut-map
                  (vconcat (nthcdr var keys)) 'evil-line)))
            ;; read motion from keyboard
            (setq command (evil-read-motion motion)
                  motion (nth 0 command)
                  count (nth 1 command)
                  type (or type (nth 2 command))))
          (cond
           ((null motion)
            (setq quit-flag t))
           ((evil-get-command-property motion :suppress-operator)
            (setq quit-flag t))
           ((eq motion 'undefined)
            (setq motion nil))
           (evil-repeat-count
            (setq count evil-repeat-count
                  ;; only the first operator's count is overwritten
                  evil-repeat-count nil))
           ((or count current-prefix-arg)
            ;; multiply operator count and motion count together
            (setq count
                  (* (prefix-numeric-value count)
                     (prefix-numeric-value current-prefix-arg)))))
          (when motion
            (evil-with-state operator
              ;; calculate motion range
              (setq range (evil-motion-range
                           motion
                           count
                           type))
              (evil-set-marker ?. (evil-range-end range) t)))
          ;; update global variables
          (setq evil-this-motion motion
                evil-this-motion-count count
                type (evil-type range type)
                evil-this-type type))))
      (unless (or (null type) (eq (evil-type range) type))
        (evil-set-type range type)
        (evil-expand-range range)
        (evil-set-range-properties range nil))
      (unless return-type
        (evil-set-type range nil))
      range)))

(defun evil-motion-range (motion &optional count type)
  "Execute a motion and return the buffer positions.
The return value is a list (BEG END TYPE)."
  (let ((opoint   (point))
        (omark    (mark t))
        (omactive (and (boundp 'mark-active) mark-active))
        (obuffer  (current-buffer))
        (evil-motion-marker (move-marker (make-marker) (point)))
        range)
    (evil-save-transient-mark
      (evil-transient-mark 1)
      (unwind-protect
          (let ((current-prefix-arg count)
                ;; Store the type in global variable `evil-this-type'.
                ;; Motions can change their type during execution
                ;; by setting this variable.
                (evil-this-type (or type
                                    (evil-type motion 'exclusive))))
            (condition-case err
                (setq range (call-interactively motion))
              (error (prog1 nil
                       (setq evil-this-type 'exclusive
                             evil-write-echo-area t)
                       (message (error-message-string err)))))
            (cond
             ;; the motion returned a range
             ((evil-range-p range))
             ;; the motion made a Visual selection
             ((evil-visual-state-p)
              (setq range (evil-range (evil-visual-beginning)
                                      (evil-visual-end)
                                      (evil-visual-type))))
             ;; the motion made an active region
             ((region-active-p)
              (setq range (evil-range (region-beginning)
                                      (region-end)
                                      evil-this-type)))
             ;; default case: range from previous position to current
             (t
              (setq range (evil-expand-range
                           (evil-normalize
                            evil-motion-marker (point) evil-this-type)))))
            (unless (or (null type) (eq (evil-type range) type))
              (evil-set-type range type)
              (evil-expand-range range)
              (evil-set-range-properties range nil))
            range)
        ;; restore point and mark like `save-excursion',
        ;; but only if the motion hasn't disabled the operator
        (unless evil-inhibit-operator
          (set-buffer obuffer)
          (evil-move-mark omark)
          (goto-char opoint))
        ;; delete marker so it doesn't slow down editing
        (move-marker evil-motion-marker nil)))))

(defun evil-read-motion (&optional motion count type modifier)
  "Read a MOTION, motion COUNT and motion TYPE from the keyboard.
The type may be overridden with MODIFIER, which may be a type
or a Visual selection as defined by `evil-define-visual-selection'.
Return a list (MOTION COUNT [TYPE])."
  (let ((modifiers '((evil-visual-char . char)
                     (evil-visual-line . line)
                     (evil-visual-block . block)))
        command prefix)
    (unless motion
      (while (progn
               (setq command (evil-keypress-parser)
                     motion (pop command)
                     prefix (pop command))
               (when prefix
                 (if count
                     (setq count (string-to-number
                                  (concat (number-to-string count)
                                          (number-to-string prefix))))
                   (setq count prefix)))
               ;; if the command is a type modifier, read more
               (when (rassq motion evil-visual-alist)
                 (setq modifier
                       (or modifier
                           (car (rassq motion evil-visual-alist))))))))
    (when modifier
      (setq type (or type (evil-type motion 'exclusive)))
      (cond
       ((eq modifier 'char)
        ;; TODO: this behavior could be less hard-coded
        (if (eq type 'exclusive)
            (setq type 'inclusive)
          (setq type 'exclusive)))
       (t
        (setq type modifier))))
    (list motion count type)))

(defun evil-keypress-parser (&optional input)
  "Read from keyboard or INPUT and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument.
Both COUNT and CMD may be nil."
  (let ((input (append input nil))
        (inhibit-quit t)
        char cmd count digit seq)
    (while (progn
             (setq char (or (pop input) (read-event)))
             (when (symbolp char)
               (setq char (or (get char 'ascii-character) char)))
             ;; this trick from simple.el's `digit-argument'
             ;; converts keystrokes like C-0 and C-M-1 to digits
             (if (or (characterp char) (integerp char))
                 (setq digit (- (logand char ?\177) ?0))
               (setq digit nil))
             (if (keymapp cmd)
                 (setq seq (append seq (list char)))
               (setq seq (list char)))
             (setq cmd (key-binding (vconcat seq) t))
             (cond
              ;; if CMD is a keymap, we need to read more
              ((keymapp cmd)
               t)
              ;; numeric prefix argument
              ((or (memq cmd '(digit-argument))
                   (and (eq (length seq) 1)
                        (not (keymapp cmd))
                        count
                        (memq digit '(0 1 2 3 4 5 6 7 8 9))))
               ;; store digits in a string, which is easily converted
               ;; to a number afterwards
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               t)
              ;; catch middle digits like "da2w"
              ((and (not cmd)
                    (> (length seq) 1)
                    (memq digit '(0 1 2 3 4 5 6 7 8 9)))
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               ;; remove the digit from the key sequence
               ;; so we can see if the previous one goes anywhere
               (setq seq (nbutlast seq 1))
               (setq cmd (key-binding (vconcat seq)))
               t)
              ((eq cmd 'negative-argument)
               (unless count
                 (setq count "-")))
              ;; user pressed C-g, so return nil for CMD
              ((memq cmd '(keyboard-quit undefined))
               (setq cmd nil)))))
    ;; determine COUNT
    (when (stringp count)
      (if (string= count "-")
          (setq count nil)
        (setq count (string-to-number count))))
    ;; return command description
    (list cmd count)))

;;; Operator commands

(evil-define-operator evil-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive (list evil-this-register (evil-yank-handler)))
  (cond
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

(evil-define-operator evil-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive (list evil-this-register))
  (evil-yank beg end type register))

(defun evil-yank-characters (beg end &optional register yank-handler)
  "Saves the characters defined by the region BEG and END in the kill-ring."
  (let ((text (buffer-substring beg end)))
    (when yank-handler
      (setq text (propertize text 'yank-handler (list yank-handler))))
    (when register
      (set-register register text))
    (kill-new text)))

(defun evil-yank-lines (beg end &optional register yank-handler)
  "Saves the lines in the region BEG and END into the kill-ring."
  (let* ((text (buffer-substring beg end))
         (yank-handler (list (or yank-handler
                                 #'evil-yank-line-handler))))
    ;; Ensure the text ends with a newline. This is required
    ;; if the deleted lines were the last lines in the buffer.
    (when (or (zerop (length text))
              (/= (aref text (1- (length text))) ?\n))
      (setq text (concat text "\n")))
    (setq text (propertize text 'yank-handler yank-handler))
    (when register
      (set-register register text))
    (kill-new text)))

(defun evil-yank-rectangle (beg end &optional register yank-handler)
  "Stores the rectangle defined by region BEG and END into the kill-ring."
  (let ((lines (list nil)))
    (apply-on-rectangle #'extract-rectangle-line beg end lines)
    ;; We remove spaces from the beginning and the end of the next.
    ;; Spaces are inserted explicitly in the yank-handler in order to
    ;; NOT insert lines full of spaces.
    (setq lines (nreverse (cdr lines)))
    ;; `text' is used as default insert text when pasting this rectangle
    ;; in another program, e.g., using the X clipboard.
    (let* ((yank-handler (list (or yank-handler
                                   #'evil-yank-block-handler)
                               lines
                               nil
                               #'evil-delete-yanked-rectangle))
           (text (propertize (mapconcat #'identity lines "\n")
                             'yank-handler yank-handler)))
      (when register
        (set-register register text))
      (kill-new text))))

(defun evil-yank-line-handler (text)
  "Inserts the current text linewise."
  (let ((text (apply #'concat (make-list (or evil-paste-count 1) text)))
        (opoint (point)))
    (remove-list-of-text-properties
     0 (length text) yank-excluded-properties text)
    (cond
     ((eq this-command 'evil-paste-after)
      (end-of-line)
      (evil-move-mark (point))
      (newline)
      (insert text)
      (delete-char -1) ; delete the last newline
      (setq evil-last-paste
            (list 'evil-paste-after
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))
      (evil-move-mark (1+ (mark t))))
     (t
      (beginning-of-line)
      (evil-move-mark (point))
      (insert text)
      (setq evil-last-paste
            (list 'evil-paste-before
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))))
    (evil-exchange-point-and-mark)
    (back-to-indentation)))

(defun evil-yank-block-handler (lines)
  "Inserts the current text as block."
  (let ((count (or evil-paste-count 1))
        (col (if (eq this-command 'evil-paste-after)
                 (1+ (current-column))
               (current-column)))
        (current-line (line-number-at-pos (point)))
        (opoint (point)))
    (dolist (line lines)
      ;; concat multiple copies according to count
      (setq line (apply #'concat (make-list count line)))
      ;; strip whitespaces at beginning and end
      (string-match "^ *\\(.*?\\) *$" line)
      (let ((text (match-string 1 line))
            (begextra (match-beginning 1))
            (endextra (- (match-end 0) (match-end 1))))
        ;; maybe we have to insert a new line at eob
        (while (< (line-number-at-pos (point))
                  current-line)
          (goto-char (point-max))
          (newline))
        (setq current-line (1+ current-line))
        ;; insert text unless we insert an empty line behind eol
        (unless (and (< (save-excursion
                          (goto-char (line-end-position))
                          (current-column))
                        col)               ; nothing in this line
                     (zerop (length text))) ; and nothing to insert
          ;; if we paste behind eol, it may be sufficient to insert tabs
          (if (< (save-excursion
                   (goto-char (line-end-position))
                   (current-column))
                 col)
              (move-to-column (+ col begextra) t)
            (move-to-column col t)
            (insert (make-string begextra ? )))
          (remove-list-of-text-properties 0 (length text)
                                          yank-excluded-properties text)
          (insert text)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert (make-string endextra ? ))))
        (forward-line 1)))
    (setq evil-last-paste
          (list this-command
                evil-paste-count
                opoint
                (length lines)                   ; number of rows
                (* count (length (car lines))))) ; number of colums
    (goto-char opoint)
    (when (and (eq this-command 'evil-paste-after)
               (not (eolp)))
      (forward-char))))

(defun evil-delete-yanked-rectangle (nrows ncols)
  "Special function to delete the block yanked by a previous paste command."
  (let ((opoint (point))
        (col (if (eq last-command 'evil-paste-after)
                 (1+ (current-column))
               (current-column))))
    (dotimes (i nrows)
      (delete-region (save-excursion
                       (move-to-column col)
                       (point))
                     (save-excursion
                       (move-to-column (+ col ncols))
                       (point)))
      (unless (eobp) (forward-line)))
    (goto-char opoint)))

(defun evil-paste-before (count &optional register)
  "Pastes the latest yanked text before the cursor position."
  (interactive (list current-prefix-arg evil-this-register))
  (if (evil-visual-state-p)
      (evil-visual-paste count register)
    (evil-with-undo
      (let* ((text (if register
                       (evil-get-register register)
                     (current-kill 0)))
             (yank-handler (when (stringp text)
                             (car-safe (get-text-property
                                        0 'yank-handler text))))
             (opoint (point)))
        (when text
          (if (memq yank-handler '(evil-yank-line-handler
                                   evil-yank-block-handler))
              (let ((evil-paste-count count)
                    ;; for non-interactive use
                    (this-command 'evil-paste-before))
                (push-mark opoint t)
                (insert-for-yank text))
            ;; no yank-handler, default
            (push-mark opoint t)
            (dotimes (i (or count 1))
              (insert-for-yank text))
            (setq evil-last-paste
                  (list 'evil-paste-before
                        count
                        opoint
                        opoint          ; beg
                        (point)))       ; end
            (evil-exchange-point-and-mark)))
        ;; no paste-pop after pasting from a register
        (when register
          (setq evil-last-paste nil))))))

(defun evil-paste-after (count &optional register)
  "Pastes the latest yanked text behind point."
  (interactive (list current-prefix-arg evil-this-register))
  (if (evil-visual-state-p)
      (evil-visual-paste count register)
    (evil-with-undo
      (let* ((text (if register
                       (evil-get-register register)
                     (current-kill 0)))
             (yank-handler (when (stringp text)
                             (car-safe (get-text-property
                                        0 'yank-handler text))))
             (opoint (point)))
        (when text
          (if (memq yank-handler '(evil-yank-line-handler
                                   evil-yank-block-handler))
              (let ((evil-paste-count count)
                    (this-command 'evil-paste-after)) ; for non-interactive use
                (insert-for-yank text))
            ;; no yank-handler, default
            (unless (eolp) (forward-char))
            (push-mark (point) t)
            ;; TODO: Perhaps it is better to collect a list of all
            ;; (point . mark) pairs to undo the yanking for COUNT > 1.
            ;; The reason is that this yanking could very well use
            ;; `yank-handler'.
            (let ((beg (point)))
              (dotimes (i (or count 1))
                (insert-for-yank text))
              (setq evil-last-paste
                    (list 'evil-paste-after
                          count
                          opoint
                          beg           ; beg
                          (point)))     ; end
              (when (evil-normal-state-p)
                (evil-adjust)))))
        (when register
          (setq evil-last-paste nil))))))

(defun evil-visual-paste (count &optional register)
  "Paste over Visual selection."
  (interactive (list current-prefix-arg evil-this-register))
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text))))
    (evil-with-undo
      (when (evil-visual-state-p)
        ;; add replaced text to the kill-ring
        (unless register
          ;; if pasting from the kill-ring,
          ;; add replaced text before the current kill
          (setq kill-ring (delete text kill-ring)))
        (setq kill-ring-yank-pointer kill-ring)
        (evil-visual-rotate 'upper-left)
        (evil-delete (evil-visual-beginning)
                     (evil-visual-end)
                     (evil-visual-type))
        (unless register
          (kill-new text))
        (when (and (eq yank-handler 'evil-yank-line-handler)
                   (not (eq (evil-visual-type) 'line)))
          (newline))
        (evil-normal-state))
      (if (eobp)
          (evil-paste-after count register)
        (evil-paste-before count register)))))

;; TODO: if undoing is disabled in the current buffer, paste-pop won't
;; work. Although this is probably not a big problem, because usually
;; buffers where `evil-paste-pop' may be useful have undoing enabled.
;; A solution would be to temporarily enable undo when pasting and
;; store the undo information in a special variable that does not
;; interfere with `buffer-undo-list'.
(defun evil-paste-pop (count)
  "Replace the just-yanked stretch of killed text with a different stretch.
This command is allowed only immediatly after a `yank',
`evil-paste-before', `evil-paste-after' or `evil-paste-pop'.
This command uses the same paste command as before, i.e., when
used after `evil-paste-after' the new text is also yanked using
`evil-paste-after', used with the same paste-count argument.

The COUNT argument inserts the COUNTth previous kill.  If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (memq last-command
                '(evil-paste-after
                  evil-paste-before))
    (error "Previous command was not an evil-paste: %s" last-command))
  (unless evil-last-paste
    (error "Previous paste command used a register"))
  (evil-undo-pop)
  (goto-char (nth 2 evil-last-paste))
  (current-kill count)
  (setq this-command (nth 0 evil-last-paste))
  (funcall (nth 0 evil-last-paste) (nth 1 evil-last-paste)))

(defun evil-paste-pop-next (count)
  "Same as `evil-paste-pop' but with negative argument."
  (interactive "p")
  (evil-paste-pop (- count)))

(evil-define-operator evil-delete (beg end type register yank-handler)
  "Delete and save in kill-ring or REGISTER."
  (interactive (list evil-this-register (evil-yank-handler)))
  (evil-yank beg end type register yank-handler)
  (cond
   ((eq type 'block)
    (delete-rectangle beg end))
   ((and (eq type 'line)
         (= (point-max) end)
         (/= (point-min) beg))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end))))

(evil-define-operator evil-delete-line (beg end type register)
  "Delete to end of line."
  :motion evil-end-of-line
  (interactive (list evil-this-register))
  (evil-delete beg end type register))

(evil-define-operator evil-delete-char (beg end type register)
  "Delete next character."
  :motion evil-forward-char
  (interactive (list evil-this-register))
  (evil-delete beg end type register))

(evil-define-operator evil-delete-backward-char (beg end type register)
  "Delete previous character."
  :motion evil-backward-char
  (interactive (list evil-this-register))
  (evil-delete beg end type register))

(evil-define-operator evil-change (beg end type register yank-handler)
  "Delete region and change to insert state.
If the region is linewise insertion starts on an empty line.
If region is a block, the inserted text in inserted at each line
of the block."
  (interactive (list evil-this-register (evil-yank-handler)))
  (let ((nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg))))
        (bop (= beg (buffer-end -1)))
        (eob (= end (buffer-end 1))))
    (evil-delete beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (if (and eob (not bop))
          (evil-open-below 1)
        (evil-open-above 1)))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator evil-change-line (beg end type register)
  "Change to end of line."
  :motion evil-end-of-line
  (interactive (list evil-this-register))
  (evil-change beg end type register))

(evil-define-operator evil-change-whole-line (beg end type register)
  "Change whole line."
  :motion evil-line
  (interactive (list evil-this-register))
  (evil-change beg end type register))

(evil-define-operator evil-substitute (beg end type register)
  "Change a character."
  :motion evil-forward-char
  (interactive (list evil-this-register))
  (evil-change beg end type register))

(evil-define-command evil-use-register (register)
  "Use REGISTER for the next command."
  :keep-visual t
  (interactive (list (read-char)))
  (setq evil-this-register register))

(evil-define-command evil-record-macro (register)
  "Record a keyboard macro into REGISTER."
  :keep-visual t
  (interactive (list (unless evil-this-macro
                       (or evil-this-register (read-char)))))
  (cond
   (evil-this-macro
    (end-kbd-macro)
    (when last-kbd-macro
      (set-register evil-this-macro last-kbd-macro))
    (setq evil-this-macro nil))
   (t
    (setq evil-this-macro register)
    (start-kbd-macro nil))))

(evil-define-command evil-execute-macro (count macro)
  "Execute keyboard macro MACRO, COUNT times.
When called interactively, MACRO is read from a register."
  :keep-visual t
  (interactive
   (let (count macro register)
     (setq count (prefix-numeric-value current-prefix-arg)
           register (or evil-this-register (read-char)))
     (if (eq register ?@)
         (setq macro last-kbd-macro)
       (setq macro (evil-get-register register)))
     (list count macro)))
  (if (member macro '("" [] nil))
      (error "No previous macro")
    (execute-kbd-macro macro count)))

(evil-define-operator evil-upcase (beg end type)
  "Convert text to upper case."
  (if (eq type 'block)
      (evil-apply-on-block 'evil-upcase beg end)
    (upcase-region beg end)))

(evil-define-operator evil-downcase (beg end type)
  "Convert text to lower case."
  (if (eq type 'block)
      (evil-apply-on-block 'evil-downcase beg end)
    (downcase-region beg end)))

(evil-define-operator evil-invert-case (beg end type)
  "Invert case of text."
  (let (char)
    (if (eq type 'block)
        (evil-apply-on-block 'evil-invert-case beg end)
      (save-excursion
        (goto-char beg)
        (while (< beg end)
          (setq char (following-char))
          (delete-char 1 nil)
          (if (eq (upcase char) char)
              (insert-char (downcase char) 1)
            (insert-char (upcase char) 1))
          (setq beg (1+ beg)))))))

(evil-define-operator evil-invert-char (beg end type)
  "Invert case of character."
  :motion evil-forward-char
  (if (eq type 'block)
      (evil-apply-on-block 'evil-invert-case beg end)
    (evil-invert-case beg end)
    (when evil-this-motion
      (goto-char end))))

(evil-define-operator evil-rot13 (beg end type)
  "ROT13 encrypt text."
  (if (eq type 'block)
      (evil-apply-on-block 'evil-rot13 beg end)
    (rot13-region beg end)))

(evil-define-operator evil-join (beg end)
  "Join the selected lines."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (join-line 1))))

(evil-define-operator evil-join-whitespace (beg end)
  "Join the selected lines without changing whitespace.
\\<evil-normal-state-map>Like \\[evil-join], \
but doesn't insert or remove any spaces."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (move-end-of-line 1)
      (unless (eobp)
        (delete-char 1)))))

(evil-define-operator evil-fill (beg end)
  "Fill text."
  :move-point nil
  :type line
  (save-excursion
    (condition-case nil
        (fill-region beg end)
      (error nil))))

(evil-define-operator evil-indent (beg end)
  "Indent text."
  :type line
  (indent-region beg end)
  (back-to-indentation))

(evil-define-operator evil-shift-left (beg end)
  "Shift text to the left."
  :type line
  (indent-rigidly beg end (- evil-shift-width)))

(evil-define-operator evil-shift-right (beg end)
  "Shift text to the right."
  :type line
  (indent-rigidly beg end evil-shift-width))

(provide 'evil-operators)

;;; evil-operators.el ends here
