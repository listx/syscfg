;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;; scim-bridge.el -- SCIM-Bridge client for GNU Emacs

;; Copyright (C) 2008, 2009, 2010 S. Irie

;; Author: S. Irie
;; Maintainer: S. Irie
;; Keywords: Input Method, i18n

(defconst scim-mode-version "0.8.2")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA  02110-1301 USA

;;; Commentary:

;; The Smart Common Input Method platform (SCIM) is an input
;; method (IM) platform containing support for more than thirty
;; languages (CJK and many European languages) for POSIX-style
;; operating systems including Linux and BSD.

;; scim-bridge.el is a SCIM-Bridge client for Emacs. This program
;; allows users on-the-spot style input with SCIM. The input statuses
;; are individually kept for each buffer, and prefix-keys such as C-x
;; and C-c can be used even if SCIM is active. So you can input
;; various languages fast and comfortably by using it.

;; This program is *not* a part of SCIM-Bridge.

;;
;; Installation:
;;
;; First, save this file as scim-bridge.el and byte-compile in
;; a directory that is listed in load-path.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'scim-bridge)
;;   (add-hook 'after-init-hook 'scim-mode-on)
;;
;; To disable XIM in Emacs, put the following in ~/.Xresources:
;;
;;   Emacs*useXIM: false
;;
;; and restart X session or execute a shell command:
;;
;;   xrdb ~/.Xresources
;;
;;
;; Here is the example of settings in .emacs:
;;
;;   (require 'scim-bridge)
;;   ;; Turn on scim-mode automatically after loading .emacs
;;   (add-hook 'after-init-hook 'scim-mode-on)
;;   ;; Use C-SPC for Set Mark command
;;   (scim-define-common-key ?\C-\s nil)
;;   ;; Use C-/ for Undo command
;;   (scim-define-common-key ?\C-/ nil)
;;   ;; Change cursor color depending on SCIM status
;;   (setq scim-cursor-color '("red" "blue" "limegreen"))
;;
;;
;; Note that this program requires GNU Emacs 22 or later, and
;; works only when Emacs is running under X session.
;;

;;; History:
;; 2010-06-26  S. Irie
;;         * Changed to reduce inter-process communication
;;         * Changed to show prediction window at cursor position in minibuffer
;;         * Modified to redraw preedit when switching frames showing same buffer
;;         * Bug fixes
;;         * Version 0.8.2
;;
;; 2010-05-29  S. Irie
;;         * Added internal option `scim-incompatible-major-modes'
;;         * Changed default value of `scim-cursor-type-for-candidate'
;;         * Changed default value of `scim-put-cursor-on-candidate'
;;         * Changed to exit when failing to open socket or register IMContext
;;         * Changed to restart when changing SCIM settings by setup utility
;;         * Unimportant changes
;;         * Bug fixes
;;         * Version 0.8.1
;;
;; 2010-04-23  S. Irie
;;         * Add files for building deb package
;;         * Add support for multi-display environment
;;         * Add support for INHERIT-INPUT-METHOD arg of `read-from-minibuffer' etc.
;;         * Add support for input focus redirection
;;         * Change maximum length of undo to 20 columns rather than 20 characters
;;         * Modify not to override descriptions of many keybindings
;;         * Modify not to override cursor color of auto-complete's fuzzy completion
;;         * Modify so that scim-mode is safely turned off when errors occur
;;         * Modify yasnippet support to work correctly for version 0.6
;;         * Reimplement functions for calculating pixel coordinates of point
;;         * Bug fixes
;;         * Version 0.8.0
;;
;; 2010-03-02  S. Irie
;;         * Add support for daemon mode
;;         * Add support for term.el (ansi-term)
;;         * Add support for undo-tree.el
;;         * Bug fixes
;;         * Version 0.7.5
;;
;; 2009-01-29  S. Irie
;;         * Add support for table.el
;;         * Available for yasnippet latest version (v0.5.9)
;;         * Correct self-insert-command simulation
;;         * Bug fixes
;;         * Modify description of installation
;;         * Version 0.7.4
;;
;; 2008-12-28  S. Irie
;;         * Cooperate with VI simulators (vi-mode, vip-mode, viper-mode)
;;         * Enhance IRC clients support
;;           - Don't clear buffer-undo-list when start of preediting
;;           - Add support for some clients (rcirc, Circe)
;;         * Cursor color can be specified for SCIM disabled state
;;         * Re-implement support for Japanese thumb shift typing method
;;         * Add option about cursor shape on isearch-mode
;;         * Change specification of `scim-cursor-type-for-candidate'
;;         * Bug fixes
;;         * Version 0.7.3
;;
;; 2008-12-06  S. Irie
;;         * Add support for incremental search mode
;;         * Add dynamic adjustment of input focus observation cycle
;;         * Add option for using minimum keymap
;;         * Add auto-restarting mechanism
;;         * Delete embedded perl script for UNIX domain socket connection
;;         * Bug fixes
;;         * Version 0.7.2
;;
;; 2008-10-20  S. Irie
;;         * Add option about behavior in minibuffer
;;         * Bug fixes
;;         * Version 0.7.1
;;
;; 2008-10-04  S. Irie
;;         * Add options about cursor color, shape, and location
;;         * Add functions for lcalization
;;         * Bug fixes
;;         * Version 0.7.0
;;
;; 2008-09-08  S. Irie
;;         * Change default value of `scim-mode-local' into t
;;         * Disable keymap in ebrowse-tree-mode
;;         * Available for ERC (IRC Client)
;;         * Bug fixes
;;         * Version 0.6.9
;;
;; 2008-09-05  S. Irie
;;         * Add treatment for read-only text
;;         * Bug fixes
;;         * Version 0.6.8
;;
;; 2008-07-26  S. Irie
;;         * Comment out debug codes
;;         * Some unimportant changes
;;         * Version 0.6.7
;;
;; 2008-06-12  S. Irie
;;         * Available for Xming multi-window mode
;;         * Version 0.6.6
;;
;; 2008-06-05  S. Irie
;;         * Change default value of `scim-common-function-key-list'
;;         * Bug fixes
;;         * Version 0.6.5
;;
;; 2008-06-01  S. Irie
;;         * Add/modify documentation strings
;;         * Version 0.6.4
;;
;; 2008-05-24  S. Irie
;;         * Solve kana-RO key problem
;;         * Bug fix
;;         * Version 0.6.3
;;
;; 2008-05-22  S. Irie
;;         * Modify treatment of kana-RO key
;;         * Bug fix
;;         * Version 0.6.2
;;
;; 2008-05-20  S. Irie
;;         * Bug fix
;;         * Version 0.6.1
;;
;; 2008-05-18  S. Irie
;;         * Manage meta-to-alt mapping
;;         * Bug fix
;;         * Version 0.6.0
;;
;; 2008-05-15  S. Irie
;;         * Experimental first release
;;         * Version 0.5.0
;;
;; 2008-05-12  S. Irie
;;         * Version 0.4.0
;;
;; 2008-05-08  S. Irie
;;         * Version 0.3.0
;;
;; 2008-05-06  S. Irie
;;         * Version 0.2.0
;;
;; 2008-04-20  S. Irie
;;         * Version 0.1.0

;; ToDo:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup scim nil
  "The Smart Common Input Method platform"
  :prefix "scim-"
  :group 'editing :group 'wp)

;; Basic settings
(defgroup scim-basic nil
  "Settings of operation, such as mode management and keyboard"
  :group 'scim)

(defcustom scim-mode-local t
  "If the value is non-nil, IMContexts are registered for each buffer
so that the input method of buffers can be toggled individually.
Otherwise, the input method is globally toggled."
  :type 'boolean
  :group 'scim-basic)

(defcustom scim-imcontext-temporary-for-minibuffer t
  "If non-nil, an one-time IMContext is used for a minibuffer so that
the minibuffer always starts with SCIM's input status off. This option
is effective only when the option `scim-mode-local' is active (non-nil)."
  :type 'boolean
  :group 'scim-basic)

(defun scim-customize-isearch (var value)
  (set var value)
  (if (and (fboundp 'scim-setup-isearch)
	   (bound-and-true-p scim-mode))
      (scim-setup-isearch)))

(defcustom scim-use-in-isearch-window t
  "If non-nil, SCIM can be used with isearch-mode. Otherwise, it can't.

Note that this option requires SCIM-Bridge version 0.4.13 or later."
  :set 'scim-customize-isearch
  :type 'boolean
  :group 'scim-basic)

(defun scim-customize-key (var value)
  (set var value)
  (if (and (fboundp 'scim-update-key-bindings)
	   (bound-and-true-p scim-mode))
      (scim-update-key-bindings var)))

(defcustom scim-use-minimum-keymap t
  "If non-nil, printable character events are not handled when SCIM is
off. This option is useful for avoiding conflict with other Emacs-Lisp
libraries.

NOTICE: Don't activate this option if SCIM-Bridge version in your system
is older than 0.4.13, otherwise SCIM cannot receive your inputs."
  :set 'scim-customize-key
  :type 'boolean
  :group 'scim-basic)

(defcustom scim-common-function-key-list
  '((control ".")
    (control ",")
    (control "<")
    (control ">")
    (control "/")
    (control " ")
    (shift " ")
    (control alt left)
    (control alt right)
    (control alt up)
    (control alt down)
    (zenkaku-hankaku)
    (henkan)
    (shift henkan)
    (muhenkan)
    (hiragana-katakana)
    (alt romaji)
    (f6)
    (f7)
    (f8)
    (shift f8)
    (f9)
    (f10)
    (f11)
    (f12)
    (kp-space)
    (kp-equal)
    (kp-multiply)
    (kp-add)
    (kp-separator)
    (kp-subtract)
    (kp-decimal)
    (kp-divide)
    (kp-0)
    (kp-1)
    (kp-2)
    (kp-3)
    (kp-4)
    (kp-5)
    (kp-6)
    (kp-7)
    (kp-8)
    (kp-9))
  "This list indicates which keystrokes SCIM takes over at both direct
insert mode and preediting mode. You can also add/remove the elements
using the function `scim-define-common-key'.
NOTICE: Don't set prefix keys in this option, such as ESC and C-x.
If you do so, operating Emacs might become impossible."
  :set 'scim-customize-key
  :type '(repeat (list :format "%v"
		       (set :format "%v"
			    :inline t
			    (const :format "M- " meta)
			    (const :format "C- " control)
			    (const :format "S- " shift)
			    (const :format "H- " hyper)
			    (const :format "s- " super)
			    (const :format "A- " alt))
		       (restricted-sexp :format "%v"
					:match-alternatives
					(symbolp stringp))))
  :group 'scim-basic)

(defcustom scim-preedit-function-key-list
  '((escape)
    (left)
    (right)
    (up)
    (down)
    (home)
    (end)
    (prior)
    (next)
    (return)
    (shift left)
    (shift right)
    (shift up)
    (shift down)
    (shift return)
    (tab)
    (iso-lefttab)
    (shift tab)
    (shift iso-lefttab)
    (backtab)
    (backspace)
    (delete)
    (kp-enter)
    (kp-tab))
  "This list indicates which keystrokes SCIM takes over when the
preediting area exists. You can also add/remove the elements using
the function `scim-define-preedit-key'."
  :set 'scim-customize-key
  :type '(repeat (list :format "%v"
		       (set :format "%v"
			    :inline t
			    (const :format "M- " meta)
			    (const :format "C- " control)
			    (const :format "S- " shift)
			    (const :format "H- " hyper)
			    (const :format "s- " super)
			    (const :format "A- " alt))
		       (restricted-sexp :format "%v"
					:match-alternatives
					(symbolp stringp))))
  :group 'scim-basic)

(defcustom scim-use-kana-ro-key nil
  "If you use Japanese kana typing method with jp-106 keyboard, turn
on (non-nil) this option to input a kana character `ろ' without pushing
the shift key.
 This option is made effectual by temporarily modifying the X-window
system's keyboard configurations with a shell command `xmodmap'."
  :set 'scim-customize-key
  :type 'boolean
  :group 'scim-basic)

(define-obsolete-variable-alias
  'scim-key-release-delay 'scim-simultaneous-pressing-time
  "scim-bridge.el version 0.7.3")

(defcustom scim-simultaneous-pressing-time nil
  "If you use Japanese thumb shift typing method on SCIM-Anthy,
specify the time interval (in seconds) which is corresponding to
`simultaneous pressing time' setting of SCIM-Anthy. Two keystrokes
within this time interval are sent to SCIM as a simultaneous keystroke."
  :type '(choice (const :tag "none" nil)
		 (number :tag "interval (sec.)" :value 0.1))
  :group 'scim-basic)

(defcustom scim-undo-by-committed-string nil
  "If the value is nil, undo is performed bringing some short
committed strings together or dividing the long committed string
within the range which does not exceed 20 columns. Otherwise, undo
is performed to each committed string."
  :type 'boolean
  :group 'scim-basic)

(defcustom scim-clear-preedit-when-unexpected-event nil
  "If the value is non-nil, the preediting area is cleared in the
situations that the unexpected event happens during preediting.
The unexpected event is, for example, that the string is pasted
with the mouse."
  :type 'boolean
  :group 'scim-basic)

;; Appearance
(defgroup scim-appearance nil
  "Faces, candidate window, etc."
  :group 'scim)

(defface scim-preedit-default-face
;  nil
  '((t :inherit underline))
  "This face indicates the whole of the preediting area."
  :group 'scim-appearance)

(defface scim-preedit-underline-face
  '((t :inherit underline))
  "This face corresponds to the text attribute `Underline' in SCIM
GUI Setup Utility."
  :group 'scim-appearance)

(defface scim-preedit-highlight-face
;  '((t :inherit (scim-preedit-underline-face highlight)))
  '((t :inherit highlight))
  "This face corresponds to the text attribute `Highlight' in SCIM
GUI Setup Utility."
  :group 'scim-appearance)

(defface scim-preedit-reverse-face
;  '((t :inherit scim-preedit-underline-face :inverse-video t))
  '((t :inverse-video t))
  "This face corresponds to the text attribute `Reverse' in SCIM
GUI Setup Utility."
  :group 'scim-appearance)

(defun scim-customize-cursor-color (var value)
  (set var value)
  (if (and (fboundp 'scim-set-cursor-color)
	   (bound-and-true-p scim-mode))
      (scim-set-cursor-color)))

(defcustom scim-cursor-color
  nil
  "If the value is a string, it specifies the cursor color applied
when SCIM is on. If a cons cell, its car and cdr are the cursor colors
which indicate that SCIM is on and off, respectively. If a list, the
first, second and third (if any) elements correspond to that SCIM is
on, off and disabled, respectively. The value nil means that the cursor
color is not controlled at all.

Note that this option requires SCIM-Bridge version 0.4.13 or later."
  :set 'scim-customize-cursor-color
  :type '(choice (const :tag "none (nil)" nil)
		 (color :tag "red" :format "red (%{sample%})\n" :value "red")
		 (color :tag "blue" :format "blue (%{sample%})\n" :value "blue")
		 (color :tag "green" :format "green (%{sample%})\n" :value "limegreen")
		 (color :tag "other" :value "red")
		 (cons  :tag "other (ON . OFF)"
			(color :format "ON: %v (%{sample%})  " :value "red")
			(color :format "OFF: %v (%{sample%})\n" :value "blue"))
		 (list  :tag "other (ON OFF)"
			(color :format "ON: %v (%{sample%})  " :value "red")
			(color :format "OFF: %v (%{sample%})  DISABLED: none\n"
			       :value "blue"))
		 (list  :tag "other (ON OFF DISABLED)"
			(color :format "ON: %v (%{sample%})  " :value "red")
			(color :format "OFF: %v (%{sample%})\n" :value "blue")
			(color :format "DISABLED: %v (%{sample%})\n"
			       :value "limegreen")))
  :group 'scim-appearance)

(defcustom scim-isearch-cursor-type
  0
  "This option specifies the cursor shape which is applied when
isearch-mode is active. If an integer 0, this option is not active so
that the cursor shape is not changed.
See `cursor-type'."
  :type '(choice (const :tag "don't specify (0)" 0)
		 (const :tag "use frame parameter" t)
		 (const :tag "don't display" nil)
		 (const :tag "filled box" box)
		 (const :tag "hollow box" hollow)
		 (const :tag "vertical bar" bar)
		 (cons :tag "vertical bar (specify width)"
		       (const :format "" bar)
		       (integer :tag "width" :value 1))
		 (const :tag "horizontal bar" hbar)
		 (cons :tag "horizontal bar (specify height)"
		       (const :format "" hbar)
		       (integer :tag "height" :value 1)))
  :group 'scim-appearance)

(defcustom scim-cursor-type-for-candidate
  'bar
  "This option specifies the cursor shape which is applied when the
preediting area shows conversion candidates. If an integer 0, this
option is not active so that the cursor shape is not changed.
See `cursor-type'."
  :type '(choice (const :tag "don't specify (0)" 0)
		 (const :tag "use frame parameter" t)
		 (const :tag "don't display" nil)
		 (const :tag "filled box" box)
		 (const :tag "hollow box" hollow)
		 (const :tag "vertical bar" bar)
		 (cons :tag "vertical bar (specify width)"
		       (const :format "" bar)
		       (integer :tag "width" :value 1))
		 (const :tag "horizontal bar" hbar)
		 (cons :tag "horizontal bar (specify height)"
		       (const :format "" hbar)
		       (integer :tag "height" :value 1)))
  :group 'scim-appearance)

(defcustom scim-put-cursor-on-candidate
  t
  "When the preediting area shows conversion candidates, the cursor
is put on the selected segment if this option is non-nil. Otherwise,
the cursor is put to the tail of the preediting area."
  :type 'boolean
  :group 'scim-appearance)

(defcustom scim-adjust-window-x-position
;  'gnome
  nil
  "This option specifies whether the position of candidate window
is adjusted so that the inline candidate and the candidates in that
window may just line up in the vertical direction. If the value is
`gnome', the adjustment will be done using the font size setting of
GNOME desktop environment. Otherwise, if the value is given as an
integer, that indicates the amount of the gap from normal position
by the number of pixels.
 This is not suitable for input method of the type to which the
candidate window is always displayed such as SCIM-pinyin (chinese),
because there is a possibility that the window hides the cursor when
the cursor is on the bottom of screen."
  :type '(choice (const :tag "use GNOME's font size" gnome)
		 (integer :tag "specify by pixel number" :value 24)
		 (const :tag "off" nil))
  :group 'scim-appearance)

(defcustom scim-prediction-window-position
  '(nil . nil)
  "(For Japanese IM only) The value should be given as (POS . ADJ).
If POS is non-nil, the forecast window is displayed under the head
of the preediting area. If the value of ADJ is non-nil, the horizontal
position of it is adjusted same as `scim-adjust-window-x-position'."
  :type '(cons
	       (choice :tag "Position"
		       (const :tag "Tail of preediting area" nil)
		       (const :tag "Head of preediting area" t))
	       (choice :tag "Adjustment"
		       (const :tag "same as conversion window" t)
		       (const :tag "off" nil)))
  :group 'scim-appearance)

(defcustom scim-mode-line-string " SCIM"
  "This variable specify a string that appears in the mode line
when scim-mode is active, and not otherwise. This string should be
a short string which starts with a space and represents scim-mode."
  :type 'string
  :group 'scim-appearance)

;; Advanced settings
(defgroup scim-expert nil
  "Advanced settings"
  :group 'scim)

(defcustom scim-focus-update-interval 0.3
  "The window focus is checked with this cycle measured in seconds.
When SCIM is off or input focus is in the other application, the slower
time cycle given by `scim-focus-update-interval-long' is used instead.

Note that this value is not used if SCIM-Bridge version in your system
is older than 0.4.13 or your window manager does not support a property
`_NET_ACTIVE_WINDOW'. In that case, `scim-focus-update-interval-long'
is used at all times."
  :type 'number
  :group 'scim-expert)

(defcustom scim-focus-update-interval-long 1.0
  "This value might be used as a slow time cycle for the observation
of input focus instead of `scim-focus-update-interval'.

See `scim-focus-update-interval' for details."
  :type 'number
  :group 'scim-expert)

(defcustom scim-kana-ro-x-keysym "F24"
  "When Japanese kana-RO key is used, this option specifies the
substitute KeySym name used in X window system for the key. This
program sets the substitute KeySym for backslash key to distinguish
it from yen-mark key."
  :set 'scim-customize-key
  :type 'string
  :group 'scim-expert)

(defcustom scim-kana-ro-key-symbol 'f24
  "When Japanese kana-RO key is used, this option specifies the event
corresponding to the substitute KeySym given in `scim-kana-ro-x-keysym'
as a symbol. This program sets the substitute KeySym for backslash key
to distinguish it from yen-mark key."
  :set 'scim-customize-key
  :type '(choice (symbol)
		 (const :tag "none" nil))
  :group 'scim-expert)

(defcustom scim-bridge-timeout 3.0
  "Specify the maximum waiting time for data reception from SCIM.
A floating point number means the number of seconds, otherwise an integer
the milliseconds."
  :type 'number
  :group 'scim-expert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System settings and constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar scim-debug nil)
(defvar scim-log-buffer "*scim-bridge-log*")

;; Definition of socket path
;;  e.g. /tmp/scim-bridge-0.3.0.socket-1000@localhost:0.0
(defvar scim-bridge-compat-version "0.3.0")
(defvar scim-bridge-socket-dir "/tmp/")
(defvar scim-bridge-socket-name "socket")
(defvar scim-bridge-name "scim-bridge")
(defvar scim-bridge-host-name "localhost")
(defvar scim-bridge-socket-path-common
  (concat scim-bridge-socket-dir scim-bridge-name "-"
	  scim-bridge-compat-version "." scim-bridge-socket-name "-"
	  (number-to-string (user-uid)) "@"
	  scim-bridge-host-name))

(define-obsolete-variable-alias
  'scim-bridge-x-display-name 'scim-bridge-x-display-substitute
  "scim-bridge.el version 0.7.5")

(defvar scim-bridge-x-display-substitute nil
  "Don't set this variable unless you want to explicitly specify the
X display number and screen number. Setting this variable makes
scim-mode unusable in multi-display environment.

If you set this variable, the value must be a string such as \":0.0\".")

(defvar scim-config-file "~/.scim/config"
  "The name of SCIM's configuration file, which is used to detect
the change of SCIM settings.")

(defvar scim-meta-key-exists
  (string< "" (shell-command-to-string "xmodmap -pke | grep '= Meta'"))
  "t is set in this variable if there is mata modifier key in the
keyboard. When automatic detection doesn't go well, please set the
value manually before scim-bridge.el is loaded.")

(defvar scim-tmp-buffer-name " *scim-bridge*"
  "This is working buffer name used for communicating with the agent.")

(defvar scim-incompatible-major-modes
  '(ebrowse-tree-mode w3m-mode)
  "List of symbols specifying major modes that keymaps of scim-mode are
deactivated.")

(make-obsolete-variable
 'scim-incompatible-mode-hooks 'scim-incompatible-major-modes
 "scim-bridge.el version 0.8.1")

(defvar scim-incompatible-mode-hooks
  nil
  "List of symbols specifying major mode hooks that keymaps of scim-mode
are deactivated when invoking these hooks.")

(define-obsolete-variable-alias
  'scim-undo-command-list 'scim-preedit-incompatible-commands
  "scim-bridge.el version 0.8.0")

(defvar scim-preedit-incompatible-commands
  '(undo undo-only redo undo-tree-undo undo-tree-redo)
  "List of symbols specifying commands which are disabled when preediting.")

(defvar scim-inherit-im-functions
  '(read-from-minibuffer read-string read-no-blanks-input completing-read)
  "List of symbols specifying functions which inherit input method.
If the function takes the argument INHERIT-INPUT-METHOD, input method
is inherited only when it's non-nil. Otherwise, input method is
unconditionally inherited.")

(defvar scim-reply-alist
  '(
    ;; Status
    ("imengine_status_changed"		. scim-imengine-status-changed)
    ("imcontext_registered"		. scim-imcontext-registered)
    ("preedit_mode_changed"		. scim-preedit-mode-changed)      ; Ignored
    ("focus_changed"			. scim-focus-changed)             ; Ignored
    ("cursor_location_changed"		. scim-cursor-location-changed)   ; Ignored
    ("key_event_handled"		. scim-key-event-handled)
    ("imcontext_deregister"		. scim-imcontext-deregister)      ; Ignored
    ("imcontext_reseted"		. scim-imcontext-reseted)         ; Ignored
    ;; Request
    ("forward_key_event"		. scim-forward-key-event)
    ("update_preedit"			. scim-update-preedit)
    ("set_preedit_shown"		. scim-set-preedit-shown)
    ("set_preedit_cursor_position"	. scim-set-preedit-cursor-position)
    ("set_preedit_string"		. scim-set-preedit-string)
    ("set_preedit_attributes"		. scim-set-preedit-attributes)
    ("set_commit_string"		. scim-set-commit-string)
    ("commit_string"			. scim-commit-string)
    ("get_surrounding_text"		. scim-get-surrounding-text)
    ("delete_surrounding_text"		. scim-delete-surrounding-text)
    ("replace_surrounding_text"		. scim-replace-surrounding-text)
    ("beep"				. scim-beep)
    ))

(defvar scim-ignored-signal-list
  '(scim-preedit-mode-changed
    scim-focus-changed
    scim-cursor-location-changed
    scim-imcontext-deregister
    scim-imcontext-reseted))

(defvar scim-modifier-alist
  `(
    ;; Keyboard
    (shift . "shift")
    (control . "control")
    (,(if scim-meta-key-exists 'alt 'meta) . "alt")
    (meta . "meta")
;    (super . "hyper")
    (super . "super")
    (hyper . "hyper")
    ;;
    (caps-lock . "caps_lock")
    (num-lock . "num_lock")
    (kana-RO . "kana_ro")
    ;; Mouse
;    (up . "up")
;    (down . "down")
;    (drag . "drag")
;    (click . "click")
;    (double . "double")
;    (triple . "triple")
    ))

(defvar scim-alt-modifier-alist
  '(
    (hiragana-katakana . romaji)
    (zenkaku-hankaku . kanji)
;    (henkan . mode-switch)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key code table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar scim-keycode-alist
  '(
    ;; *** Function keys ***********************************************
    (backspace . ?\xff08)
    (tab . ?\xff09)
    (linefeed . ?\xff0a)
    (clear . ?\xff0b)
    (return . ?\xff0d)
    (pause . ?\xff13)
;    (scroll-lock . ?\xff14)
;    (sys-req . ?\xff15)
    (escape . ?\xff1b)
    (delete . ?\xffff)
    ;; *** International & multi-key character composition *************
;    (multi-key . ?\xff20)
;    (codeinput . ?\xff37)
;    (singlecandidate . ?\xff3c)
;    (multiplecandidate . ?\xff3d)
;    (previouscandidate . ?\xff3e)
    ;; Japanese keyboard support ***************************************
    (kanji . ?\xff21)
    (muhenkan . ?\xff22)
;    (henkan-mode . ?\xff23)
    (henkan . ?\xff23)
    (romaji . ?\xff24)
    (hiragana . ?\xff25)
    (katakana . ?\xff26)
    (hiragana-katakana . ?\xff27)
    (zenkaku . ?\xff28)
    (hankaku . ?\xff29)
    (zenkaku-hankaku . ?\xff2a)
    (touroku . ?\xff2b)
    (massyo . ?\xff2c)
    (kana-lock . ?\xff2d)
    (kana-shift . ?\xff2e)
    (eisu-shift . ?\xff2f)
    (eisu-toggle . ?\xff30)
;    (kanji-bangou . ?\xff37)
;    (zen-koho . ?\xff3d)
;    (mae-koho . ?\xff3e)
    ;; *** Cursor control & motion *************************************
    (home . ?\xff50)
    (left . ?\xff51)
    (up . ?\xff52)
    (right . ?\xff53)
    (down . ?\xff54)
    (prior . ?\xff55)
;    (page-up . ?\xff55)
    (next . ?\xff56)
;    (page-down . ?\xff56)
    (end . ?\xff57)
    (begin . ?\xff58)
    ;; *** Misc Functions **********************************************
    (select . ?\xff60)
    (print . ?\xff61)
    (execute . ?\xff62)
    (insert . ?\xff63)
    (undo . ?\xff65)
    (redo . ?\xff66)
    (menu . ?\xff67)
    (find . ?\xff68)
    (cancel . ?\xff69)
    (help . ?\xff6a)
    (break . ?\xff6b)
;    (mode-switch . ?\xff7e)
;    (num-lock . ?\xff7f)
    ;; *** Keypad ******************************************************
    (kp-space . ?\xff80)
    (kp-tab . ?\xff89)
    (kp-enter . ?\xff8d)
    (kp-f1 . ?\xff91)
    (kp-f2 . ?\xff92)
    (kp-f3 . ?\xff93)
    (kp-f4 . ?\xff94)
    (kp-home . ?\xff95)
    (kp-left . ?\xff96)
    (kp-up . ?\xff97)
    (kp-right . ?\xff98)
    (kp-down . ?\xff99)
    (kp-prior . ?\xff9a)
;    (kp-page-up . ?\xff9a)
    (kp-next . ?\xff9b)
;    (kp-page-down . ?\xff9b)
    (kp-end . ?\xff9c)
    (kp-begin . ?\xff9d)
    (kp-insert . ?\xff9e)
    (kp-delete . ?\xff9f)
    (kp-equal . ?\xffbd)
    (kp-multiply . ?\xffaa)
    (kp-add . ?\xffab)
    (kp-separator . ?\xffac)
    (kp-subtract . ?\xffad)
    (kp-decimal . ?\xffae)
    (kp-divide . ?\xffaf)
    (kp-0 . ?\xffb0)
    (kp-1 . ?\xffb1)
    (kp-2 . ?\xffb2)
    (kp-3 . ?\xffb3)
    (kp-4 . ?\xffb4)
    (kp-5 . ?\xffb5)
    (kp-6 . ?\xffb6)
    (kp-7 . ?\xffb7)
    (kp-8 . ?\xffb8)
    (kp-9 . ?\xffb9)
    ;; *** Auxilliary functions ****************************************
    (f1 . ?\xffbe)
    (f2 . ?\xffbf)
    (f3 . ?\xffc0)
    (f4 . ?\xffc1)
    (f5 . ?\xffc2)
    (f6 . ?\xffc3)
    (f7 . ?\xffc4)
    (f8 . ?\xffc5)
    (f9 . ?\xffc6)
    (f10 . ?\xffc7)
    (f11 . ?\xffc8)
    (f12 . ?\xffc9)
    (f13 . ?\xffca)
    (f14 . ?\xffcb)
    (f15 . ?\xffcc)
    (f16 . ?\xffcd)
    (f17 . ?\xffce)
    (f18 . ?\xffcf)
    (f19 . ?\xffd0)
    (f20 . ?\xffd1)
    (f21 . ?\xffd2)
    (f22 . ?\xffd3)
    (f23 . ?\xffd4)
    (f24 . ?\xffd5)
    (f25 . ?\xffd6)
    (f26 . ?\xffd7)
    (f27 . ?\xffd8)
    (f28 . ?\xffd9)
    (f29 . ?\xffda)
    (f30 . ?\xffdb)
    (f31 . ?\xffdc)
    (f32 . ?\xffdd)
    (f33 . ?\xffde)
    (f34 . ?\xffdf)
    (f35 . ?\xffe0)
    ;; *** Modifier keys ***********************************************
;    (shift-l . ?\xffe1)
;    (shift-r . ?\xffe2)
;    (control-l . ?\xffe3)
;    (control-r . ?\xffe4)
;    (caps-lock . ?\xffe5)
    (capslock . ?\xffe5)
;    (shift-lock . ?\xffe6)
;    (meta-l . ?\xffe7)
;    (meta-r . ?\xffe8)
;    (alt-l . ?\xffe9)
;    (alt-r . ?\xffea)
;    (super-l . ?\xffeb)
;    (super-r . ?\xffec)
;    (hyper-l . ?\xffed)
;    (hyper-r . ?\xffee)
    ;; *** ISO 9995 function and modifier keys *************************
;    (iso-lock . ?\xfe01)
;    (iso-level2-latch . ?\xfe02)
;    (iso-level3-shift . ?\xfe03)
;    (iso-level3-latch . ?\xfe04)
;    (iso-level3-lock . ?\xfe05)
;    (iso-group-shift . ?\xff7e)
;    (iso-group-latch . ?\xfe06)
;    (iso-group-lock . ?\xfe07)
;    (iso-next-group . ?\xfe08)
;    (iso-next-group-lock . ?\xfe09)
;    (iso-prev-group . ?\xfe0a)
;    (iso-prev-group-lock . ?\xfe0b)
;    (iso-first-group . ?\xfe0c)
;    (iso-first-group-lock . ?\xfe0d)
;    (iso-last-group . ?\xfe0e)
;    (iso-last-group-lock . ?\xfe0f)
;    (iso-left-tab . ?\xfe20)
    (iso-lefttab . ?\xfe20)
    (iso-move-line-up . ?\xfe21)
    (iso-move-line-down . ?\xfe22)
    (iso-partial-line-up . ?\xfe23)
    (iso-partial-line-down . ?\xfe24)
    (iso-partial-space-left . ?\xfe25)
    (iso-partial-space-right . ?\xfe26)
    (iso-set-margin-left . ?\xfe27)
    (iso-set-margin-right . ?\xfe28)
    (iso-release-margin-left . ?\xfe29)
    (iso-release-margin-right . ?\xfe2a)
    (iso-release-both-margins . ?\xfe2b)
    (iso-fast-cursor-left . ?\xfe2c)
    (iso-fast-cursor-right . ?\xfe2d)
    (iso-fast-cursor-up . ?\xfe2e)
    (iso-fast-cursor-down . ?\xfe2f)
    (iso-continuous-underline . ?\xfe30)
    (iso-discontinuous-underline . ?\xfe31)
    (iso-emphasize . ?\xfe32)
    (iso-center-object . ?\xfe33)
    (iso-enter . ?\xfe34)
    ;; *** Lispy accent keys *******************************************
    (dead-grave . ?\xfe50)
    (dead-acute . ?\xfe51)
    (dead-circumflex . ?\xfe52)
    (dead-tilde . ?\xfe53)
    (dead-macron . ?\xfe54)
    (dead-breve . ?\xfe55)
    (dead-abovedot . ?\xfe56)
    (dead-diaeresis . ?\xfe57)
    (dead-abovering . ?\xfe58)
    (dead-doubleacute . ?\xfe59)
    (dead-caron . ?\xfe5a)
    (dead-cedilla . ?\xfe5b)
    (dead-ogonek . ?\xfe5c)
    (dead-iota . ?\xfe5d)
    (dead-voiced-sound . ?\xfe5e)
    (dead-semivoiced-sound . ?\xfe5f)
    (dead-belowdot . ?\xfe60)
    (dead-hook . ?\xfe61)
    (dead-horn . ?\xfe62)
    ;; *** Katakana ****************************************************
    (overline . ?\x47e)
    (kana-fullstop . ?\x4a1)
    (kana-openingbracket . ?\x4a2)
    (kana-closingbracket . ?\x4a3)
    (kana-comma . ?\x4a4)
    (kana-conjunctive . ?\x4a5)
;    (kana-middledot . ?\x4a5)
    (kana-WO . ?\x4a6)
    (kana-a . ?\x4a7)
    (kana-i . ?\x4a8)
    (kana-u . ?\x4a9)
    (kana-e . ?\x4aa)
    (kana-o . ?\x4ab)
    (kana-ya . ?\x4ac)
    (kana-yu . ?\x4ad)
    (kana-yo . ?\x4ae)
    (kana-tsu . ?\x4af)
;    (kana-tu . ?\x4af)
    (prolongedsound . ?\x4b0)
    (kana-A . ?\x4b1)
    (kana-I . ?\x4b2)
    (kana-U . ?\x4b3)
    (kana-E . ?\x4b4)
    (kana-O . ?\x4b5)
    (kana-KA . ?\x4b6)
    (kana-KI . ?\x4b7)
    (kana-KU . ?\x4b8)
    (kana-KE . ?\x4b9)
    (kana-KO . ?\x4ba)
    (kana-SA . ?\x4bb)
    (kana-SHI . ?\x4bc)
    (kana-SU . ?\x4bd)
    (kana-SE . ?\x4be)
    (kana-SO . ?\x4bf)
    (kana-TA . ?\x4c0)
    (kana-CHI . ?\x4c1)
;    (kana-TI . ?\x4c1)
    (kana-TSU . ?\x4c2)
;    (kana-TU . ?\x4c2)
    (kana-TE . ?\x4c3)
    (kana-TO . ?\x4c4)
    (kana-NA . ?\x4c5)
    (kana-NI . ?\x4c6)
    (kana-NU . ?\x4c7)
    (kana-NE . ?\x4c8)
    (kana-NO . ?\x4c9)
    (kana-HA . ?\x4ca)
    (kana-HI . ?\x4cb)
    (kana-FU . ?\x4cc)
;    (kana-HU . ?\x4cc)
    (kana-HE . ?\x4cd)
    (kana-HO . ?\x4ce)
    (kana-MA . ?\x4cf)
    (kana-MI . ?\x4d0)
    (kana-MU . ?\x4d1)
    (kana-ME . ?\x4d2)
    (kana-MO . ?\x4d3)
    (kana-YA . ?\x4d4)
    (kana-YU . ?\x4d5)
    (kana-YO . ?\x4d6)
    (kana-RA . ?\x4d7)
    (kana-RI . ?\x4d8)
    (kana-RU . ?\x4d9)
    (kana-RE . ?\x4da)
    (kana-RO . ?\x4db)
    (kana-WA . ?\x4dc)
    (kana-N . ?\x4dd)
    (voicedsound . ?\x4de)
    (semivoicedsound . ?\x4df)
;    (kana-switch . ?\xFF7E)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode management
(defcustom scim-mode nil
  "Toggle scim-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `scim-mode'."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :version "22.1"
  :type 'boolean
  :group 'scim
  :require 'scim-bridge)

;; Hook variables
(defvar scim-set-commit-string-hook nil)
(defvar scim-commit-string-hook nil)
(defvar scim-preedit-show-hook nil)

;; Manage key bindings
(defvar scim-mode-map nil)
(defvar scim-mode-preedit-map nil)
(defvar scim-mode-common-map nil)
(defvar scim-mode-kana-ro-map nil)
(defvar scim-mode-minimum-map nil)
(defvar scim-mode-map-alist nil)
(defvar scim-mode-map-disabled nil)
(make-variable-buffer-local 'scim-mode-map-disabled)
(defvar scim-mode-map-prev-disabled nil)
(make-variable-buffer-local 'scim-mode-map-prev-disabled)
(put 'scim-mode-map-prev-disabled 'permanent-local t)
(defvar scim-kana-ro-prev-x-keysym nil)
(defvar scim-keyboard-layout nil)
(defvar scim-keymap-overlay nil)

;; Communication & buffer editing
(defvar scim-bridge-socket nil)
(defvar scim-bridge-socket-alist nil)
(defvar scim-callback-queue nil)
(defvar scim-selected-display nil)
(defvar scim-last-command-event nil)
(defvar scim-current-buffer nil)
(defvar scim-selected-frame nil)
(defvar scim-frame-focus nil)
(defvar scim-focus-update-timer nil)
(defvar scim-net-active-window-unsupported nil)
(defvar scim-string-insertion-failed nil)
(defvar scim-config-last-modtime nil)
(defvar scim-last-rejected-event nil)
(defvar scim-last-command nil)
(defvar scim-cursor-prev-location nil)

;; IMContexts
(defvar scim-buffer-group nil)
(make-variable-buffer-local 'scim-buffer-group)
(put 'scim-buffer-group 'permanent-local t)
;; Memo:
;;  Each element of `scim-buffer-group-alist' is a list:
;;  (GROUP IMCONTEXT-ID-ALIST IMCONTEXT-STATUS-ALIST BUFFER-LIST)
;;  GROUP is group identifier which is an object comparable by `eq'
;;  IMCONTEXT-ID-ALIST is an alist of string such as "12"
;;  IMCONTEXT-STATUS-ALIST is an alist of boolean nil or t
;;  BUFFER-LIST is a list of buffers which belong to this group
;; Example:
;;  ((#<buffer text.txt>
;;          ((":1.0" . "1")
;;           (":0.0" . "20"))
;;          ((":1.0" . t)
;;           (":0.0" . nil))
;;          (#<buffer text.txt>))
;;   (#<buffer *scratch*>
;;          ((":1.0" . "2")
;;           (":0.0" . "19"))
;;          ((":1.0" . nil)
;;           (":0.0" . t))
;;          (#<buffer *scratch*>)))
(defvar scim-buffer-group-alist nil)
(defvar scim-imcontext-id nil)
(defvar scim-imcontext-status nil)
(defvar scim-preediting-p nil)
(defvar scim-preedit-point (make-marker))
(defvar scim-preedit-update nil)
(defvar scim-preedit-shown "")
(defvar scim-preedit-string "")
(defvar scim-preedit-prev-string "")
(defvar scim-preedit-curpos 0)
(defvar scim-preedit-prev-curpos 0)
(defvar scim-preedit-attributes nil)
(defvar scim-preedit-prev-attributes nil)
(defvar scim-preedit-overlays nil)
(defvar scim-committed-string "")
(defvar scim-saved-frame-coordinates '(0 . 0))
(defvar scim-adjust-window-x-offset 0)
(defvar scim-surrounding-text-modified nil)
(defvar scim-cursor-type-saved 0)
(make-variable-buffer-local 'scim-cursor-type-saved)

;; Minibuffer
(defvar scim-parent-buffer-group nil)
(defvar scim-force-inherit-im nil)
(defvar scim-isearch-buffer-group nil)
(defvar scim-isearch-minibuffer nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
(defun scim-escape-string (str)
  (let* ((tmp (append str nil))
	 cur
	 (next tmp))
    (while (setq cur (memq ?\\ next))
      (setq next (cdr cur))
      (setcdr cur (cons ?\\ next)))
    (setq next tmp)
    (while (setq cur (memq ?\n next))
      (setq next (cdr cur))
      (setcar cur ?\\)
      (setcdr cur (cons ?n next)))
    (setq next tmp)
    (while (setq cur (memq ?\  next))
      (setq next (cdr cur))
      (setcar cur ?\\)
      (setcdr cur (cons ?s next)))
    (concat tmp)))

(defun scim-construct-command (list)
  (mapconcat (lambda (f) (scim-escape-string f)) list " "))

(defun scim-unescape-string (str)
  (let* ((tmp (append str nil))
	 (cur tmp)
	 next)
    (while (setq cur (memq ?\\ cur))
      (setq next (cdr cur))
      (setcar cur (car (or (rassq (car next)
				  '((?\  . ?s)
				    (?\n . ?n)
;				    (?\\ . ?\\)
				    ))
			   next)))
      (setq cur (setcdr cur (cdr next))))
    (concat (delq nil tmp))))

(defun scim-split-commands (commands)
  (mapcar (lambda (line)
	    (mapcar 'scim-unescape-string (split-string line " ")))
	  (split-string (substring commands 0 -1) "\n")))

(defun scim-decode-event (event)
  ;; Convert Emacs event to scim-bridge command
  (let ((modifiers (mapcar (lambda (mod)
			     (cdr (assq mod scim-modifier-alist)))
			   (event-modifiers event)))
	(key-code (event-basic-type event)))
    (if (numberp key-code)
	(if (and (member "shift" modifiers)
		 (>= key-code ?a)
		 (<= key-code ?z))
	    (setq key-code (- key-code 32)))
      (if (member "alt" modifiers)
	  (setq key-code
		(or (cdr (assq key-code scim-alt-modifier-alist))
		    key-code)))
      (if (and scim-use-kana-ro-key
	       scim-kana-ro-key-symbol
	       (eq key-code scim-kana-ro-key-symbol))
	  (setq key-code ?\\
		modifiers (cons "kana_ro" modifiers)))
      (setq key-code (or (cdr (assq key-code scim-keycode-alist))
			 key-code)))
    (cons key-code modifiers)))

(defun scim-encode-event (key-code modifiers)
  ;; Convert scim-bridge command to Emacs event
  (setq key-code (string-to-number key-code))
  (let ((bas (or (car (rassq key-code scim-keycode-alist))
		 (if (< key-code 128) key-code)))
	(mods nil))
    (if (member "alt" modifiers)
	(setq bas (or (car (rassq bas scim-alt-modifier-alist))
		      bas)))
    (while modifiers
      (let ((m (car (rassoc (car modifiers) scim-modifier-alist))))
	(if m (cond ((eq m 'caps-lock)
		     nil) ; Ignore `caps_lock' modifier
		    ((eq m 'num-lock)
		     nil) ; Ignore `num_lock' modifier
		    ((eq m 'kana-RO)
		     (if (and scim-use-kana-ro-key
			      scim-kana-ro-key-symbol
			      (eq bas ?\\)
			      (not scim-mode-map-prev-disabled))
			 (setq bas scim-kana-ro-key-symbol)))
		    (t (add-to-list 'mods m)))))
      (setq modifiers (cdr modifiers)))
    (if bas (event-convert-list (nconc mods (list bas))))))

(defun scim-twos-complement (string)
  (let ((num (string-to-number string)))
    (if (< num 2147483648.0)
	(round num)
      (round (- num 4294967296.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Localization
(defun scim-set-group-doc (group string)
  "Change the documentation string of GROUP into STRING.
If STRING is empty or nil, the documentation string is left original."
  (if (> (length string) 0)
      (put group 'group-documentation string)))

(defun scim-set-variable-doc (variable string &optional custom-type)
  "Change the documentation string of VARIABLE into STRING.
If STRING is empty or nil, the documentation string is left original.
If CUSTOM-TYPE is non-nil, it is set to the `custom-type' property of
VARIABLE, which corresponds to the :type keyword in `defcustom'."
  (if (> (length string) 0)
      (put variable 'variable-documentation string))
  (if custom-type
      (put variable 'custom-type custom-type)))

(defun scim-set-face-doc (face string)
  "Change the documentation string of FACE into STRING.
If STRING is empty or nil, the documentation string is left original."
  (if (> (length string) 0)
      (put face 'face-documentation string)))

(defun scim-set-function-doc (function string)
  "Change the documentation string of FUNCTION into STRING.
If STRING is empty or nil, the documentation string is left original."
  (if (and (fboundp function)
	   (> (length string) 0))
      (let ((func (symbol-function function)))
	(if (byte-code-function-p func)
	    (let ((new-func (append func nil)))
	      (setcar (nthcdr 4 new-func) string)
	      (fset function (apply 'make-byte-code new-func)))
	  (setcar (nthcdr 2 func) string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages & Log
(defun scim-log1 (format-string args)
  (let ((log-str (apply 'format format-string args)))
    (with-current-buffer (get-buffer-create scim-log-buffer)
      (let ((window (get-buffer-window (current-buffer))))
	(save-selected-window
	  (if window (select-window window))
	  (goto-char (point-max))
	  (insert log-str ?\n)
	  (if window (recenter -1))
	  log-str)))))

(defun scim-log (format-string &rest args)
  (if (and scim-debug
	   format-string)
      (scim-log1 format-string args)))

(defun scim-log-undo-list (format-string &rest args)
  (when scim-debug
    (if format-string
	(scim-log1 format-string args))
    (if (not (listp buffer-undo-list))
	(scim-log1 "undo list (disabled): %S" (list buffer-undo-list))
      (scim-log1 " top: %S" (list (car buffer-undo-list)))
      (scim-log1 " 2nd: %S" (list (cadr buffer-undo-list)))
      (scim-log1 " 3rd: %S" (list (nth 2 buffer-undo-list)))
      (scim-log1 " 4th: %S" (list (nth 3 buffer-undo-list))))))

(defun scim-message (format-string &rest args)
  (apply 'message (concat "SCIM: " format-string) args)
  (apply 'scim-log (concat "message: " format-string) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control buffer-undo-list
(defun scim-insert-and-modify-undo-list (str)
  (let* ((prev-list (if (car-safe buffer-undo-list)
			buffer-undo-list
		      (cdr-safe buffer-undo-list)))
	 (prev (car-safe prev-list))
	 (consp-prev (and (consp prev)
			  (integerp (car prev))
			  (integerp (cdr prev))))
	 (consecutivep (and consp-prev
			   (= (cdr prev) (point))
			   (not (= (preceding-char) ?\n))
			   (<= (+ (string-width (buffer-substring-no-properties
						 (car prev) (point)))
				  (string-width str))
			       20)))) ; max 20 columns
;#    (scim-log-undo-list "previous undo list")
    (when (and consp-prev
	       (integerp (car (cdr prev-list))))
;#      (scim-log-undo-list "get rid of point setting entry")
      (setcdr prev-list (cdr (cdr prev-list))))
    (insert-and-inherit str)
;#    (scim-log-undo-list "insert string: %S" str)
    (when (integerp (car (cdr-safe buffer-undo-list)))
;#      (scim-log-undo-list "get rid of point setting entry")
      (setcdr buffer-undo-list (cdr (cdr buffer-undo-list))))
    (if (and consecutivep
	     (eq (cdr (cdr buffer-undo-list)) prev-list))
	(when (eq scim-last-command 'self-insert-command)
;#	  (scim-log-undo-list "unify consecutive insertion entries")
	  (setcar (car buffer-undo-list) (car (car prev-list)))
	  (setcdr buffer-undo-list (cdr prev-list)))
      (when (and (> (string-width str) 20)
		 (listp buffer-undo-list)) ; Undo enabled?
	(let ((beg (car (car buffer-undo-list)))
	      (end (cdr (car buffer-undo-list)))
	      (new-list (cdr buffer-undo-list)))
;#	  (scim-log-undo-list "divide long insertion entry")
	  (while (let ((len (length (truncate-string-to-width str 20))))
		   (setq new-list (cons nil (cons (cons beg (+ beg len)) new-list))
			 beg (+ beg len)
			 str (substring-no-properties str len))
		   (> (string-width str) 20)))
	  (setq buffer-undo-list (cons (cons beg end) new-list)))))))

;; Advices for commands which conflict with preediting
(defun scim-defadvice-disable-for-preedit ()
  (mapc (lambda (command)
	  (eval
	   `(defadvice ,command
	      (around ,(intern (concat "scim-inhibit-" (symbol-name command))) ())
	      (if scim-preediting-p
		  (error "SCIM: `%s' cannot be used while preediting!" ',command)
		ad-do-it))))
	scim-preedit-incompatible-commands))

(defun scim-activate-advices-disable-for-preedit (enable)
  (with-no-warnings
    (if enable
	(ad-enable-regexp "^scim-inhibit-")
      (ad-disable-regexp "^scim-inhibit-"))
    (ad-activate-regexp "^scim-inhibit-")))

;; Advices for yasnippet (version < 0.6)
(mapc (lambda (command)
	(eval
	 `(defadvice ,command
	    (around ,(intern (concat "scim-inhibit-" (symbol-name command))) ())
	    (unless scim-preediting-p
	      ad-do-it))))
      '(yas/field-undo-before-hook
	yas/check-cleanup-snippet
	yas/field-undo-after-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control keyboard
(defun scim-set-mode-map-alist ()
  (setq scim-mode-map-alist
	(list (cons 'scim-mode scim-mode-map)
	      (cons 'scim-preediting-p scim-mode-preedit-map))))

(defun scim-set-keymap-parent ()
  (set-keymap-parent scim-mode-map
		     (cond
		      ((or (not (eq window-system 'x))
			   scim-mode-map-prev-disabled)
;#		       (scim-log "use empty keymap")
		       nil)
		      ((or (not scim-use-minimum-keymap)
			   scim-imcontext-status)
;#		       (scim-log "use common keymap")
		       scim-mode-common-map)
		      (t
;#		       (scim-log "use minimum keymap")
		       scim-mode-minimum-map))))

(defun scim-enable-kana-ro-key (&optional keysym)
  (unless keysym (setq keysym scim-kana-ro-x-keysym))
  (when keysym
;#    (scim-log "enable Kana-RO key: %s" keysym)
    (shell-command-to-string
     (concat "xmodmap -pke | sed -n 's/\\(\\(=\\) backslash underscore\\| backslash underscore$\\)/\\2 "
	     keysym " underscore/gp' | xmodmap -"))
    (setq scim-kana-ro-prev-x-keysym keysym)))

(defun scim-disable-kana-ro-key (&optional keysym)
  (unless keysym (setq keysym scim-kana-ro-prev-x-keysym))
  (when keysym
;#    (scim-log "disable Kana-RO key: %s" keysym)
    (shell-command-to-string
     (concat "xmodmap -pke | sed -n 's/\\(\\(=\\) " keysym " underscore\\| " keysym
	     " underscore$\\)/\\2 backslash underscore/gp' | xmodmap -"))
    (setq scim-kana-ro-prev-x-keysym nil)))

(defun scim-get-keyboard-layout ()
  (let ((xkb-rules (x-window-property "_XKB_RULES_NAMES" nil "STRING" 0 nil nil)))
    (if xkb-rules
	(intern (cadr (split-string xkb-rules "\0" t))))))

(defun scim-update-kana-ro-key (&optional inhibit delayed)
  (when (eq scim-keyboard-layout 'jp106)
    (if (and (not inhibit)
	     scim-use-kana-ro-key
	     scim-kana-ro-x-keysym
	     scim-frame-focus
	     (not scim-mode-map-prev-disabled)
	     (or (not scim-use-minimum-keymap) scim-imcontext-status))
	(if delayed
	    (let ((cycle (if scim-net-active-window-unsupported
			     scim-focus-update-interval-long
			   scim-focus-update-interval)))
	      (run-at-time (+ cycle 0.1) nil 'scim-enable-kana-ro-key))
	  (scim-enable-kana-ro-key))
      (scim-disable-kana-ro-key))))

(defun scim-switch-keymap (enable)
  (if (and (not enable)
	   scim-preediting-p)
      (scim-abort-preedit))
  (setq scim-mode-map-prev-disabled (not enable))
  (scim-update-cursor-color)
  (scim-set-keymap-parent)
  (if (and scim-use-kana-ro-key
	   scim-kana-ro-x-keysym
	   scim-frame-focus
	   (or (not scim-use-minimum-keymap) scim-imcontext-status))
      (scim-update-kana-ro-key)))

(defun scim-enable-keymap ()
  (interactive)
  (if scim-mode
      (scim-switch-keymap t))
  (setq scim-mode-map-disabled nil))

(defun scim-disable-keymap ()
  (interactive)
  (if scim-mode
      (scim-switch-keymap nil))
  (setq scim-mode-map-disabled t))

(defun scim-check-major-mode ()
  (if (memq major-mode scim-incompatible-major-modes)
      (scim-disable-keymap)))

(defun scim-make-keymap-internal (keys &optional parent &rest ranges)
  (let ((map (if ranges (make-keymap) (make-sparse-keymap))))
    (if parent (set-keymap-parent map parent))
    (while ranges
      (let ((i (caar ranges))
	    (max (cdar ranges)))
	(while (<= i max)
	  (define-key map (char-to-string i) 'scim-handle-event)
	  (setq i (1+ i))))
      (setq ranges (cdr ranges)))
    (while keys
      (let* ((key (reverse (car keys)))
	     (bas (car key))
	     (mods (cdr key)))
	(if (stringp bas)
	    (setq bas (string-to-char bas)))
	(when (memq 'alt mods)
	  (unless scim-meta-key-exists
	    (setq mods (cons 'meta (delq 'alt mods))))
	  (setq bas (or (car (rassq bas scim-alt-modifier-alist))
			bas)))
	(define-key map (vector (nconc mods (list bas))) 'scim-handle-event)
	(setq keys (cdr keys))))
    map))

(defun scim-combine-modifiers (base modifiers)
  (if modifiers
      (apply 'nconc
	     (mapcar
	      (lambda (k) (list (cons (car modifiers) k) k))
	      (scim-combine-modifiers base (cdr modifiers))))
    (list (list base))))

(defun scim-make-minimum-map ()
  (scim-make-keymap-internal scim-common-function-key-list))

(defun scim-make-kana-ro-map ()
  (scim-make-keymap-internal (if (and scim-use-kana-ro-key
				      scim-kana-ro-key-symbol)
				 (scim-combine-modifiers
				  scim-kana-ro-key-symbol
				  '(meta control hyper super alt)))
			     scim-mode-minimum-map))

(defun scim-make-common-map ()
  (scim-make-keymap-internal nil
			     scim-mode-kana-ro-map
			     '(32 . 126)))

(defun scim-make-preedit-map ()
  (scim-make-keymap-internal scim-preedit-function-key-list
			     nil
			     '(0 . 26) '(28 . 31)))

(defun scim-update-key-bindings (&optional symbol)
  (when (and scim-frame-focus
	     (not scim-mode-map-prev-disabled)
	     (or (null symbol)
		 (and (eq symbol 'scim-use-kana-ro-key)
		      scim-kana-ro-x-keysym
		      (or (not scim-use-minimum-keymap) scim-imcontext-status))
		 (and (eq symbol 'scim-kana-ro-x-keysym)
		      scim-use-kana-ro-key
		      (or (not scim-use-minimum-keymap) scim-imcontext-status))
		 (and (eq symbol 'scim-use-minimum-keymap)
		      (not scim-imcontext-status)
		      scim-use-kana-ro-key
		      scim-kana-ro-x-keysym)))
    (when (memq symbol '(nil scim-kana-ro-x-keysym))
      (scim-update-kana-ro-key t))
    (scim-update-kana-ro-key))
  (when (null symbol)
;#    (scim-log "update scim-mode-minimum-map")
    (if (keymapp scim-mode-minimum-map)
	(setcdr scim-mode-minimum-map (cdr (scim-make-minimum-map)))
      (setq scim-mode-minimum-map (scim-make-minimum-map))))
  (when (memq symbol '(nil scim-use-kana-ro-key scim-kana-ro-key-symbol))
;#    (scim-log "update scim-mode-kana-ro-map")
    (if (keymapp scim-mode-kana-ro-map)
	(setcdr scim-mode-kana-ro-map (cdr (scim-make-kana-ro-map)))
      (setq scim-mode-kana-ro-map (scim-make-kana-ro-map))))
  (when (memq symbol '(nil scim-common-function-key-list))
;#    (scim-log "update scim-mode-common-map")
    (if (keymapp scim-mode-common-map)
	(setcdr scim-mode-common-map (cdr (scim-make-common-map)))
      (setq scim-mode-common-map (scim-make-common-map))))
  (when (memq symbol '(nil scim-use-minimum-keymap))
;#    (scim-log "update scim-mode-map")
    (unless (keymapp scim-mode-map)
      (setq scim-mode-map (make-sparse-keymap)))
    (define-key scim-mode-map [scim-receive-event] 'scim-exec-callback)
    (scim-set-keymap-parent))
  (when (memq symbol '(nil scim-preedit-function-key-list))
;#    (scim-log "update scim-mode-preedit-map")
    (if (keymapp scim-mode-preedit-map)
	(setcdr scim-mode-preedit-map (cdr (scim-make-preedit-map)))
      (setq scim-mode-preedit-map (scim-make-preedit-map)))))

(defun scim-define-key (symbol keys handle)
  ;; If keys is given as an array, it doesn't indicate key sequence,
  ;; but multiple definitions of single keystroke.
  (let ((keys-list (if (arrayp keys)
		       (listify-key-sequence keys)
		     (list keys))))
    (while keys-list
      (let ((key (car keys-list)))
	(if (listp key)
	    (let* ((n (1- (length key)))
		   (bas (nth n key)))
	      ;; If the key event is specified by a list and the last
	      ;; element is given as a string, the code number for the first
	      ;; character of the string is used for an event basic type.
	      (when (stringp bas)
		(setq key (copy-sequence key))
		(setcar (nthcdr n key) (string-to-char bas)))
	      (setq key (event-convert-list key))))
	;; In Emacs 22, the function `event-modifiers' cannot return the
	;; correct value until the symbol is parsed.
	(key-binding (vector key))
	;; It is necessary to call a function `event-basic-type' after
	;; `event-modifiers' because `event-basic-type' uses the symbol
	;; property `event-symbol-elements' added by `event-modifiers'
	;; when event is given as a symbol.
	(let ((modifiers (event-modifiers key))
	      (key-code (event-basic-type key)))
	  (if (integerp key-code)
	      (setq key-code (char-to-string key-code)
		    modifiers (reverse modifiers)))
	  (setq key (append modifiers (list key-code))))
	(if handle
	    (add-to-list symbol key)
	  (set symbol (delete key (symbol-value symbol)))))
      (setq keys-list (cdr keys-list)))
    (symbol-value symbol))) ; Return value

(defun scim-define-common-key (key handle)
  "Specify which key events SCIM anytime takes over. If HANDLE
is non-nil, SCIM handles the key events given by KEY. When KEY is
given as an array, it doesn't indicate key sequence, but multiple
definitions of single keystroke.
 It is necessary to call a function `scim-update-key-bindings' or
restart scim-mode so that this settings may become effective."
  (scim-define-key 'scim-common-function-key-list key handle))

(defun scim-define-preedit-key (key handle)
  "Specify which key events SCIM takes over when preediting. If
HANDLE is non-nil, SCIM handles the key events given by KEY. When
KEY is given as an array, it doesn't indicate key sequence, but
multiple definitions of single keystroke.
 It is necessary to call a function `scim-update-key-bindings' or
restart scim-mode so that this settings may become effective."
  (scim-define-key 'scim-preedit-function-key-list key handle))

;; Advice for `describe-key'
(defadvice describe-key
  (around scim-describe-key ())
  (cond
   ((not scim-mode)
    ad-do-it)
   ((null scim-mode-map-alist)
    ;; Translate Jananese kana RO key
    (if (commandp (lookup-key (scim-make-keymap-internal
			       (scim-combine-modifiers
				scim-kana-ro-key-symbol
				'(meta control hyper super alt)))
			      key))
	(let ((mods (event-modifiers (aref key 0))))
	  (setq key (vector
		     (event-convert-list
		      (delq 'shift
			    (append mods (list (if (memq 'shift mods) 95 92)))))))))
    ad-do-it)
   (t
    ;; Set modified flag of *Help* buffer in order to detect
    ;; whether *Help* is updated or not.
    (with-current-buffer (help-buffer)
      (set-buffer-modified-p t))
    (if (eq (key-binding key) 'scim-handle-event)
	;; Invoke `describe-key' without scim-mode's keymaps
	(let ((scim-mode-map-alist nil))
	  (if scim-keymap-overlay
	      (overlay-put scim-keymap-overlay 'keymap nil))
	  (setq unread-command-events
		(nconc (listify-key-sequence key) unread-command-events))
	  (call-interactively 'describe-key)
	  (if scim-keymap-overlay
	      (overlay-put scim-keymap-overlay 'keymap scim-mode-preedit-map)))
      ad-do-it)
    ;; Add descriptions to *Help* buffer, if any
    (with-current-buffer (help-buffer)
      (let* ((raw (vector (aref (this-single-command-raw-keys) 0)))
	     (format (format "SCIM: scim-mode handles %s when %%s.\n"
			     (key-description raw)))
	     (preedit (lookup-key scim-mode-preedit-map raw))
	     (common (lookup-key scim-mode-common-map raw))
	     (minimum (lookup-key scim-mode-minimum-map raw))
	     (inhibit-read-only t))
	(when (or preedit common minimum)
	  ;; Popup *Help* buffer if it was't updated
	  (if (or (= (buffer-size) 0)
		  (buffer-modified-p))
	      (with-output-to-temp-buffer (help-buffer)
		(princ (current-message))))
	  ;; Insert above [BACK] button
	  (goto-char (point-max))
	  (beginning-of-line 0)
	  ;; When *Help* is opened for the first time, [BACK] button doesn't appear
	  (unless (get-text-property (point) 'button)
	    (goto-char (point-max))
	    (insert "\n"))
	  (if (or preedit common)
	      (insert (format format "preediting")))
	  (if common
	      (insert (format format "SCIM is active")))
	  (if (if scim-use-minimum-keymap minimum common)
	      (insert (format format "SCIM is not active")))
	  (insert "\n")))))))

(defun scim-activate-advice-describe-key (enable)
  (if enable
      (ad-enable-advice 'describe-key 'around 'scim-describe-key)
    (ad-disable-advice 'describe-key 'around 'scim-describe-key))
  (ad-activate 'describe-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control display
(defun scim-get-x-display ()
  (let ((env (or scim-bridge-x-display-substitute
		 (frame-parameter nil 'display)
		 (getenv "DISPLAY"))))
    (and env
	 (let* ((display (substring env (string-match ":[0-9]+" env)))
		(screen (and (not (string-match "\\.[0-9]+$" display)) ".0")))
	   (concat display screen)))))

(defun scim-set-cursor-color (&optional single-frame)
  (let ((color (cond
		;;    |      `scim-cursor-color'     |   ON   |   OFF  |Disabled|
		;;    +------------------------------+--------+--------+--------+
		;;    |              nil             |  none  |  none  |  none  |
		((or (null scim-cursor-color)
		     (null scim-imcontext-id))
		 nil)
		;;    |           "color1"           | color1 |  none  |  none  |
		((stringp scim-cursor-color)
		 (if scim-imcontext-status
		     scim-cursor-color))
		;;    |    ("color1" . "color2")     | color1 | color2 | color2 |
		((consp scim-cursor-color)
		 (let ((tail (cdr scim-cursor-color)))
		   (cond
		    ((stringp tail)
		     (if scim-imcontext-status
			 (car scim-cursor-color)
		       tail))
		    ;;|     ("color1" "color2")      | color1 | color2 |  none  |
		    ;;| ("color1" "color2" "color3") | color1 | color2 | color3 |
		    ((consp tail)
		     (if scim-mode-map-prev-disabled
			 (cadr tail)
		       (if scim-imcontext-status
			   (car scim-cursor-color)
			 (car tail)))))))))
	(ac-fuzzy (with-no-warnings
		    ;; Fuzzy state of auto-complete-mode
		    (and (featurep 'auto-complete)
			 (bound-and-true-p ac-fuzzy-enable)
			 ac-fuzzy-cursor-color)))
	(viper (with-no-warnings
		 (and (featurep 'viper)
		      viper-mode
		      (eq viper-current-state 'insert-state))))
	(orig-frame (selected-frame)))
;#    (scim-log "set cursor color: %S" color)
    (condition-case err
	(while (progn
		 (unless single-frame
		   (select-frame (next-frame nil t)))
		 (when (or (and (eq window-system 'x)
				(not ac-fuzzy)
				(eq (window-buffer (frame-selected-window))
				    scim-current-buffer))
			   (not scim-mode))
		   (unless color
		     (setq color (frame-parameter nil 'foreground-color)))
		   (if viper
		       (with-no-warnings
			 (setq viper-insert-state-cursor-color color)
			 (viper-set-cursor-color-according-to-state))
		     (set-cursor-color color)))
		 (not (eq (selected-frame) orig-frame))))
      (error
	(select-frame orig-frame)
	(scim-message "Failed to set cursor color %S" err)))))

(defun scim-update-cursor-color (&optional single-frame)
  (if (and scim-cursor-color
	   (eq (selected-frame) scim-selected-frame))
      (scim-set-cursor-color single-frame)))

(defun scim-after-make-frame-function (frame)
  (save-current-buffer
    (let ((old-frame (selected-frame)))
      (unwind-protect
	  (progn
	    (select-frame frame)
	    (set-buffer (window-buffer (frame-selected-window)))
	    (let ((scim-selected-frame frame))
	      (scim-update-cursor-color t)))
	(select-frame old-frame)))))

(defun scim-reset-imcontext-statuses ()
  "Reset entirely the variables which keep the IMContext statuses
of each buffer in order to correct impropriety of the cursor color.
This function might be invoked just after using SCIM GUI Setup Utility."
  (mapc (lambda (group)
	  (let ((pair (assoc scim-selected-display (nth 2 group))))
	    (if pair
		(setcdr pair nil))))
	scim-buffer-group-alist)
  (setq scim-imcontext-status nil)
  (scim-update-cursor-color))

(defun scim-frame-top-left-coordinates (&optional frame)
  "Return the pixel coordinates of FRAME as a cons cell (LEFT . TOP),
which are relative to top left corner of screen.

If FRAME is omitted, use selected-frame.

Users can also get the frame coordinates by referring the variable
`scim-saved-frame-coordinates' just after calling this function."
  ;; Note: This function was imported from pos-tip.el ver. 0.0.3
;#  (scim-log "get frame coordinates")
  (with-current-buffer (get-buffer-create " *xwininfo*")
    (let ((case-fold-search nil))
      (buffer-disable-undo)
      (erase-buffer)
      (call-process shell-file-name nil t nil shell-command-switch
		    (concat "xwininfo -id " (frame-parameter frame 'window-id)))
      (goto-char (point-min))
      (search-forward "\n  Absolute")
      (setq scim-cursor-prev-location nil
	    scim-saved-frame-coordinates
	    (cons (progn (string-to-number (buffer-substring-no-properties
					    (search-forward "X: ")
					    (line-end-position))))
		  (progn (string-to-number (buffer-substring-no-properties
					    (search-forward "Y: ")
					    (line-end-position)))))))))

(defun scim-compute-pixel-position (&optional pos window frame-coordinates)
  "Return the absolute pixel coordinates of POS in WINDOW as a cons cell
like (X . Y), showing the location of bottom left corner of the character.

Omitting POS and WINDOW means use current position and selected window,
respectively.

If FRAME-COORDINATES is omitted, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the return value. If
non-nil, specifies the frame location as a cons cell like (LEFT . TOP).
This option makes the calculations slightly faster, but can be used only
when it's clear that frame is in the specified position. Users can get
the previous values of frame coordinates by referring the variable
`scim-saved-frame-coordinates'."
  (unless window
    (setq window (selected-window)))
  (let ((frame (window-frame window)))
    (unless frame-coordinates
      (scim-frame-top-left-coordinates frame))
    (let* ((x-y (or (pos-visible-in-window-p (or pos (window-point window)) window t)
		    '(0 0)))
	   (ax (+ (car scim-saved-frame-coordinates)
		  (car (window-inside-pixel-edges window))
		  (car x-y)))
	   (ay (+ (cdr scim-saved-frame-coordinates)
		  (cadr (window-pixel-edges window))
		  (cadr x-y)))
	   ;; `posn-object-width-height' returns an incorrect value
	   ;; when the header line is displayed (Emacs bug #4426).
	   ;; In this case, `frame-char-height' is used substitutively,
	   ;; but this function doesn't return actual character height.
	   (height (with-current-buffer (window-buffer window)
		     (cond
		      ((null header-line-format)
		       (cdr (posn-object-width-height
			     (posn-at-x-y (max (car x-y) 0) (cadr x-y) window))))
		      ((and (boundp 'text-scale-mode-amount)
			    (not (zerop text-scale-mode-amount)))
		       (round (* (frame-char-height frame)
				 (with-no-warnings
				   (expt text-scale-mode-step
					 text-scale-mode-amount)))))
		      (t
		       (frame-char-height frame))))))
      (cons ax (+ ay height)))))

;;; TODO: FIXME: Does anyone know how to get the actual character height
;;;              even if the header line is displayed?

(defun scim-get-gnome-font-size ()
  "Return the pixel size of application font in the GNOME desktop
environment. It is necessary to set the screen resolution (dots per
inch) and to be able to use a shell command `gconftool-2'. If not,
this function returns zero."
  (if (string= (shell-command-to-string "which gconftool-2") "")
      0
    (let ((font (shell-command-to-string
		 "gconftool-2 -g /desktop/gnome/interface/font_name"))
	  (dpi (shell-command-to-string
		"gconftool-2 -g /desktop/gnome/font_rendering/dpi")))
      (/ (* (string-to-number
	     (substring font (string-match "[0-9]+$" font) -1))
	    (string-to-number dpi))
	 72))))

(defun scim-set-window-x-offset ()
  (setq scim-adjust-window-x-offset
	(cond ((eq scim-adjust-window-x-position 'gnome)
	       (+ (scim-get-gnome-font-size) 4))
	      ((integerp scim-adjust-window-x-position)
	       scim-adjust-window-x-position)
	      (t 0))))

(defun scim-get-active-window-id ()
  "Return the number of the window-system window which is foreground,
i.e. input focus is in this window."
  (if scim-net-active-window-unsupported
      (string-to-number
       (shell-command-to-string
	;; If window manager doesn't support `_NET_ACTIVE_WINDOW' property,
	;; shell command `xwininfo' is used substitutively but it might
	;; return incorrect value.
	"xwininfo -root -children -int | grep -v '\"scim-panel-gtk\"\\|\"tomoe\"\\|\"nagisa\"\\|(has no name)' | grep ' children:$' -A 1 | tail -n 1"))
    (let ((x-active-window (x-window-property "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t)))
      (if x-active-window
	  ;; It's possible that `x-active-window' take the value of 0. Why?
	  (condition-case err
	      (+ (ash (car x-active-window) 16)  (cdr x-active-window))
	    (wrong-type-argument -1))
	(scim-message "Your window manager doesn't suppot _NET_ACTIVE_WINDOW property.")
	(setq scim-net-active-window-unsupported t)
	(scim-get-active-window-id)))))

(defun scim-change-x-display ()
  (let ((display (scim-get-x-display)))
;#    (scim-log "change display from %s to %s" scim-selected-display display)
    (setq scim-bridge-socket (cdr (assoc display scim-bridge-socket-alist)))
    (if scim-bridge-socket
	(setq scim-selected-display display)
      (scim-bridge-connect)
      (if (and scim-bridge-socket
;	       (equal display ":0.0") ; debug code
	       (memq (process-status scim-bridge-socket)
		     '(open run)))
	  (setq scim-bridge-socket-alist (cons (cons display scim-bridge-socket)
					       scim-bridge-socket-alist)
		scim-selected-display display)
	(scim-mode-quit)
	(error "Unable to open socket for display %S. Turned off scim-mode." display)))))

(defun scim-config-file-timestamp ()
  (nth 5 (file-attributes scim-config-file)))

(defun scim-check-frame-focus (&optional focus-in)
  (let* (active-win
	 redirect
	 (focused-p
	  (and (eq window-system 'x)
	       (setq active-win (scim-get-active-window-id))
	       (or (eq active-win
		       (string-to-number
			(frame-parameter nil 'outer-window-id)))
		   (eq active-win
		       (and (setq redirect
				  (car (delq nil
					     (mapcar (lambda (frame)
						       (and (frame-focus frame)
							    frame))
						     (frame-list)))))
			    (string-to-number
			     (frame-parameter redirect 'outer-window-id)))))))
	 (new-focus (or (not scim-frame-focus) focus-in)))
    (cond
     ((not (eq focused-p new-focus)))
     ((and new-focus
	   (not scim-frame-focus)
	   scim-config-last-modtime
	   (time-less-p scim-config-last-modtime
			(scim-config-file-timestamp)))
;#      (scim-log "SCIM's settings changed")
      (scim-mode-off)
      (if (scim-mode-on)
	  (progn
	    (scim-message "scim-mode restarted")
	    (scim-check-current-buffer))
	(scim-message "Failed to restart")))
     (t
      (setq scim-config-last-modtime (and (not new-focus)
					  (scim-config-file-timestamp)))
      (when (and (stringp scim-imcontext-id)
		 (eq (current-buffer) scim-current-buffer))
	(setq scim-frame-focus new-focus)
;#	(scim-log "change focus: %S" (and scim-frame-focus (current-buffer)))
;#	(scim-log "scim-current-buffer: %S" scim-current-buffer)
	(if scim-frame-focus
	    (setq scim-keyboard-layout (scim-get-keyboard-layout)))
	(when (and scim-use-kana-ro-key
		   scim-kana-ro-x-keysym)
	  (scim-update-kana-ro-key nil (not focus-in)))
	(scim-set-keymap-parent)
	(scim-change-focus scim-frame-focus) ; Send
	(unless scim-preediting-p
	  (scim-bridge-receive)) ; Receive
	(when scim-frame-focus
	  (scim-frame-top-left-coordinates)
	  (scim-set-window-x-offset)))
      (unless focus-in
	(scim-check-current-buffer))))))

(defun scim-cancel-focus-update-timer ()
  (when scim-focus-update-timer
    (cancel-timer scim-focus-update-timer)
    (setq scim-focus-update-timer nil)))

(defun scim-start-focus-observation ()
  (let ((cycle (if (or scim-net-active-window-unsupported
		       scim-mode-map-prev-disabled
		       (not scim-imcontext-status)
		       (not scim-frame-focus))
		   scim-focus-update-interval-long
		 scim-focus-update-interval)))
    (scim-cancel-focus-update-timer)
    (setq scim-focus-update-timer
	  (run-at-time cycle cycle 'scim-check-frame-focus))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulate preediting area
(defun scim-check-rgb-color (string)
  (and (eq (length string) 7)
       (string-match "^#[0-9A-Fa-f]+$" string)))

(defun scim-select-face-by-attr (attr)
  (cdr (assoc attr
	      '(("underline" . scim-preedit-underline-face)
		("highlight" . scim-preedit-highlight-face)
		("reverse" . scim-preedit-reverse-face)))))

(defun scim-remove-preedit (&optional abort)
  (remove-hook 'before-change-functions 'scim-before-change-function)
  (unless (or (string= scim-preedit-prev-string "")
	      abort)
    (let ((pos scim-preedit-point)
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t))
      (condition-case err
	  (progn
	    (if (consp buffer-undo-list)
		;; `buffer-undo-list' contains undo information
		(progn
		  (let ((undo-in-progress t))
		    (setq buffer-undo-list (primitive-undo 2 buffer-undo-list)))
		  ;; Restore modification flag of yasnippet field at cursor position
		  (if (boundp 'yas/active-field-overlay)
		      ;; yasnippet version >= 0.6
		      (when (memq yas/active-field-overlay (overlays-at pos))
			(dont-compile ; To avoid byte-compile error
			  (setf (yas/field-modified-p
				 (overlay-get yas/active-field-overlay 'yas/field))
				nil)))
		    ;; yasnippet version < 0.6
		    (mapc (lambda (overlay)
			    (when (overlay-get overlay 'yas/modified?)
			      (overlay-put
			       overlay 'yas/modified?
			       (overlay-get overlay 'scim-saved-yas/modified?))))
			  (overlays-at pos))))
	      ;; Undo disabled or `buffer-undo-list' is empty
	      (let (buffer-undo-list)
		(delete-region pos (+ pos (length scim-preedit-prev-string)))))
	    (undo-boundary)
	    (goto-char pos)
	    ;; Invoke function bound to `point-entered' text property
	    (let ((func (get-text-property pos 'point-entered)))
	      (when func
		(funcall func))))
	(error
	 (scim-message "Failed to delete preediting text %S" err)))))
  (mapc 'delete-overlay scim-preedit-overlays)
  (when (local-variable-p 'scim-cursor-type-saved)
    (if (eq scim-cursor-type-saved 1)
	(kill-local-variable 'cursor-type)
      (setq cursor-type scim-cursor-type-saved))
    (kill-local-variable 'scim-cursor-type-saved))
  (setq scim-preedit-prev-string ""
	scim-preedit-prev-curpos 0
	scim-preedit-prev-attributes nil
	scim-preedit-overlays nil)
  (set-marker scim-preedit-point nil)
  (when scim-keymap-overlay
    (delete-overlay scim-keymap-overlay)
    (setq scim-keymap-overlay nil))
  (setq scim-preediting-p nil))

(defun scim-cleanup-preedit (&optional abort)
  (scim-remove-preedit abort)
;#  (scim-log "cleanup preedit")
  (setq scim-preedit-update nil
	scim-preedit-shown ""
	scim-preedit-string ""
	scim-preedit-curpos 0
	scim-preedit-attributes nil
	scim-committed-string ""))

(defun scim-show-preedit (&optional resume)
  (setq scim-preedit-update nil)
  (if resume (setq scim-surrounding-text-modified t))
  (let* ((str scim-preedit-string)
	 (attrs scim-preedit-attributes)
	 (empty (or (string= scim-preedit-shown "FALSE")
		    (string= str ""))))
    (cond
     ;; isearch-mode
     ((and scim-isearch-minibuffer
	   scim-surrounding-text-modified
	   (not empty))
;#      (scim-log "preediting text not shown")
      (add-to-list 'unread-command-events 'scim-resume-preedit))
     ;; IMContext is empty or invisible
     (empty
      (scim-cleanup-preedit))
     ;; IMContext contains preedit string
     (resume
      (setq scim-preedit-update t))
     ;; Change only cursor position
     ((and scim-preediting-p
	   (string= str scim-preedit-prev-string)
	   (equal attrs scim-preedit-prev-attributes)
	   (or (= scim-preedit-curpos scim-preedit-prev-curpos)
	       (not (or (member "highlight" attrs)
			(member "reverse" attrs)))))
      (unless (= scim-preedit-curpos scim-preedit-prev-curpos)
	(goto-char (+ scim-preedit-point scim-preedit-curpos))
	(scim-set-cursor-location t)
	(setq scim-preedit-prev-curpos scim-preedit-curpos)))
     (t
      (if scim-preediting-p
	  (scim-remove-preedit)
	(if (eq window-system 'x)
	    (scim-frame-top-left-coordinates)))
      ;; Put String
      (setq scim-preediting-p (current-buffer))
      (setq scim-keymap-overlay (make-overlay (point-min) (1+ (point-max)) nil nil t))
      (overlay-put scim-keymap-overlay 'keymap scim-mode-preedit-map)
      (overlay-put scim-keymap-overlay 'priority 100) ; override yasnippet's keymap
      (set-marker scim-preedit-point (point))
;#;      (scim-log "current cursor position: %d" scim-preedit-point)
      (mapc (lambda (overlay)
	      (when (overlay-get overlay 'yas/modified?)
		(overlay-put overlay 'scim-saved-yas/modified? t)))
	    (overlays-at scim-preedit-point))
      (undo-boundary)
      (condition-case err
	  (insert-and-inherit str)
	(text-read-only
	 (scim-message "Failed to insert preediting text %S" err)
	 (scim-cleanup-preedit)
	 (let ((scim-string-insertion-failed nil))
	   (scim-reset-imcontext))
	 (setq str ""
	       scim-string-insertion-failed t)))
      (undo-boundary)
      (unless (string= str "")
	(setq scim-preedit-prev-string str
	      scim-preedit-prev-curpos scim-preedit-curpos
	      scim-preedit-prev-attributes attrs)
	;; Set attributes
;#;	(scim-log "attributes: %s" attrs)
	(let* ((max (length str))
	       (ol (make-overlay scim-preedit-point
				 (+ scim-preedit-point max)))
	       highlight)
	  (overlay-put ol 'face 'scim-preedit-default-face)
	  (overlay-put ol 'priority 0)
	  (setq scim-preedit-overlays (list ol))
	  (while attrs
	    (let* ((beg (min (max (string-to-number (pop attrs)) 0) max))
		   (end (min (string-to-number (pop attrs)) max))
		   (type (pop attrs))
		   (value (pop attrs))
		   fc pr)
;#;	      (scim-log "beg: %d  end: %d  type: %s  val: %s" begin end type value)
	      (if (cond ((and (string= type "foreground")
			      (scim-check-rgb-color value))
			 (setq fc (list :foreground value)
			       pr 50))
			((and (string= type "background")
			      (scim-check-rgb-color value))
			 (setq fc (list :background value)
			       pr 50))
			((and (string= type "decoreate")
			      (setq fc (scim-select-face-by-attr value)))
			 (unless (eq fc 'scim-preedit-underline-face)
			   (setq highlight t))
			 (setq pr 100)))
		  (let ((ol (make-overlay (+ scim-preedit-point beg)
					  (+ scim-preedit-point end))))
		    (overlay-put ol 'face fc)
		    (overlay-put ol 'priority pr)
		    (push ol scim-preedit-overlays))
		(scim-message "Unable to set attribute %S %S." type value))))
	  ;; This modification hook must be registered as a global hook because
	  ;; local hooks might be reset when major mode is changed.
	  (add-hook 'before-change-functions 'scim-before-change-function)
;#	  (scim-log "highlighted: %s" highlight)
	  (if highlight
	      ;; When conversion candidate is shown
	      (progn
		(unless (or (eq scim-cursor-type-for-candidate 0)
			    (local-variable-p 'scim-cursor-type-saved))
		  (setq scim-cursor-type-saved
			(or (and (local-variable-p 'cursor-type) cursor-type)
			    1))) ; 1 means that global value has been used
		(setq scim-preedit-curpos (min scim-preedit-curpos max))
		(if scim-put-cursor-on-candidate
		    (goto-char (+ scim-preedit-point scim-preedit-curpos)))
		(scim-set-cursor-location))
	    ;; When the string is preedited or prediction window is shown
	    (goto-char (+ scim-preedit-point scim-preedit-curpos))
	    (scim-set-cursor-location t)))
	(run-hooks 'scim-preedit-show-hook))
      ))))

(defun scim-do-update-preedit ()
  (when scim-preedit-update
;#    (scim-log "preedit-update  win-buf: %S  cur-buf: %S  cmd-buf: %S  str: %S" (window-buffer) (current-buffer) scim-current-buffer scim-preedit-string)
    (scim-show-preedit)
    (unless scim-string-insertion-failed
      (scim-preedit-updated))))

(defun scim-abort-preedit ()
  (when scim-preediting-p
    (save-excursion
      (scim-cleanup-preedit (not scim-clear-preedit-when-unexpected-event)))
    (scim-reset-imcontext)))

(defun scim-before-change-function (&optional beg end)
  (when (eq (current-buffer) scim-current-buffer)
;#    (scim-log "buffer will be modified (beg:%s  end:%s)" beg end)
;#    (scim-log "cursor positon: %s" (point))
    (unless (and (memq major-mode '(erc-mode
				    rcirc-mode
				    circe-server-mode
				    circe-channel-mode))
		 (memq this-command '(nil scim-handle-event)))
      (scim-abort-preedit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manage buffer switching
(defun scim-buffer-group-identifier ()
  (or (not scim-mode-local)
      (current-buffer)))

(defun scim-buffer-group-suitable-p ()
  (eq (eq scim-buffer-group t)
      (not scim-mode-local)))

(defun scim-check-current-buffer ()
;#;  (scim-log "check current buffer")
  (catch 'exit
    (scim-cancel-focus-update-timer)
    (setq scim-last-rejected-event nil)
    (with-current-buffer (window-buffer)
      (let ((buffer (current-buffer))
	    (visited-p scim-buffer-group)
	    (non-x-p (not (eq window-system 'x)))
	    (display-unchanged-p (or (eq (selected-frame) scim-selected-frame)
				     (equal (scim-get-x-display)
					    scim-selected-display))))
	;; Switch IMContext between global and local
	(unless (or non-x-p
		    (not visited-p)
		    (scim-buffer-group-suitable-p))
	  (setq visited-p nil)
	  (if (eq buffer scim-current-buffer)
	      (scim-deregister-imcontext)
	    (let ((scim-current-buffer buffer))
	      (scim-deregister-imcontext))))
	;; Change focus if buffer is switched to another one or display is changed
	(unless (and (eq buffer scim-current-buffer)
		     (if non-x-p
			 (null scim-imcontext-id)
		       (and scim-imcontext-id
			    display-unchanged-p)))
	  ;; Focus out from previous buffer
;#	  (scim-log "buffer was changed from %S to %S" scim-current-buffer buffer)
	  (when (buffer-live-p scim-current-buffer)
	    (with-current-buffer scim-current-buffer
	      (when (stringp scim-imcontext-id)
		(when scim-frame-focus
		  (scim-change-focus nil) ; Send
		  (scim-bridge-receive)) ; Receive
		(if scim-preediting-p
		    ;; Cleenup preedit if focus change become timeout
		    (scim-abort-preedit)))))
	  ;; Setup currently selected buffer
	  (unless display-unchanged-p
	    (condition-case err
		(scim-change-x-display)
	      (error
	       (scim-message "%s: %s" (car err) (if (cddr err) (cdr err) (cadr err)))
	       (if scim-mode (scim-mode-quit))
	       (throw 'exit nil))))
	  (setq scim-current-buffer buffer)
	  (let* ((group-id (or scim-buffer-group
			       scim-parent-buffer-group
			       (scim-buffer-group-identifier)))
		 (group (assq group-id scim-buffer-group-alist)))
	    (setq scim-imcontext-id (cdr (assoc scim-selected-display
						(cadr group)))
		  scim-imcontext-status (cdr (assoc scim-selected-display
						    (nth 2 group))))
	    (unless scim-buffer-group
	      (setq scim-buffer-group group-id)
	      (when scim-parent-buffer-group
		;; Inherit IMContext
;#		(scim-log "inherit IMContext (buffer group: %s)" group-id)
		(setcar (nthcdr 3 group)
			(cons buffer (delq buffer (nth 3 group)))))
	      (add-hook 'kill-buffer-hook 'scim-kill-buffer-function nil t)))
	  ;; Check whether buffer is already registered
	  (unless (or non-x-p
		      (and visited-p scim-imcontext-id))
;#	    (scim-log "new buffer was detected: %S" buffer)
	    (condition-case err
		(scim-register-imcontext)
	      (error
	       (scim-message "%s: %s" (car err) (if (cddr err) (cdr err) (cadr err)))
	       (if scim-mode (scim-mode-quit))
	       (throw 'exit nil))))
	  ;; `scim-preedit-string' not empty means
	  ;; continuous preediting of incremental search
	  (when (string= scim-preedit-string "")
	    ;; Focus in if window is active
	    (setq scim-frame-focus nil)
	    (if (stringp scim-imcontext-id)
		(scim-check-frame-focus t)))
	  (scim-set-keymap-parent)
	  (scim-update-cursor-color)))
      (setq scim-parent-buffer-group nil)
      ;; Disable keymap if buffer is read-only, explicitly disabled, or vi-mode.
      (if (eq (and (or buffer-read-only
		       scim-mode-map-disabled
		       (eq major-mode 'vi-mode)
		       (and (boundp 'vip-current-mode)
			    (eq vip-current-mode 'vi-mode))
		       (and (boundp 'viper-current-state)
			    (eq viper-current-state 'vi-state)))
		   (not (and isearch-mode
			     scim-use-kana-ro-key
			     scim-kana-ro-x-keysym)))
	      (not scim-mode-map-prev-disabled))
	  (scim-switch-keymap scim-mode-map-prev-disabled))
      ;; Set/restore cursor shape
      (when (local-variable-p 'scim-cursor-type-saved)
	(cond
	 (scim-preediting-p
	  (setq cursor-type scim-cursor-type-for-candidate))
	 (isearch-mode
	  (setq cursor-type scim-isearch-cursor-type))
	 (t
	  (if (eq scim-cursor-type-saved 1)
	      (kill-local-variable 'cursor-type)
	    (setq cursor-type scim-cursor-type-saved))
	  (kill-local-variable 'scim-cursor-type-saved))))
      ;; Check selected frame
      (unless (eq (selected-frame) scim-selected-frame)
	(when (and scim-preediting-p
		   (eq window-system 'x))
	  (scim-frame-top-left-coordinates)
	  (scim-remove-preedit)
	  (scim-show-preedit))
	(setq scim-selected-frame (selected-frame))
	(scim-update-cursor-color)))
    (scim-start-focus-observation)))

(defun scim-kill-buffer-function ()
  (scim-deregister-imcontext))

(defun scim-exit-minibuffer-function ()
  (if scim-imcontext-temporary-for-minibuffer
      (scim-deregister-imcontext)))

;; Advices for `anything.el'

(defadvice anything-read-pattern-maybe
  (before scim-fix-hook-anything-read-pattern-maybe ())
  (if scim-mode
      (add-hook 'post-command-hook 'scim-check-current-buffer)))

(defadvice anything-isearch-post-command
  (before scim-fix-hook-anything-isearch-post-command ())
  (if (and scim-mode
	   (not (memq 'scim-check-current-buffer
		      (default-value 'post-command-hook))))
      (scim-check-current-buffer)))

(defun scim-activate-advices-fix-post-command-hook (enable)
  (if enable
      (ad-enable-regexp "^scim-fix-hook-")
    (ad-disable-regexp "^scim-fix-hook-"))
  (ad-activate-regexp "^scim-fix-hook-"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INHERIT-INPUT-METHOD
(defun scim-defadvice-inherit-imcontext ()
  (mapc (lambda (command)
	  (eval
	   `(defadvice ,command
	      (around ,(intern (concat "scim-inherit-im-" (symbol-name command))) ())
	      (if (and (with-no-warnings
			 (or (null (assq 'inherit-input-method ad-arg-bindings))
			     inherit-input-method
			     scim-force-inherit-im))
		       (stringp scim-imcontext-id))
		  (let ((scim-force-inherit-im t))
		    (setq scim-parent-buffer-group scim-buffer-group)
		    ad-do-it)
		ad-do-it))))
	scim-inherit-im-functions))

(defun scim-activate-advices-inherit-im (enable)
  (if enable
      (ad-enable-regexp "^scim-inherit-im-")
    (ad-disable-regexp "^scim-inherit-im-"))
  (ad-activate-regexp "^scim-inherit-im-"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with agent through an UNIX domain socket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disconnect
(defun scim-bridge-disconnect ()
  (let ((proc scim-bridge-socket))
    (condition-case err
	(when (processp proc)
	  (set-process-sentinel proc nil)
	  (let ((buffer (process-buffer proc)))
	    (when (buffer-live-p buffer)
	      (with-current-buffer buffer
		(remove-hook 'after-change-functions
			     'scim-bridge-receive-passively t))
	      (kill-buffer buffer)))
	  (delete-process proc)
;#	  (scim-log "process: %s  status: %s" proc (process-status proc))
	  )
      (error (scim-message "%S: %S" (car err) (cdr err))))
    (setq scim-bridge-socket nil)))

(defun scim-bridge-process-sentinel (proc stat)
;#  (scim-log "process: %s  status: %s" proc (substring stat 0 -1))
  (scim-mode-quit)
  (if (scim-mode-on) ; Try to restart
      ;; Succeeded
      (progn
	(scim-message "scim-mode restarted")
	(scim-check-current-buffer))
    ;; Failed
    (scim-message "Socket was unexpectedly closed. Turned off scim-mode.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connect
(defun scim-bridge-connect-internal ()
  (let* ((display (scim-get-x-display))
	 (socket (concat scim-bridge-socket-path-common display))
	 (buffer (progn
		   (string-match "\\(\\**\\)$" scim-tmp-buffer-name)
		   (replace-match (concat "(" display ")\\1")
				  t nil scim-tmp-buffer-name)))
	 (i 0)
	 proc error)
    (unless (file-exists-p socket)
      (scim-message "Launch SCIM-Bridge..."))
    (call-process-shell-command scim-bridge-name nil 0 nil "--noexit")
    (while (and (not (processp proc))
		(< i 10)) ; Try connection 10 times at maximum
      (sleep-for (* 0.1 i))
      (setq proc (condition-case err
		     (make-network-process
		      :name scim-bridge-name
		      :service socket
		      :buffer buffer
		      :family 'local :server nil :noquery t)
		   (error
		    (setq error err)
		    nil))
	    i (1+ i)))
    (unless (processp proc)
      (scim-message "%S: %S" (car error) (cdr error)))
    proc))

(defun scim-bridge-connect ()
  (if (and (processp scim-bridge-socket)
	   (not (memq (process-status scim-bridge-socket) '(open run))))
      (scim-bridge-disconnect))
  (unless (and (processp scim-bridge-socket)
	       (memq (process-status scim-bridge-socket) '(open run)))
    (let ((proc (scim-bridge-connect-internal)))
      (setq scim-bridge-socket proc)
      (when (processp proc)
;#	(scim-log "process: %s  status: %s" proc (process-status proc))
	;; `process-kill-without-query' is an obsolete function (as of Emacs 22.1)
;	(process-kill-without-query proc)
	(set-process-query-on-exit-flag proc nil)
	(set-process-coding-system proc 'utf-8 'utf-8)
	(set-process-sentinel proc 'scim-bridge-process-sentinel)
	(with-current-buffer (process-buffer proc)
;#	  (scim-log "temp buffer: %S" (current-buffer))
	  (unless scim-debug (buffer-disable-undo))
	  (erase-buffer)
	  ;; `make-local-hook' is an obsolete function (as of Emacs 21.1)
;	  (make-local-hook 'after-change-functions)
	  (add-hook 'after-change-functions
		    'scim-bridge-receive-passively nil t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communicate with agent
(defun scim-bridge-receive (&optional passive)
  (let (repl)
    (save-current-buffer
      (when (or passive
		(and (processp scim-bridge-socket)
		     (set-buffer (process-buffer scim-bridge-socket))))
	(let ((inhibit-modification-hooks t)
	      (sec (and (floatp scim-bridge-timeout) scim-bridge-timeout))
	      (msec (and (integerp scim-bridge-timeout) scim-bridge-timeout)))
	  (when (= (point-max) 1)
	    (accept-process-output scim-bridge-socket sec msec t))
	  (when (and (> (point-max) 1)
		     (/= (char-before (point-max)) ?\n))
;#	    (scim-log "retry data reception")
	    (accept-process-output scim-bridge-socket sec msec t))
	  (setq repl (buffer-string))
	  (erase-buffer)
;#	  (scim-log "receive:\n%s" repl)
	  (setq unread-command-events
		(delq 'scim-receive-event unread-command-events)))
	(if (string= repl "")
	    (scim-message "Data reception became timeout.")
	  (scim-parse-reply (scim-split-commands repl) passive))))))

(defun scim-bridge-send-only (command)
  (condition-case err
      (progn
	(with-current-buffer (process-buffer scim-bridge-socket)
	  (let ((inhibit-modification-hooks t))
	    (erase-buffer)))
;#	(scim-log "process: %s  status: %s" scim-bridge-socket (process-status scim-bridge-socket))
;#	(scim-log "send: %S" command)
	(process-send-string scim-bridge-socket (concat command "\n"))
	t) ; Succeeded
    (error
     (scim-message "Couldn't send command to agent %S" err)
     nil))) ; Failed

(defun scim-bridge-send-receive (command)
  (and (scim-bridge-send-only command)
       (scim-bridge-receive)))

(defun scim-bridge-receive-passively (beg &optional end lng)
;#  (scim-log "passively receive")
  (scim-bridge-receive t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Send command to agent
(defun scim-register-imcontext ()
  (let ((group (assq scim-buffer-group scim-buffer-group-alist)))
    (if group
	(setcar (nthcdr 3 group)
		(cons (current-buffer)
		      (delq (current-buffer) (nth 3 group))))
      (setq group (list scim-buffer-group
			nil nil
			(list (current-buffer)))
	    scim-buffer-group-alist (cons group scim-buffer-group-alist)))
    (unless scim-imcontext-id
      (setq scim-imcontext-id 'RQ) ; Set symbol to avoid multiple request
      (let ((time-limit (+ (float-time)
			   (or (and (floatp scim-bridge-timeout)
				    scim-bridge-timeout)
			       (/ scim-bridge-timeout 1000.0)))))
	(scim-bridge-send-receive "register_imcontext")
	(while (and (not (stringp scim-imcontext-id))
		    (< (float-time) time-limit))
	  (scim-bridge-receive)))
      (unless (stringp scim-imcontext-id)
	(scim-mode-quit)
	(error "Couldn't register imcontext. Turned off scim-mode."))
      (setcdr group
	      (list (cons (cons scim-selected-display scim-imcontext-id)
			  (cadr group))
		    (cons (cons scim-selected-display scim-imcontext-status)
			  (nth 2 group))
		    (nth 3 group)))
      (scim-cleanup-preedit)
      (scim-set-preedit-mode))))

(defun scim-deregister-imcontext () ;(id)
  (if (and (stringp scim-imcontext-id)
	   scim-frame-focus)
      (scim-change-focus nil))
  (let ((group (assq scim-buffer-group scim-buffer-group-alist)))
    (when (and group
	       (null (setcar (nthcdr 3 group)
			     ;; Remove current buffer from imcontext group
			     (delq (current-buffer) (nth 3 group)))))
      (mapc (lambda (pair)
	      (let* ((scim-selected-display (car pair))
		     (scim-bridge-socket (cdr (assoc scim-selected-display
						     scim-bridge-socket-alist)))
		     (scim-imcontext-id (cdr pair)))
		(if (stringp scim-imcontext-id)
		    (scim-bridge-send-receive
		     (concat "deregister_imcontext " scim-imcontext-id)))))
	    (cadr group))
      ;; Even if IMContext is not deregistered yet, reset variables
      ;; immediately here, because `scim-imcontext-deregister' callback
      ;; can't receive IMContext ID.
      (setq scim-imcontext-id nil
	    scim-imcontext-status nil
	    scim-buffer-group-alist (assq-delete-all scim-buffer-group
						     scim-buffer-group-alist))))
  (kill-local-variable 'scim-buffer-group)
  (if (eq scim-current-buffer (current-buffer))
      (setq scim-current-buffer nil)))

(defun scim-reset-imcontext () ;(id)
;#;  (scim-log "buffer: %S" (current-buffer))
  (scim-bridge-send-receive
   (concat "reset_imcontext " scim-imcontext-id)))

(defun scim-set-preedit-mode () ;(id mode)
  (scim-bridge-send-receive
   (concat "set_preedit_mode " scim-imcontext-id " embedded")))

(defun scim-change-focus (focus-in) ;(id focus-in)
;  (unless (equal (buffer-name) "aaa.el") ; Debug code -- DON'T UNCOMMENT!!
  (scim-bridge-send-only ; Result is not received in this function
   (concat "change_focus " scim-imcontext-id
	   (if focus-in " true" " false"))))
;  ) ; Debug code

(defun scim-set-cursor-location (&optional prediction) ;(id x y)
  (let* ((pixpos (scim-compute-pixel-position
		  (if (and prediction
			   (car scim-prediction-window-position)
			   (not (minibufferp)))
		      scim-preedit-point
		    (+ scim-preedit-point scim-preedit-curpos))
		  nil scim-saved-frame-coordinates))
	 (x-y (format "%d %d"
		      (if (or (and prediction
				   (null (cdr scim-prediction-window-position)))
			      (minibufferp))
			  (car pixpos)
			(max (- (car pixpos) scim-adjust-window-x-offset) 1))
		      (cdr pixpos))))
;#    (scim-log "cursor position (x y): %s" x-y)
    (unless (equal x-y scim-cursor-prev-location)
      (setq scim-cursor-prev-location x-y)
      (scim-bridge-send-only
       (concat "set_cursor_location " scim-imcontext-id " " x-y)))))

(defun scim-handle-key-event (key-code key-pressed modifiers) ;(id key-code key-pressed &rest modifiers)
  (scim-bridge-send-receive
   (scim-construct-command
    (append (list "handle_key_event"
		  scim-imcontext-id key-code key-pressed)
	    modifiers))))

(defun scim-preedit-updated ()
  (scim-bridge-send-only "preedit_updated"))

(defun scim-string-commited ()
  (scim-bridge-send-only "string_commited"))

(defun scim-surrounding-text-gotten (retval cursor-position string)
  (scim-bridge-send-receive
   (scim-construct-command
    (cons "surrounding_text_gotten"
	  (if retval
	      (list "true" (number-to-string cursor-position) string)
	    (list "false"))))))

(defun scim-surrounding-text-deleted (retval)
  (scim-bridge-send-receive
   (concat "surrounding_text_deleted " (if retval "true" "false"))))

(defun scim-surrounding-text-replaced (retval)
  (scim-bridge-send-receive
   (concat "surrounding_text_replaced " (if retval "true" "false"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process commands from agent to clients (Callbacks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scim-imengine-status-changed (id enabled)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (let ((status (equal enabled "TRUE")))
      (setq scim-imcontext-status status)
      (setcdr (assoc scim-selected-display
		     (nth 2 (assq scim-buffer-group scim-buffer-group-alist)))
	      status)
      (scim-update-cursor-color)
      (scim-set-keymap-parent)
      (when (and scim-use-kana-ro-key
		 scim-kana-ro-x-keysym
		 scim-use-minimum-keymap)
	(scim-update-kana-ro-key)))))

(defun scim-preedit-mode-changed ()
  t)

(defun scim-imcontext-registered (id)
  ;; Initialize IMContext
;#  (scim-log "imcontext registered (id: %s  buf: %S)" id (if scim-mode-local (current-buffer) "global"))
  (setq scim-imcontext-id id
	scim-imcontext-status nil))

(defun scim-imcontext-deregister ()
  t)

(defun scim-imcontext-reseted (id)
  t)

(defun scim-cursor-location-changed ()
  t)

(defun scim-key-event-handled (consumed)
  (if (or (string= consumed "true")
	  (null scim-last-command-event))
      ;; If key event is handled
      (when scim-last-command-event
	;; Send cursor location for displaying SCIM-Ruby history window
	(when (and (not scim-preediting-p)
		   (not scim-preedit-update)
		   (string= scim-preedit-string ""))
	  (let ((scim-preedit-point (point))
		(scim-adjust-window-x-offset 0))
	    (scim-set-cursor-location)))
	(setq scim-last-command-event nil))
    ;; If key event is ignored
    (let* ((vec (vector scim-last-command-event))
	   (event (or (and (boundp 'local-function-key-map)
			   (lookup-key local-function-key-map vec))
		      (lookup-key function-key-map vec)))
	   keybind)
      (setq event (or (and (arrayp event)
			   (aref event 0))
		      scim-last-command-event))
      (let ((scim-mode-map-alist nil)) ; Disable keymap temporarily
	(if scim-keymap-overlay
	    (overlay-put scim-keymap-overlay 'keymap nil))
	(setq keybind (key-binding (vector event)))
	(if (and (null keybind)
		 (integerp event)
		 (memq 'shift (event-modifiers event)))
	    ;; Reset the 25th bit corresponding to the shift key
	    (setq event (logand event (lognot ?\x2000000))
		  keybind (key-binding (vector event))))
	(if scim-keymap-overlay
	    (overlay-put scim-keymap-overlay 'keymap scim-mode-preedit-map)))
;#      (scim-log "event: --> %s --> %s" scim-last-command-event event)
      (if (or (eq scim-last-command-event scim-last-rejected-event)
	      (eq keybind this-command)
	      isearch-mode)
	  (progn
	    (scim-message "%s is undefined"
			  (single-key-description scim-last-command-event))
	    (if isearch-mode
		(isearch-done)))
	(if (memq keybind '(self-insert-command
			    *table--cell-self-insert-command))
	    ;; Self-insert command
	    (progn
	      (scim-do-update-preedit)
;#	      (scim-log "execute command: %s" keybind)
	      (setq scim-last-rejected-event scim-last-command-event
		    scim-last-command-event nil
		    last-command-event event
		    last-command scim-last-command
		    this-command keybind)
	      (unwind-protect
		  (if (and (eq keybind 'self-insert-command)
			   (eq scim-last-command 'self-insert-command))
		      (scim-insert-and-modify-undo-list (char-to-string event))
		    (command-execute keybind)
		    (if (eq keybind '*table--cell-self-insert-command)
			(with-no-warnings
			  (table--finish-delayed-tasks))))
		(setq scim-last-rejected-event nil)))
	  ;; The other commands
;#	  (scim-log "event rejected: %s" scim-last-command-event)
	  (if scim-keymap-overlay
	      (overlay-put scim-keymap-overlay 'keymap nil))
	  (setq scim-mode-map-alist nil
		this-command scim-last-command
		unread-command-events
		(cons scim-last-command-event unread-command-events))
	  (remove-hook 'post-command-hook 'scim-check-current-buffer)
	  (add-hook 'pre-command-hook 'scim-fallback-pre-function)))))
  (setq scim-last-rejected-event scim-last-command-event
	scim-last-command-event nil))

(defun scim-fallback-pre-function ()
  (remove-hook 'pre-command-hook 'scim-fallback-pre-function)
  (add-hook 'post-command-hook 'scim-check-current-buffer)
  (add-hook 'post-command-hook 'scim-fallback-post-function))

(defun scim-fallback-post-function ()
  (remove-hook 'post-command-hook 'scim-fallback-post-function)
  (if scim-keymap-overlay
      (overlay-put scim-keymap-overlay 'keymap scim-mode-preedit-map))
  (scim-set-mode-map-alist))

(defun scim-update-preedit (id)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-preedit-update t)))

(defun scim-set-preedit-string (id string)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-preedit-string string)))

(defun scim-set-preedit-attributes (id &rest attrs)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-preedit-attributes attrs)))

(defun scim-set-preedit-cursor-position (id position)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
;#    (scim-log "cursor position: %s" position)
    (setq scim-preedit-curpos (string-to-number position))))

(defun scim-set-preedit-shown (id shown)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (setq scim-preedit-shown shown)))

(defun scim-set-commit-string (id string)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
;#    (scim-log "commit string: %S" string)
    (setq scim-committed-string string)
    (run-hooks 'scim-set-commit-string-hook)))

(defun scim-*table--cell-insert (string)
  (with-no-warnings
    (table--finish-delayed-tasks)
    (table-recognize-cell 'force)
    (dont-compile ; To avoid byte-compile error
      (table-with-cache-buffer
       (insert-and-inherit string)
       (table--untabify (point-min) (point-max))
       (table--fill-region (point-min) (point-max))
       (setq table-inhibit-auto-fill-paragraph t)))
    (table--finish-delayed-tasks)))

(defun scim-commit-string (id)
  (cond
   ((not (string= id scim-imcontext-id))
    (scim-message "IMContext ID (%s) is mismatched." id))
   (isearch-mode
    (isearch-process-search-string scim-committed-string scim-committed-string)
    (scim-string-commited))
   (buffer-read-only
    (scim-message "Buffer is read-only: %S" (current-buffer)))
   ((not scim-string-insertion-failed)
    (scim-remove-preedit)
    (condition-case err
	(progn
	  (cond
	   ;; ansi-term
	   ((and (eq major-mode 'term-mode)
		 (get-buffer-process (current-buffer)))
	    (with-no-warnings
	      (term-send-raw-string scim-committed-string)))
	   ;; table-mode
	   ((and (featurep 'table)
		 (with-no-warnings table-mode-indicator))
	    (scim-*table--cell-insert scim-committed-string))
	   ;; Normal commit
	   (scim-undo-by-committed-string
	    (insert-and-inherit scim-committed-string))
	   ;; Normal commit (Undoing will be performed every 20 characters)
	   (t
	    (scim-insert-and-modify-undo-list scim-committed-string)))
	  (setq scim-last-command 'self-insert-command)
	  (scim-string-commited)
	  (run-hooks 'scim-commit-string-hook))
      (text-read-only
       (scim-message "Failed to commit string %S" err)
       (setq scim-string-insertion-failed t)))
    (scim-show-preedit t))))

(defun scim-forward-key-event (id key-code key-pressed &rest modifiers)
  (let ((event (scim-encode-event key-code modifiers)))
    (if event
	(if (string= key-pressed "TRUE")
	    (setq unread-command-events (cons event unread-command-events)))
      (if (not (string= id scim-imcontext-id))
	  (scim-message "IMContext ID (%s) is mismatched." id)
	(scim-handle-key-event key-code key-pressed modifiers)))))

(defun scim-get-surrounding-text (id before-max after-max)
  (if (not (string= id scim-imcontext-id))
      (scim-message "IMContext ID (%s) is mismatched." id)
    (let* ((len-before (scim-twos-complement before-max))
	   (len-after (scim-twos-complement after-max))
	   (end-before (if scim-preediting-p
			   scim-preedit-point
			 (point)))
	   (beg-after (if scim-preediting-p
			  (+ end-before (length scim-preedit-prev-string))
			end-before))
	   (beg-before (if (>= len-before 0)
			   (max (- end-before len-before) (point-min))
			 (line-beginning-position)))
	   (end-after (if (>= len-after 0)
			  (min (+ beg-after len-after) (point-max))
			(line-end-position)))
	   (str-before (buffer-substring-no-properties beg-before end-before))
	   (str-after (buffer-substring-no-properties beg-after end-after))
	   (string (concat str-before str-after))
	   (cursor (length str-before))
	   (retval (> (length string) 0)))
;#      (scim-log "val: %S  str: %S  pos: %d" retval string cursor)
      (scim-surrounding-text-gotten retval cursor string))))

(defun scim-*table--cell-delete-region (beg end)
  (push-mark beg t t)
  (goto-char end)
  (call-interactively '*table--cell-delete-region))

(defun scim-delete-surrounding-text (id offset length)
  (cond
   ((not (string= id scim-imcontext-id))
    (scim-message "IMContext ID (%s) is mismatched." id))
   (buffer-read-only
    (scim-message "Buffer is read-only: %S" (current-buffer)))
   ((not scim-string-insertion-failed)
;#    (scim-log "delete surrounding text")
    (scim-remove-preedit)
    (let* ((pos (point))
	   (beg (+ pos (scim-twos-complement offset)))
	   (end (+ beg (string-to-number length)))
	   (retval t))
      (condition-case err
	  (cond
	   ((and (featurep 'table)
		 (with-no-warnings table-mode-indicator))
	    (scim-*table--cell-delete-region beg end))
	   (t
	    (delete-region beg end)))
	(text-read-only
	 (scim-message "Failed to delete surrounding text %S" err)
	 (setq retval nil
	       scim-string-insertion-failed t)))
      (scim-show-preedit t)
      (scim-surrounding-text-deleted retval)))))

(defun scim-replace-surrounding-text (id corsor-index string)
  (cond
   ((not (string= id scim-imcontext-id))
    (scim-message "IMContext ID (%s) is mismatched." id))
   (buffer-read-only
    (scim-message "Buffer is read-only: %S" (current-buffer)))
   ((not scim-string-insertion-failed)
;#    (scim-log "replace surrounding text")
    (scim-remove-preedit)
    (let* ((pos (point))
	   (beg (- pos (scim-twos-complement corsor-index)))
	   (end (+ beg (length string)))
	   (retval t))
      (condition-case err
	  (cond
	   ((and (featurep 'table)
		 (with-no-warnings table-mode-indicator))
	    (scim-*table--cell-delete-region beg end)
	    (scim-*table--cell-insert string))
	   (t
	    (delete-region beg end)
	    (goto-char beg)
	    (insert-and-inherit string)))
	(text-read-only
	 (scim-message "Failed to replace surrounding text %S" err)
	 (setq retval nil
	       scim-string-insertion-failed t)))
      (goto-char pos)
      (scim-show-preedit t)
      (scim-surrounding-text-replaced retval)))))

(defun scim-focus-changed ()
  t)

(defun scim-beep (id)
  (ding t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execute commands replied from agent
(defun scim-exec-callback-1 (sexplist)
;#  (scim-log "buffer: %s" (current-buffer))
;#  (scim-log "display: %s" scim-selected-display)
;#  (scim-log "imcontext-id: %s" scim-imcontext-id)
  (mapc (lambda (sexp)
;#	  (scim-log "execute: %S" sexp)
	  (eval sexp))
	sexplist))

(defun scim-exec-callback ()
  (interactive)
  (when (interactive-p)
    (unless (eq last-command 'scim-handle-event)
      (setq scim-string-insertion-failed nil))
    (setq this-command last-command
	  unread-command-events (delq 'scim-receive-event unread-command-events)))
  (when (buffer-live-p scim-current-buffer)
    (with-current-buffer scim-current-buffer
;#      (scim-log "callback queue: %s" (pp-to-string scim-callback-queue))
      (while scim-callback-queue
	(let* ((queue (car scim-callback-queue))
	       (scim-bridge-socket (car queue))
	       (scim-selected-display (car (rassoc scim-bridge-socket
						   scim-bridge-socket-alist)))
	       (group (assq scim-buffer-group scim-buffer-group-alist))
	       (scim-imcontext-id (cdr (assoc scim-selected-display
					      (cadr group))))
	       (scim-imcontext-status (cdr (assoc scim-selected-display
						  (nth 2 group)))))
	  (setq scim-callback-queue (cdr scim-callback-queue))
	  (scim-exec-callback-1 (cdr queue))))
      (let ((group (assq scim-buffer-group scim-buffer-group-alist)))
	(setq scim-imcontext-id (cdr (assoc scim-selected-display
					    (cadr group)))
	      scim-imcontext-status (cdr (assoc scim-selected-display
						(nth 2 group)))))
      (scim-do-update-preedit))))

(defun scim-parse-reply (cmdlist &optional passive)
  (let (rsexplist)
    (while cmdlist
      (let* ((args (pop cmdlist))
	     (callback (cdr (assoc (car args) scim-reply-alist))))
	(cond
	 ((memq callback scim-ignored-signal-list)
;#	  (scim-log "ignore: %S" args)
	  )
	 (callback
	  (setq rsexplist (cons (cons callback (cdr args)) rsexplist)))
	 (t
	  (scim-message (mapconcat 'identity args " "))
	  ))))
    (when rsexplist
;#      (scim-log "this-command: %s" this-command)
;#      (scim-log "last-command: %s" last-command)
;#      (scim-log "scim-last-command-event: %s" scim-last-command-event)
;#      (scim-log "before-change-functions: %s" before-change-functions)
      (if passive
	  (let ((queue1 (list (cons (get-buffer-process (current-buffer))
				    (nreverse rsexplist)))))
	    (if scim-callback-queue
		(nconc scim-callback-queue queue1)
	      (setq scim-callback-queue queue1))
	    (setq unread-command-events
		  (cons 'scim-receive-event
			(delq 'scim-receive-event unread-command-events))))
	(when (buffer-live-p scim-current-buffer)
	  (with-current-buffer scim-current-buffer
	    (scim-exec-callback-1 (nreverse rsexplist))
	    (scim-do-update-preedit)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process key events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scim-wait-following-key-event (prev-event key-code modifiers)
  (let ((event (read-event nil nil scim-simultaneous-pressing-time)))
    (scim-handle-key-event key-code "true" modifiers)
    (when (and event
	       (not scim-string-insertion-failed))
      (if (or (eq event prev-event)
	      (not (eq (key-binding (vector event)) 'scim-handle-event)))
	  (setq unread-command-events (cons event unread-command-events))
	(let ((scim-simultaneous-pressing-time nil))
	  (undo-boundary)
	  (setq this-command 'scim-handle-event)
	  (scim-dispatch-key-event event))))))

(defun scim-dispatch-key-event (event)
  (let* ((elist (scim-decode-event event))
	 (key-code (car elist))
	 (modifiers (cdr elist)))
    (when (numberp key-code)
      (unless scim-frame-focus (scim-check-frame-focus t))
      (scim-check-current-buffer))
;#;    (scim-log "event: %S" elist)
;#    (scim-log "event: %s" event)
    (when (member "kana_ro" modifiers)
      (setq event (event-convert-list
		   (append (event-modifiers event) (list key-code))))
;#      (scim-log "event: --> %s" event)
      (unless (eq (key-binding (vector event)) 'scim-handle-event)
	(setq key-code nil
	      unread-command-events (cons event unread-command-events))))
    (when key-code
      (setq scim-last-command-event event
	    scim-surrounding-text-modified nil)
      (if (and (stringp scim-imcontext-id)
	       (numberp key-code))
	  ;; Send a key event to agent
	  (let ((key-code (number-to-string key-code)))
	    (setq scim-string-insertion-failed nil)
	    (if (and scim-simultaneous-pressing-time
		     scim-imcontext-status)
		;; Thumb shift typing method
		(scim-wait-following-key-event event key-code modifiers)
	      (scim-handle-key-event key-code "true" modifiers))
	    (unless scim-string-insertion-failed
	      (scim-handle-key-event key-code "false" modifiers)))
	;; IMContext is not registered or key event is not recognized
	(scim-key-event-handled "false"))))
  ;; Repair post-command-hook
  (unless (memq 'scim-fallback-pre-function
		(default-value 'pre-command-hook))
    (when (and (local-variable-p 'post-command-hook)
	       (not (memq t post-command-hook)))
      (if post-command-hook
	  (add-to-list 'post-command-hook t t)
	(kill-local-variable 'post-command-hook)))
    (unless (memq 'scim-check-current-buffer
		  (default-value 'post-command-hook))
      (if (y-or-n-p "SCIM: `post-command-hook' was reset for some reasons. Try to repair this? ")
	  (add-hook 'post-command-hook 'scim-check-current-buffer)
	(scim-mode-off)))))

(defun scim-handle-event (&optional arg)
  (interactive "*p")
  (unless (eq last-command 'scim-handle-event)
    (setq scim-last-command last-command))
  (scim-dispatch-key-event last-command-event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup incremental search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions
(defun scim-isearch-start ()
  (if scim-preediting-p
      (scim-abort-preedit))
  (unless (or (eq scim-isearch-cursor-type 0)
	      (local-variable-p 'scim-cursor-type-saved))
    (setq scim-cursor-type-saved
	  (or (and (local-variable-p 'cursor-type) cursor-type)
	      1)))) ; 1 means that global value has been used

(defun scim-isearch-check-preedit ()
  (unless scim-preediting-p
    (with-current-buffer scim-isearch-minibuffer
      (exit-minibuffer))))

(defun scim-isearch-read-string-post-function ()
;#  (scim-log "isearch: exit SCIM input")
  (remove-hook 'post-command-hook 'scim-isearch-check-preedit)
  (remove-hook 'minibuffer-exit-hook 'scim-isearch-read-string-post-function t)
  (scim-isearch-start)
  (when (stringp scim-imcontext-id)
    (let ((scim-frame-focus nil)) ; To avoid focus out
      (scim-deregister-imcontext))))

(defun scim-isearch-read-string-pre-function ()
;#  (scim-log "isearch: start SCIM input")
  (remove-hook 'post-command-hook 'scim-isearch-read-string-pre-function)
  (add-hook 'post-command-hook 'scim-isearch-check-preedit t)
  (add-hook 'minibuffer-exit-hook 'scim-isearch-read-string-post-function nil t)
  (scim-show-preedit)
  (setq scim-isearch-minibuffer (current-buffer)))

(defun scim-isearch-process-search-characters (last-char)
  (let ((overriding-terminal-local-map nil)
	(prompt (isearch-message-prefix))
	(minibuffer-local-map (with-no-warnings isearch-minibuffer-local-map))
	(current-input-method nil)
	(scim-imcontext-temporary-for-minibuffer nil)
	(scim-isearch-minibuffer nil)
	(scim-current-buffer nil)
	str junk-hist)
    (add-hook 'post-command-hook 'scim-isearch-read-string-pre-function t)
    (if (eq (car unread-command-events) 'scim-resume-preedit)
	(setq unread-command-events (cdr unread-command-events))
      (setq unread-command-events (cons last-char unread-command-events)))
    (setq str (read-string prompt isearch-string 'junk-hist nil t)
	  isearch-string ""
	  isearch-message "")
;#    (scim-log "isearch-string: %S" str)
    (if (and str (> (length str) 0))
	(let ((unread-command-events nil))
	  (isearch-process-search-string str str))
      (isearch-update))
    (if (eq (car unread-command-events) 'scim-resume-preedit)
	(if (string= scim-preedit-string "")
	    (setq unread-command-events (cdr unread-command-events))
	  (setq unread-command-events (cons ?a unread-command-events))))))

(defun scim-isearch-other-control-char ()
  (if (and scim-use-kana-ro-key
	   (eq (event-basic-type last-command-event) scim-kana-ro-key-symbol))
      (setq unread-command-events
	    (if scim-imcontext-status
		(append (list ?a 'scim-resume-preedit last-command-event)
			unread-command-events)
	      (cons (event-convert-list
		     (append (event-modifiers last-command-event) (list ?\\)))
		    unread-command-events)))
    (funcall (lookup-key scim-mode-map (this-command-keys)))
    (if isearch-mode
	(isearch-update))))

;; Advices for `isearch.el'
(defadvice isearch-printing-char
  (around scim-isearch-printing-char ())
  (if scim-imcontext-status
      (let ((current-input-method "SCIM"))
	ad-do-it)
    ad-do-it))

(defadvice isearch-other-control-char
  (around scim-isearch-other-control-char ())
  (if (and scim-mode
	   ;; `lookup-key' returns nil if KEY is undefined.
	   ;; Otherwise, returns a number if KEY is too long
	   ;; (this maybe means the case that KEY is mouse event).
	   (commandp (lookup-key scim-mode-map (this-command-keys))))
      (scim-isearch-other-control-char)
    ad-do-it))

(defadvice isearch-message-prefix
  (around scim-isearch-message-prefix ())
  (if (and scim-imcontext-status
	   (not nonincremental)
	   (not (eq this-command 'isearch-edit-string)))
      (let ((current-input-method "SCIM")
	    (current-input-method-title "SCIM"))
	ad-do-it)
    ad-do-it))

;; Advices for `isearch-x.el'
(defadvice isearch-toggle-specified-input-method
  (around scim-isearch-toggle-specified-input-method ())
  (if scim-imcontext-status
      (isearch-update)
    ad-do-it))

(defadvice isearch-toggle-input-method
  (around scim-isearch-toggle-input-method ())
  (if scim-imcontext-status
      (isearch-update)
    ad-do-it))

(defadvice isearch-process-search-multibyte-characters
  (around scim-isearch-process-search-characters ())
  (if (and (string= current-input-method "SCIM")
	   (eq this-command 'isearch-printing-char))
      (scim-isearch-process-search-characters last-char)
    ad-do-it))

;; Commands and functions
(defun scim-enable-isearch ()
  "Make SCIM usable with isearch-mode."
  (interactive)
  (add-hook 'isearch-mode-hook 'scim-isearch-start)
  (ad-enable-regexp "^scim-isearch-")
  (ad-activate-regexp "^scim-isearch-"))

(defun scim-disable-isearch ()
  "Make SCIM not usable with isearch-mode."
  (interactive)
  (remove-hook 'isearch-mode-hook 'scim-isearch-start)
  (ad-disable-regexp "^scim-isearch-")
  (ad-activate-regexp "^scim-isearch-"))

(defun scim-setup-isearch ()
  (if scim-use-in-isearch-window
      (scim-enable-isearch)
    (scim-disable-isearch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scim-cleanup-variables ()
  (mapc (lambda (buffer)
	  (with-current-buffer buffer
	    (kill-local-variable 'scim-buffer-group)
	    (kill-local-variable 'scim-mode-map-prev-disabled)))
	(buffer-list))
  (setq-default scim-buffer-group nil)
  (setq-default scim-mode-map-prev-disabled nil)
  (setq scim-current-buffer nil
	scim-buffer-group-alist nil
	scim-imcontext-id nil
	scim-imcontext-status nil
	scim-callback-queue nil
	scim-preediting-p nil
	scim-last-rejected-event nil
	scim-last-command nil
	scim-config-last-modtime nil
	scim-net-active-window-unsupported nil))

(defun scim-mode-on ()
  "Turn scim-mode on."
  (interactive)
  (if (not (or (eq window-system 'x) ; X frame
	       (getenv "DISPLAY")))  ; non-X frame under X session
      (scim-mode-quit)
    (let (socket-live-p)
      (mapc (lambda (pair)
	      (if (processp (cdr pair))
		  (setq socket-live-p t)))
	    scim-bridge-socket-alist)
      (if socket-live-p (scim-mode-off))) ; Restart scim-mode
    (unwind-protect
	(scim-bridge-connect)
      (if (not (and scim-bridge-socket
		    (memq (process-status scim-bridge-socket)
			  '(open run))))
	  ;; Connection failed
	  (scim-mode-quit)
	;; Connection succeeded
	(setq scim-selected-display (scim-get-x-display)
	      scim-bridge-socket-alist (list (cons scim-selected-display
						   scim-bridge-socket)))
	;; Turn on minor mode
	(setq-default scim-mode t)
	(scim-cleanup-variables)
	(setq scim-frame-focus nil
	      scim-selected-frame (selected-frame))
	(scim-defadvice-disable-for-preedit)
	(scim-activate-advices-disable-for-preedit t)
	(scim-activate-advices-fix-post-command-hook t)
	(scim-defadvice-inherit-imcontext)
	(scim-activate-advices-inherit-im t)
	(scim-activate-advice-describe-key t)
	(scim-setup-isearch)
	;; Initialize key bindings
	(mapc (lambda (buffer)
		(with-current-buffer buffer
		  (if (memq major-mode scim-incompatible-major-modes)
		      (setq scim-mode-map-disabled t))))
	      (buffer-list))
	(scim-update-key-bindings)
	(scim-set-mode-map-alist)
	(add-to-ordered-list
	 'emulation-mode-map-alists 'scim-mode-map-alist 50)
	;; Setup hooks
	(add-hook 'minibuffer-exit-hook 'scim-exit-minibuffer-function)
	(add-hook 'after-change-major-mode-hook 'scim-check-major-mode)
	(when scim-incompatible-mode-hooks
	  (scim-message "`scim-incompatible-mode-hooks' is obsolete option. Use `scim-incompatible-major-modes' instead.")
	  (mapc (lambda (hook)
		  (add-hook hook 'scim-disable-keymap))
		scim-incompatible-mode-hooks))
	(add-hook 'ediff-startup-hook 'scim-check-current-buffer)
	(add-hook 'post-command-hook 'scim-check-current-buffer)
;#	(scim-log "post-command-hook: %s" post-command-hook)
	(add-hook 'after-make-frame-functions 'scim-after-make-frame-function)
	(add-hook 'kill-emacs-hook 'scim-mode-off)
;#	(scim-log "scim-mode ON")
	)))
  scim-mode)

(defun scim-mode-quit ()
  (remove-hook 'kill-emacs-hook 'scim-mode-off)
  (mapc (lambda (buffer)
	  (with-current-buffer buffer
	    (remove-hook 'kill-buffer-hook 'scim-kill-buffer-function t)))
	(buffer-list))
  (remove-hook 'after-make-frame-functions 'scim-after-make-frame-function)
  (remove-hook 'post-command-hook 'scim-check-current-buffer)
  (remove-hook 'ediff-startup-hook 'scim-check-current-buffer)
  (mapc (lambda (hook)
	  (remove-hook hook 'scim-disable-keymap))
	scim-incompatible-mode-hooks)
  (remove-hook 'after-change-major-mode-hook 'scim-check-major-mode)
  (remove-hook 'minibuffer-exit-hook 'scim-exit-minibuffer-function)
  (setq emulation-mode-map-alists
	(delq 'scim-mode-map-alist emulation-mode-map-alists))
  (scim-update-kana-ro-key t)
  (scim-activate-advices-disable-for-preedit nil)
  (scim-activate-advices-fix-post-command-hook nil)
  (scim-activate-advices-inherit-im nil)
  (scim-activate-advice-describe-key nil)
  (scim-disable-isearch)
  (scim-cancel-focus-update-timer)
  (scim-cleanup-preedit)
  (mapc (lambda (pair)
	  (setq scim-bridge-socket (cdr pair))
	  (scim-bridge-disconnect))
	scim-bridge-socket-alist)
  (setq scim-bridge-socket-alist nil)
  (setq-default scim-mode nil)
  (scim-cleanup-variables)
  (scim-set-cursor-color)
;#  (scim-log "scim-mode OFF")
  scim-mode)

(defun scim-mode-off ()
  "Turn scim-mode off."
  (interactive)
  (when (and (stringp scim-imcontext-id)
	     scim-frame-focus)
    (condition-case err
	(scim-change-focus nil)
      (error (scim-message "%S" err))))
  (setq scim-frame-focus nil)
  ;; Deregister IMContext IDs
  (mapc (lambda (group)
	  (let ((scim-imcontext-id (cdar (cadr group))))
	    (condition-case err
		(scim-deregister-imcontext)
	      (error (scim-message "%S" err)))))
	scim-buffer-group-alist)
  ;; Turn off minor mode
  (scim-mode-quit))

(defun scim-update-mode ()
  (if scim-mode
      (scim-mode-on)
    (scim-mode-off)))

(defun scim-mode (&optional arg)
  "Toggle SCIM minor mode (scim-mode).
With optional argument ARG, turn scim-mode on if ARG is
positive, otherwise turn it off."
  (interactive "P")
  (if (not (or (eq window-system 'x)
	       (getenv "DISPLAY")
	       scim-mode))
      (prog1 nil
	(scim-message "scim-mode needs Emacs to run under X session."))
    (setq-default scim-mode
		  (if (null arg)
		      (not scim-mode)
		    (> (prefix-numeric-value arg) 0)))
    (scim-update-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup minor-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor-mode-alist
(unless (assq 'scim-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(scim-mode scim-mode-line-string) minor-mode-alist)))

;; minor-mode-map-alist
;;  scim-mode doesn't use `minor-mode-map-alist' but
;;  `emulation-mode-map-alists', and it is not set yet because
;;  `scim-mode-map' will be generated dynamically.

;; mode-line-mode-menu
(define-key mode-line-mode-menu [scim-mode]
  `(menu-item ,(purecopy "Smart Common Input Method (SCIM)") scim-mode
	      :help "Support the input of various languages"
	      :button (:toggle . (bound-and-true-p scim-mode))))

(provide 'scim-bridge)

;;;
;;; scim-bridge.el ends here
