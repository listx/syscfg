; http://www.emacswiki.org/emacs/GenericMode
(require 'generic-x)

(define-generic-mode
	'xdefaults-mode
	'(("!") ("/*" . "*/")) ; comments
	'() ; static keywords e.g., ("if" "else" "return")
	'(
		; regexes for matching aginst font-lock
		("^\\w+" . 'font-lock-type-face)
		("[*.:]" . 'font-lock-builtin-face)
	)
	'("\\.Xdefaults$") ; files to activate this mode (FIXME: maybe move it to kakapo instead?)
	nil ; other functions to call
	"A mode for ~/.Xdefaults and ~/.Xresources files" ; docstring for this mode
)


(provide 'l-generic-x)
