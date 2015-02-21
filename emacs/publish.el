; do not convert TAB characters in source code blocks into spaces
(setq org-src-preserve-indentation t)

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
))
