(setq org-publish-project-alist
	'(("eh"
		:base-directory "~/prog/elementary-haskell/"
		:publishing-directory "~/prog/elementary-haskell/public_html"
		:publishing-function org-html-publish-to-html
		:section-numbers t
		:with-toc t
		:html-preamble t
		:html-head "<link rel=\"stylesheet\"
			href=\"css/style.css\"
			type=\"text/css\"/>"
	)))
