; When https://github.com/bazelbuild/emacs-bazel-mode is ready, deprecate this.
(use-package bazel-mode
  :config
  (add-to-list 'auto-mode-alist '(".*/BUILD\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '(".*/BUILD\\.bazel\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '(".*/WORKSPACE\\'" . bazel-mode)))

(provide 'l-bazel)
