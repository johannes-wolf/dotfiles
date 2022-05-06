;;; prog.el -*- lexical-binding: t; -*-

;; C/C++
(after! cc-mode
  (sp-local-pair '(c++-mode) "R\"(" ")\"" :actions '(insert))

  (add-hook! 'c-mode-common-hook
    (lambda ()
      (setq comment-start "/* "
            comment-end   " */")))

  (c-add-style
   "mine" '("doom"
                (indent-tabs-mode . nil)
                (c-basic-offset . 4)
                (c-offsets-alist
                 (case-label    . --)
                 (access-label  . --)
                 (label         . --)
                 (inclass       . +)
                 (innamespace   . [0])
                 (inextern-lang . [0]))))
  (setq-default c-default-style "mine"))

;; Python
(after! python-mode
  (sp-local-pair '(python-mode) "f\"" "\"" :actions '(insert)))

;; LSP
(after! lsp-mode
  (setq lsp-log-io nil
        lsp-file-watch-threshold 4000
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-segments '(file symbols)
        lsp-imenu-index-symbol-kinds '(File Module Namespace Package Class Method Enum Interface
                                            Function Variable Constant Struct Event Operator TypeParameter))
  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]build\\'"
                 "[/\\\\]out\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories)))
