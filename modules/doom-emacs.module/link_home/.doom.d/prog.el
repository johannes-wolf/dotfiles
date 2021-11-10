;;; prog.el -*- lexical-binding: t; -*-

;; C/C++
(after! cc-mode
  (sp-local-pair '(c++-mode) "R\"(" ")\"" :actions '(insert))

  (add-hook! 'c-mode-common-hook
    (lambda ()
      (setq comment-start "/* "
            comment-end   " */")))
  (c-add-style
   "my" '("doom"
                (indent-tabs-mode . nil)
                (c-basic-offset . 4)
                (c-offsets-alist
                 (innamespace   . 0)
                 (inextern-lang . 0)
                 (case-label    . 0))))
  (setq-default c-default-style "my"))

;; Python
(after! python-mode
  (sp-local-pair '(python-mode) "f\"" "\"" :actions '(insert)))
