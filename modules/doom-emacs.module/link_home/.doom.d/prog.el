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
