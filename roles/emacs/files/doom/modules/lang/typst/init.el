;;; lang/typst/init.el -*- lexical-binding: t; -*-

(defvar typst-process
  nil "Active typst process.")

(defvar typst-auto-open
  t "Append --open to typst invocations.")

(defvar typst-mode-syntax-table
  nil "Syntax table for typst mode.")
(setq typst-mode-syntax-table
      (let ((s (make-syntax-table)))
        (modify-syntax-entry ?\" "\"" s)
        (modify-syntax-entry ?\( "()" s)
        (modify-syntax-entry ?\) ")(" s)
        (modify-syntax-entry ?\[ "(]" s)
        (modify-syntax-entry ?\] ")[" s)
        (modify-syntax-entry ?\{ "(}" s)
        (modify-syntax-entry ?\} "){" s)
        (modify-syntax-entry ?$  "$$"  s)
        (modify-syntax-entry ?-  "w"  s)
        (modify-syntax-entry ?#  "w"  s)
        (modify-syntax-entry ?/ ". 124b"  s)
        (modify-syntax-entry ?* ". 23"  s)
        (modify-syntax-entry ?\n "> b"  s)
        s))

(defcustom typst-keywords
  '("let" "set" "show" "if" "else" "return" "break"
    "continue" "for" "while" "in")
  "Identifiers treated as reserved keywords in Typst."
  :type '(repeat string))

(defcustom typst-modules
  '("calc")
  "Identifiers treated as reserved keywords in Typst."
  :type '(repeat string))

(defvar typst-font-lock-defaults
  '(()
    nil ; enable highlighting
    nil ; case sensitive
    nil))

(define-derived-mode typst-mode
  prog-mode "Typst"
  "Major mode for typst."
  (set-syntax-table typst-mode-syntax-table)

  (setq-local comment-start "//")
  (setq-local comment-padding 1)
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local font-lock-defaults typst-font-lock-defaults)

  (local-set-key (kbd "RET") (key-binding (kbd "M-j"))))

(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-mode))

(defun typst-kill-process ()
  (interactive)
  (when (processp typst-process)
    (ignore-errors (kill-process typst-process)))
  (setq typst-process nil))

(defun typst-start-process (action)
  (interactive)
  (typst-kill-process)
  (let ((root (or (expand-file-name (project-root (project-current)))
                  default-directory
                  (file-name-directory (buffer-file-name)))))
    (setq typst-process (make-process :name "typst"
                                      :command (list "typst" action "--root" root (buffer-file-name) (when typst-auto-open "--open"))
                                      :buffer "*typst*"
                                      :stderr "*typst stderr*")))
  (with-current-buffer "*typst stderr*"
    (compilation-mode))
  )

(defun typst-watch-buffer ()
  (interactive)
  (when (buffer-file-name)
    (typst-start-process "watch")))

(defun typst-compile-buffer ()
  (interactive)
  (when (buffer-file-name)
    (typst-start-process "compile")))

(defun typst-open-error-buffer ()
  (interactive)
  (when (buffer-file-name)
    (switch-to-buffer "*typst stderr*")))

(map! :map typst-mode-map
      :prefix "C-c"
      "w" 'typst-watch-buffer
      "c" 'typst-compile-buffer
      "k" 'typst-kill-process
      "e" 'typst-open-error-buffer)

;(with-eval-after-load
;    (require 'lsp)
;    (add-to-list 'lsp-language-id-configuration
;                 '("\\.typ" . "typst")))
;
;(after! lsp
;  (lsp-register-client
;   (make-lsp-client :new-connection (lsp-stdio-connection "typst-lsp")
;                    :activation-fn (lsp-activate-on "typst")
;                    :server-id 'typst-lsp)))
