;; typst-mode is full of bugs atm
;(use-package typst-mode
;  :straight (typst-mode :type git :host github :repo "Ziqi-Yang/typst-mode.el"))

(defvar typst-process
  nil "Active typst process.")

(defvar typst-auto-open
  t "Append --open to typst invocations.")

(define-derived-mode typst-mode
  prog-mode "Typst"
  "Major mode for typst.")

(general-create-definer typst-bind
  :states '(normal emacs)
  :keymaps 'typst-mode-map
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-mode))

(defun typst-kill-process ()
  (interactive)
  (when (processp typst-process)
    (ignore-errors (kill-process typst-process)))
  (setq typst-process nil))

(defun typst-start-process (action)
  (interactive)
  (typst-kill-process)
  (let ((root (or default-directory (file-name-directory (buffer-file-name)))))
    (setq typst-process (make-process :name "typst"
                                      :command (list "typst" "--root" root action (buffer-file-name) (when typst-auto-open "--open"))
                                      :buffer "*typst*"
                                      :stderr "*typst stderr*")))
  )

(defun typst-watch-buffer ()
  (interactive)
  (when (buffer-file-name)
    (typst-start-process "watch")))

(defun typst-compile-buffer ()
  (interactive)
  (when (buffer-file-name)
    (typst-start-process "compile")))

(typst-bind
 "cw" 'typst-watch-buffer
 "cc" 'typst-compile-buffer
 "ck" 'typst-kill-process)

(provide 'init-typst)
