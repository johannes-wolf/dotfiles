;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;(setq doom-font (font-spec :family "Source Code Pro" :size 26 :weight 'semi-light))
(setq doom-font (font-spec :family "Fira Code" :size 26))
(setq doom-theme-list '(leuven))
(setq doom-theme (nth (random (length doom-theme-list)) doom-theme-list))
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (if (file-directory-p (expand-file-name "~/Org"))
                        "~/Org/"
                      "~/org/"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Ignor cache and build dirs
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "cmake-build-debug")
  (add-to-list 'projectile-globally-ignored-directories "cmake-build-release"))

;; Add XML based templating language
;; as web-mode.
(use-package! web
  :mode ("\\.ftl\\'" . web-mode))

;; Configure eglot to support multi-root jumps using xref.
(after! eglot
  (setq eglot-extend-to-xref t
        eglot-report-progress t
        eglot-ignored-server-capabilities '(:foldingRangeProvider
                                            :documentFormattingProvider
                                            :documentHighlightProvider
                                            :documentOnTypeFormattingProvider)))

(defun ediff-copy-AB-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun ediff-copy-BA-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun ediff-goto-C ()
  (interactive)
  (when (derived-mode-p 'ediff-mode)
    (select-window (get-buffer-window "") nil)))

(defun ediff-goto-control-panel ()
  (interactive)
  (when (derived-mode-p 'ediff-mode)
    (select-window (get-buffer-window "*Ediff Control Panel*") nil)))

(defun add-both-to-ediff-mode-map ()
  (define-key ediff-mode-map "C-c C-c" 'ediff-goto-control-panel)
  (define-key ediff-mode-map "C-c C-e" 'ediff-goto-C)
  (define-key ediff-mode-map "+" 'ediff-copy-AB-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-both-to-ediff-mode-map)

(use-package! typst-ts-mode
  :after eglot
  :init (setq typst-ts-mode-indent-offset 2)
  :config
  (defvar typst-process
    nil "Active typst process.")

  (defvar typst-auto-open
    t "Append --open to typst invocations.")

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
      (compilation-mode)))

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

  (map! :map typst-ts-mode-map
        :prefix "C-c"
        "w" 'typst-watch-buffer
        "c" 'typst-compile-buffer
        "k" 'typst-kill-process
        "e" 'typst-open-error-buffer)

  (add-to-list 'eglot-server-programs
               `((typst-ts-mode) .
                 ,(eglot-alternatives `(,typst-ts-lsp-download-path
                                        "tinymist"
                                        "typst-lsp")))))


(add-hook! 'after-save-hook
           #'executable-make-buffer-file-executable-if-script-p)

(load! "abbrevs.el")
(setq-default abbrev-mode t)
