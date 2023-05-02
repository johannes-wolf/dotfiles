;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/Org"
	org-agenda-files '("~/Org/" "~/Org/Schule/" "~/Org/Schule/E2/")
	;; Attach
	org-attach-method 'mv
	org-attach-store-link-p t
	org-attach-id-dir "./.attach/"
	;; Export
	org-export-with-sub-superscripts t
	org-export-default-language "de"
	;; Latex
	org-preview-latex-image-directory  (expand-file-name (concat org-directory "/.cache/")))
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;; Hit [f12] to ff org directory
  (global-set-key [f12] '(lambda () (interactive) (consult-find org-directory)))
  (tyrant-def
    :keymaps 'org-mode-map
    "me" 'org-export-dispatch))

(define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

(defun +org--toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let* ((beg (or beg
		  (if (org-before-first-heading-p)
		      (save-excursion (point-min))
		    (save-excursion (org-back-to-heading) (point)))))
	 (end (or end
		  (if (org-before-first-heading-p)
		      (save-excursion (org-next-visible-heading 1) (point))
		    (save-excursion (org-end-of-subtree) (point)))))
	 (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
				     (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))

(defun my-org-dwim-at-point (&optional arg)
  "Do what I mean at point."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
	   (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
	(setq context (org-element-property :parent context)
	      type (org-element-type context)))
      (pcase type
	((or `citation `citation-reference)
	 (org-cite-follow context arg))

	(`footnote-reference
	 (org-footnote-goto-definition (org-element-property :label context)))

	(`footnote-definition
	 (org-footnote-goto-previous-reference (org-element-property :label context)))
	((or `table `table-row)
	 (if (org-at-TBLFM-p)
	     (org-table-calc-current-TBLFM)
	   (ignore-errors
	     (save-excursion
	       (goto-char (org-element-property :contents-begin context))
	       (org-call-with-arg 'org-table-recalculate (or arg t))))))

	(`table-cell
	 (org-table-blank-field)
	 (org-table-recalculate arg)
	 (when (and (string-empty-p (string-trim (org-table-get-field)))
		    (bound-and-true-p evil-local-mode))
	   (evil-change-state 'insert)))

	(`babel-call
	 (org-babel-lob-execute-maybe))

	(`statistics-cookie
	 (save-excursion (org-update-statistics-cookies arg)))

	((or `src-block `inline-src-block)
	 (org-babel-execute-src-block arg))

	((or `latex-fragment `latex-environment)
	 (org-latex-preview arg))

	(`link
	 (let* ((lineage (org-element-lineage context '(link) t))
		(path (org-element-property :path lineage)))
	   (if (or (equal (org-element-property :type lineage) "img")
		   (and path (image-type-from-file-name path)))
	       (+org--toggle-inline-images-in-subtree
		(org-element-property :begin lineage)
		(org-element-property :end lineage))
	     (org-open-at-point arg))))
	
	(`paragraph
	 (+org--toggle-inline-images-in-subtree))))))
  
(define-key org-mode-map (kbd "C-c C-c") #'my-org-dwim-at-point)

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-local-set-key 'normal (kbd "RET") #'my-org-dwim-at-point))))

(use-package ox-koma-letter
  :ensure nil
  :after ox-latex
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))
  (add-to-list 'org-latex-packages-alist '("T1,EU1" "fontenc" t)) ;; lualatex utf-8 support
  (add-to-list 'org-latex-classes
	       '("default-koma-letter"
		 "\\documentclass\[%
   parskip=half\]\{scrlttr2\}
   \[DEFAULT-PACKAGES]
   \[PACKAGES]
   \[EXTRA]"))
  :custom
  (org-koma-letter-class-option-file "DIN5008A"))

;; lualatex preview
(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode %f"
	"lualatex -shell-escape -interaction nonstopmode %f"))

(setq luamagick '(luamagick :programs ("lualatex" "convert")
			    :description "pdf > png"
			    :message "you need to install lualatex and imagemagick."
			    :use-xcolor t
			    :image-input-type "pdf"
			    :image-output-type "png"
			    :image-size-adjust (1.0 . 1.0)
			    :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f && lualatex -interaction nonstopmode -output-directory %o %f")
			    :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

(add-to-list 'org-preview-latex-process-alist luamagick)
(setq org-preview-latex-default-process 'luamagick)

;; Support inline image background color
(defcustom +org-inline-image-background nil
  "The color used as the default background for inline images.
When nil, use the default face background."
  :group 'org
  :type '(choice color (const nil)))

(defun create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
	 (type (cadr args))
	 (data-p (caddr args))
	 (props (cdddr args)))
    ;; Get this return result style from `create-image'.
    (append (list file type data-p)
	    (list :background (or +org-inline-image-background (face-background 'default)))
	    props)))

(advice-add 'create-image :filter-args
	    #'create-image-with-background-color)

;; Set default background color to white
(setq +org-inline-image-background "white")

(defun uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
			(user-uid)
			(emacs-pid)
			(system-name)
			(user-full-name)
			user-mail-address
			(current-time)
			(emacs-uptime)
			(garbage-collect)
			(random)
			(recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
	    (substring s 0 8)
	    (substring s 8 12)
	    (substring s 13 16)
	    (substring s 16 20)
	    (substring s 20 32))))

(unless (boundp 'org-export-current-backend)
  (defadvice org-export-as (around oecb-around)
    (let ((org-export-current-backend (ad-get-arg 0)))
      ad-do-it)))

;; Function for creating babel PDF/SVG files (PDF on export, SVG for preview)
(defun +babel-file (name)
  (interactive "s")
  (let ((extension (if (and (boundp 'org-export-current-backend) (eq org-export-current-backend 'latex)) ".pdf"
		     ".svg"))
	(tmp-dir (concat org-directory "/.org-babel/"))
	(tmp-name (if (eq name nil) (uuid-create) name)))
    (unless (file-directory-p tmp-dir)
      (make-directory tmp-dir))
    (concat tmp-dir "/" tmp-name extension)))

;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (python  . t)
   (lua     . t)
   (calc    . t)
   (maxima  . t)))

(defun +org-confirm-babel-evaluate (lang body)
  (not (member lang '('gnuplot 'calc 'maxima 'latex))))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

(use-package ox-latex
  :ensure nil
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))
  (add-to-list 'org-latex-classes
               '("koma11"
                 "\\documentclass[a4paper,11pt]\{scrartcl\}
\\usepackage\{parskip\}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("koma12"
                 "\\documentclass[a4paper,12pt]\{scrartcl\}
\\usepackage\{parskip\}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package ox-koma-letter
  :after ox-latex
  :ensure nil
  :init
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))
  (add-to-list 'org-latex-classes
               '("default-koma-letter"
                 "\\documentclass\[%
   parskip=half\]\{scrlttr2\}
   \[DEFAULT-PACKAGES]
   \[PACKAGES]
   \[EXTRA]"))
  :custom
  (org-koma-letter-class-option-file "DIN5008A"))

(use-package ox-reveal
  :straight (ox-reveal :type git :host github :repo "yjwen/org-reveal"))
(require 'ox-reveal)

(defun org-reveal-install ()
  (interactive)
  (let ((default-directory contrib-dir)) 
        (shell-command "git clone https://github.com/hakimel/reveal.js.git && cd reveal.js && npm install")))

(setq org-reveal-root (concat "file://" (expand-file-name contrib-dir) "/reveal.js"))

(provide 'init-org)
