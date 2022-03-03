;;; org.el -*- lexical-binding: t; -*-

(setq org-directory "~/Org"
      org-agenda-files '("~/Org/" "~/Org/Schule/" "~/Org/Schule/E2/"))

;; Attach
(setq org-attach-method 'mv
      org-attach-store-link-p t
      org-attach-id-dir "./.attach/")

;; Export
(setq org-export-with-sub-superscripts t
      org-export-default-language "de")

;; Misc
(setq org-use-property-inheritance t
      org-preview-latex-image-directory "./.org-latex-cache/"
      org-file-apps '(("\\.png\\'" . "xdg-open %s")
                      ("\\.jpg\\'" . "xdg-open %s")))

(after! ox-latex
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))
  (add-to-list 'org-latex-classes
               '("school"
                 "\\documentclass[a4paper,11pt]\{scrartcl\}
\\usepackage\{parskip\}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Org Capture
(after! org-capture
        (setq org-capture-templates
        '(("h" "Homework" entry (file+headline "~/Org/Schule/homework.org" "Homework")
                "* TODO %? :%^{Subject|deu|ma|en|fra|ges|geo|eth|inf|phy|che|bio}:\nSCHEDULED: %^{Bis}t\n%i\n%a")
                ("t" "Todo" entry (file "~/Org/todo.org")
                "* TODO %?\n%t\n%i\n%a")
                ("n" "Note" entry (file "~/Org/notes.org")
                "* %?\n%t\n%i\n%a"))))

(use-package! org
  :init
  ;; Capture templates
  (setq org-capture-templates
        '(("h" "Homework" entry (file+headline "~/Org/Schule/homework.org" "Homework")
           "* TODO %? :%^{Subject|deu|ma|en|fra|ges|geo|eth|inf|phy|che|bio}:\nSCHEDULED: %^{Bis}t\n%i\n%a")
          ("t" "Todo" entry (file "~/Org/todo.org")
           "* TODO %?\n%t\n%i\n%a")
          ("n" "Note" entry (file "~/Org/notes.org")
           "* %?\n%t\n%i\n%a")))

  ;; Smash [f12] to search org directory
  (global-set-key [f12] '(lambda () (interactive) (consult-find org-directory)))

  (sp-local-pair '(org-mode) "<<" ">>" :actions '(insert))
  (sp-local-pair '(org-mode) "$" "$" :actions '(insert))
  (sp-local-pair '(org-mode) "$$" "$$" :actions '(insert))
  (sp-local-pair '(org-mode) "~" "~" :actions '(insert))

  :config
  (map! :map org-mode-map
        :nv "=&" (lambda ()
                   (interactive)
                   (when (texmathp)
                     (progn
                       (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)&=" 1 1 t)
                       (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)&&" 1 1 t))))))

(use-package! org-download
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :custom
  (org-download-method 'attach)
  (org-download-screenshot-method "flameshot gui --raw > %s")
  (org-download-annotate-function (lambda (link) ""))
  (org-download-heading-lvl nil)
  (org-download-delete-image-after-download t)

  :config
  (map! :map org-mode-map
        ("<f5>" (lambda ()
                  (interactive)
                  (let ((dwim-fun (org-collect-keywords '("F5"))))
                    (when dwim-fun
                      (funcall (intern (cadar dwim-fun)))))))))

;(use-package! org-fragtog-mode
;  :hook (org-mode . org-fragtog-mode))

(use-package! anki-editor
  :hook (org-mode . anki-editor-mode)
  :custom
  (anki-editor-create-decks t)
  (anki-editor-org-tags-as-anki-tags t))

(use-package! org-krita
  :hook (org-mode . org-krita-mode))

(use-package! ox-koma-letter
  :after ox-latex
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

;; Function for creating random file names
(defun +babel-file (name extension)
  (interactive)
  (let ((tmp-dir "./.org-babel/")
        (tmp-name (if (eq name nil) (uuid-create) name)))
    (unless (file-directory-p tmp-dir)
      (make-directory tmp-dir))
    (concat "./.org-babel/" tmp-name extension)))

;; ROAM

(setq org-roam-directory "~/roam"
      +org-roam-open-buffer-on-find-file nil
      org-id-extra-files (org-roam--list-files org-roam-directory))

(setq org-publish-project-alist
      '(("roam-html-notes"
         :base-directory "~/roam"
         :publishing-function org-html-publish-to-html
         :publishing-directory "~/roam/.export_html"
         :section-numbers nil
         :with-toc nil
         :html-head "")
        ("roam-html-equations"
         :base-directory "~/roam/.org-latex-cache"
         :base-extension "png\\|pdf\\|svg"
         :publishing-function org-publish-attachment
         :publishing-directory "~/roam/.export_html/.org-latex-cache")
        ("roam-html-babel"
         :base-directory (org-babel-temporary-directory)
         :base-extension "png\||pdf\||svg"
         :publishing-function org-publish-attachment
         :publishing-directory "~/roam/.export_html/.babel-export")
        ("roam-html" :components ("roam-html-notes" "roam-html-equations"))
        ("roam-pdf"
         :base-directory "~/roam"
         :publishing-function org-latex-publish-to-pdf
         :publishing-directory "~/roam/.export_pdf"
         :section-number nil
         :latex-class "school"
         :with-toc nil)))
