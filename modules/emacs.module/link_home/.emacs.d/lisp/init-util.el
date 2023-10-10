;(use-package openai
;  :straight (openai :type git :host github :repo "emacs-openai/openai"))
;
;(use-package chatgpt
;  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt"))

(use-package languagetool
  :straight (languagetool :type git :host github :repo "PillFall/languagetool.el")
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :init
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command (concat (expand-file-name contrib-dir)
					     "/LanguageTool/languagetool-commandline.jar")
	languagetool-server-command (concat (expand-file-name contrib-dir)
					    "/LanguageTool/languagetool-server.jar")
	langtool-mother-tongue "de"
	langtool-default-language "de-DE"
	languagetool-api-key "pit-KBsxQkNdisJe"
	languagetool-username "accounts@johannes-wolf.com"))
