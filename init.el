;;; init.el --- Init file for Emacs

;;; Commentary:

;; Specification of package repositories and compilation of literal init file

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

(defvar init/http)

;; Windows doesn't support SSL
(setq init/http (if (string-equal (system-name) "windows-nt")
		    "http"
		  "https"))

(add-to-list 'package-archives
	     '("melpa" . (concat init/http "://melpa.org/packages/")))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . (concat init/http "://elpa.gnu.org/packages/"))))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (hungry-delete flycheck web-mode undo-tree ox-reveal htmlize which-key use-package try rainbow-mode paredit org-bullets emmet-mode counsel company clojure-mode-extra-font-locking cider auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/literal-init.org"))

(provide 'init)
;;; init.el ends here
