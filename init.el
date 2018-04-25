;;; init.el --- Init file for Emacs

;;; Commentary:

;; Specification of package repositories and compilation of literal init file

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

;; Windows doesn't support SSL
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/"))
       (gnu-url (concat (if no-ssl "http" "https") "://gnu.elpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t)
  (when (version< emacs-version "24.4")
    (add-to-list 'package-archives (cons "gnu" gnu-url) t)))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (magit smartparens-config which-key web-mode use-package undo-tree try smartparens rainbow-mode paredit org-bullets hungry-delete htmlize flycheck emmet-mode counsel company clojure-mode-extra-font-locking cider auto-complete))))
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
