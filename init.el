;;; init.el --- Init file for Emacs

;;; Commentary:

;; Specification of package repositories and compilation of literal init file

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

;;; Handy utility functions for legibility
(defun my/version>= (lhs rhs)
  "Wrapper for version< to make code more legible"
  (not (version< lhs rhs)))

(defun my/is-windows-and-no-ssl ()
  "Checks if system type is Windows and if SSL utilities aren't available"
  (and (memq system-type '(windows-nt ms-dos))
       (not (gnutls-available-p))))

(defun my/http-protocol (ssl)
  "Returns the HTTP protocol prefix (for SSL and regular)"
  (if ssl
      (identity "https")
    (identity "http")))

(defun my/build-proper-url (ssl url)
  "Takes whether ssl or not and the url and returns proper url"
  (concat (my/http-protocol ssl)
	  "://" url))

;;; end Handy utility functions

;; Windows doesn't support SSL in emacs-version < 26
(if (my/version>= emacs-version "26")
    (progn
      (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t))
  (progn
    (let* ((no-ssl (my/is-windows-and-no-ssl))
	   (url (my/build-proper-url (not no-ssl) "melpa.org/packages/"))
	   (gnu-url (my/build-proper-url (not no-ssl) "gnu.elpa.org/packages/")))
      (add-to-list 'package-archives (cons "melpa" url) t)
      (when (version< emacs-version "24.4")
	(add-to-list 'package-archives (cons "gnu" gnu-url) t)))))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/literal-init.org"))

(provide 'init)
;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   (quote
    (rust-mode all-the-icons darktooth-theme which-key web-mode use-package undo-tree try tide smartparens rjsx-mode rainbow-mode org-bullets magit json-mode hungry-delete haskell-mode emmet-mode counsel cider auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
