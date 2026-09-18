;;; Custom Emacs Configuration Init

;; UI Changes
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(indent-tabs-mode nil)
(setq org-adapt-indentation t)
(global-visual-line-mode t)

(load-file (expand-file-name "~/.emacs.d/mode-line.el"))

;; Auto saves
(unless (file-directory-p "~/.emacs.d/auto-saves")
  (make-directory "~/.emacs.d/auto-saves"))

(setq backup-directory-alist '(("." . "~/.emacs.d/auto-saves")))
(setq undo-tree-auto-save-history nil)

(if (version< emacs-version "26")
    (progn
      (column-number-mode nil)
      (line-number-mode -1))
  (global-display-line-numbers-mode t))

(unless (version< emacs-version "26")
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Increase GC threshold
(setq gc-cons-threshold 100000000)

;; Increase process memory
(setq read-process-output-max (* 1024 1024))

;; Theme
(unless (package-installed-p 'color-theme-sanityinc-tomorrow)
  (package-install 'color-theme-sanityinc-tomorrow))
(require 'sanityinc-tomorrow-night-theme)
(load-theme 'sanityinc-tomorrow-night t)

;; Dependencies
(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))

(use-package try)

(use-package which-key
  :config
  (which-key-mode))

(use-package org-bullets ; Prettify bullets to make document look nicer
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package cider ; Disabled
  :ensure nil)

(use-package rainbow-mode)

(use-package counsel) ; Counsel is a dependency of Swiper

(use-package ivy
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(defalias 'list-buffers 'ibuffer)

(use-package undo-tree
  :init
  (global-undo-tree-mode))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  ; Tell Web-Mode about templating engines it should know about
  (setq web-mode-engines-alist
	'(("ejs"    . "\\.ejs\\'")))
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-css-indent-offset 2))

(use-package js2-mode
  :config (add-to-list 'auto-mode-alist
                       `(,(rx ".js" string-end) . js2-mode))
  :hook
  (js2-mode . (lambda ()
               (setq js2-basic-offset 2))))

(use-package rjsx-mode
  :config (add-to-list 'auto-mode-alist
                       '("components\\/.*\\.js\\'" . rjsx-mode)))
; Consider adding rjsx-mode to the auto-mode-alist for `(,(rx ".js" string-end))

(use-package rust-mode)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package json-mode
  :config
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(use-package yaml-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (rust-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  :commands
  (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :config
  (dap-auto-configure-mode))

(use-package yasnippet)

(use-package flycheck
  :init (global-flycheck-mode t)
  :config (progn
            ;; disable jshint since we prefer eslint checking
            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers
                                  '(javascript-jshint)))

            ;; use eslint with web-mode for jsx files
            (flycheck-add-mode 'javascript-eslint 'web-mode)

            ;; customize flycheck temp file prefix
            (setq-default flycheck-temp-prefix ".flycheck")

            ;; disable json-jsonlist checking for json files
            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers
                                  '(json-jsonlist)))))

(use-package smartparens
  :config
  ;; load default config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  ;;   (sp-local-pair 'web-mode "%" "%")
  ;;   (sp-local-pair 'web-mode "<" ">")
  )

(use-package magit
  :init
  (progn
  (bind-key "C-x g" 'magit-status)))

(defun my/insert-comment-todo ()
  "Insert text `// TODO(gab): ' at point moving point forward."
  (interactive)
  (insert "// TODO(gab): "))

(defun my/insert-comment-note ()
  "Insert text `// NOTE(gab): ' at point moving point forward."
  (interactive)
  (insert "// NOTE(gab): "))

(defun my/duplicate-line ()
  "Duplicate the line at `(point)' and write it to the line below."
  (interactive)
  (let ((start (line-beginning-position))
	(end (line-end-position)))
    (goto-char end)
    (newline)
    (insert (buffer-substring start end))))

(defun my/next-line-and-center ()
  "Move the point forwards one line and recenters the window."
  (interactive)
  (forward-line 1)
  (recenter))

(defun my/previous-line-and-center ()
  "Move the point backwards one line and recenters the window."
  (interactive)
  (forward-line -1)
  (recenter))

(defun my/minor-modes-list ()
  "Returns a list of the minor modes' symbols"
  (delq nil
	(mapcar
	 (lambda (x)
	   (let ((car-x (car x)))
	     (when (and (symbolp car-x) (symbol-value car-x))
	       car-x)))
	 minor-mode-alist)))

(defun minor-modes ()
  "Returns a list containing all the active minor-modes."
  (interactive)
  (message "Minor Modes: %s"
	   (mapconcat 'symbol-name (my/minor-modes-list) ", ")))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)))

;; Keybinds
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(global-set-key (kbd "C-c t") 'my/insert-comment-todo)
(global-set-key (kbd "C-c n") 'my/insert-comment-note)
(global-set-key (kbd "C-c d") 'my/duplicate-line)
(global-set-key (kbd "C-;") 'my/next-line-and-center)
(global-set-key (kbd "C-'") 'my/previous-line-and-center)

(provide 'custom-init)
;;; custom-init.el ends here
