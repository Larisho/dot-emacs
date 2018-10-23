(setq inhibit-startup-message t)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(column-number-mode nil)
(line-number-mode -1)

(unless (version< emacs-version "26")
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(use-package darktooth-theme
  :ensure t)

(use-package all-the-icons
  :ensure t
  :config
  (setq inhibit-compacting-font-caches t))

(defgroup my nil
  "Parent group for modeline customizations")

(defgroup my-modeline nil
  ""
  :group 'my)

(defvar my-modeline-vspc
  (propertize " " 'face 'variable-pitch))

(defface my-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group 'my-modeline)

(defface my-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group 'my-modeline)

(defface my-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group 'my-modeline)

(defface my-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'my-modeline)

;; (defface my-modeline-buffer-modified
;;   '((t (:inherit (error bold) :background nil)))
;;   "Face used for the 'unsaved' symbol in the mode-line."
;;   :group 'my-modeline)

;; Keep `my/modeline-current-window' up-to-date
(defvar my/modeline-current-window (frame-selected-window))
(defun my/modeline|set-selected-window (&rest _)
  "Sets `my/modeline-current-window' appropriately"
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq my/modeline-current-window win))))

(add-hook 'window-configuration-change-hook #'my/modeline|set-selected-window)
(add-hook 'focus-in-hook #'my/modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'my/modeline|set-selected-window)
(advice-add #'select-window :after #'my/modeline|set-selected-window)

(defun my/ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
     `all-the-icons-octicon' to fetch the icon."
  (concat (if vc-mode " " "  ")
	  (when icon
	    (concat
	     (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
	     (if text my-modeline-vspc)))
	  (when text
	    (propertize text 'face face))
	  (if vc-mode "  " " ")))

;; cross-platform path-splitter
(defun split-path (path) 
  (split-path-1 path ()))

(defun split-path-1 (path accum) 
  (let ((dir  (directory-file-name (file-name-directory path))) 
	(name (file-name-nondirectory path))) 
    (if (equal dir path)
	accum
      (split-path-1 dir (cons name accum)))))

(defun my/join-path (ls)
  (mapconcat 'identity ls "/"))

(defsubst my/active ()
  (eq (selected-window) my/modeline-current-window))

(defun my/pathcdr (path-seg path-split)
  (if (equal path-seg (car path-split))
      path-split
    (my/pathcdr path-seg (cdr path-split))))

(defun concat-lists (&rest ls)
  "Concatenates the lists provided into one list"
  (apply 'append ls))

(defun my/modeline-project-buffer-info ()
  "Tries to find `.git' folder and create a path from there to the current buffer and if it doesn't find it, return the parent directory and the current buffer."
  (if (equal (buffer-file-name) nil)
      (buffer-name)
   (let* ((filename (buffer-file-name))
	 (dir (locate-dominating-file filename ".git")))
    (if (equal dir nil)
	(let ((arr (split-string filename "/" t)))
	  (my/join-path (nthcdr (- (length arr) 2)
			     arr)))
      (let ((split-file (split-string (buffer-file-name) "/" t))
	    (split-dir (split-string dir "/" t)))
	(my/join-path
	 (my/pathcdr (car (last split-dir))
		  split-file)))))))

(defun my/modeline-buffer-file-name()
  "Returns the value of `my/modeline-project-buffer-info'. If the path returned is too long, abbreviate path using fish style abbreviations."
  (let* ((path (my/modeline-project-buffer-info))
	(split (split-string path "/" t))
	(size 45))
    (if (and (> (string-width path) size) (> (length split) 2))
	(my/join-path
	  (concat-lists
	   (butlast (mapcar (lambda (val)
			      (char-to-string (string-to-char val)))
			    split)
		    1)
	   (nthcdr (- (length split) 1)
		   split)))
      path)))

(defun my/modeline-buffer-file-name-0 ()
  (let ((max-size 30)
	(seg (reverse (split-path (buffer-file-name)))))
    (my/modeline-buffer-file-name-1 max-size
				       (car seg)
				       (cdr seg))))

(defun my/modeline-buffer-file-name-1 (ms r l)
  (if (or (<= ms (length r)) (not l))
      r
    (my/modeline-buffer-file-name-1 ms
				       (concat (car l) "/" r)
				       (cdr l))))

;; TODO: Use (- window-body-width len) to determine how much space there should be
(defun my/gen-padding (len)
  "Generates some padding based on the available window"
  (let ((space (float (window-body-width))))
    (if (>= len (/ space 2.0))
	"  "
      (make-string (ceiling (* space 0.25))
		   ?\s))))

(defun my/file-icon ()
  "Gets the icon that corresponds with the current buffer's file name"
  (concat (all-the-icons-icon-for-file (buffer-name) :height 1.25)
	  "  "))

(defun my/buffer-info ()
  "Combined information about the current buffer, including the current working
     directory, the file name, and its state (modified, read-only or non-existent)."
  (let ((name (my/modeline-buffer-file-name)))
    (concat
     (if buffer-file-name
	 name
       "%b")
     (cond (buffer-read-only
	    (concat " "
		    (all-the-icons-octicon
		     "lock"
		     :face 'my-modeline-warning
		     :v-adjust -0.05)))
	   ((buffer-modified-p)
	    (concat " "
		    (all-the-icons-faicon
		     "floppy-o"
		     :face nil
		     :v-adjust -0.0575)))
	   ((and buffer-file-name
		 (not (file-exists-p buffer-file-name)))
	    (concat " "
		    (all-the-icons-octicon
		     "circle-slash"
		     :face 'my-modeline-urgent
		     :v-adjust -0.05)))
	   ((buffer-narrowed-p)
	    (concat " "
		    (all-the-icons-octicon
		     "fold"
		     :face 'my-modeline-warning
		     :v-adjust -0.05)))
	   (t
	    (concat " "
		    (all-the-icons-octicon
		     "info"
		     :face nil
		     :v-adjust -0.05))))
     (my/gen-padding (length name)))))

(defun my/buffer-encoding ()
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
	    (0 "LF  ")
	    (1 "CRLF  ")
	    (2 "CR  "))
	  (let ((sys (coding-system-plist buffer-file-coding-system)))
	    (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
		   "UTF-8")
		  (t (upcase (symbol-name (plist-get sys :name))))))
	  (if line-number-mode
	      (concat " " (what-line))
	    "")
	  "  "))

(defun my/vcs ()
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
	   (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
	    (active (my/active))
	    (all-the-icons-default-adjust -0.1))
	(concat " "
		(cond ((memq state '(edited added))
		       (if active (setq face 'my-modeline-info))
		       (all-the-icons-octicon
			"git-compare"
			:face face
			:v-adjust -0.05))
		      ((eq state 'needs-merge)
		       (if active (setq face 'my-modeline-info))
		       (all-the-icons-octicon "git-merge" :face face))
		      ((eq state 'needs-update)
		       (if active (setq face 'my-modeline-warning))
		       (all-the-icons-octicon "arrow-down" :face face))
		      ((memq state '(removed conflict unregistered))
		       (if active (setq face 'my-modeline-urgent))
		       (all-the-icons-octicon "alert" :face face))
		      (t
		       (if active (setq face 'font-lock-doc-face))
		       (all-the-icons-octicon
			"git-compare"
			:face face
			:v-adjust -0.05)))
		" "
		(propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
			    'face (if active face))
		"  ")))))

(defun my/major-mode ()
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
	   (when (stringp mode-line-process)
	     mode-line-process)
	   (and (featurep 'face-remap)
		(/= text-scale-mode-amount 0)
		(format " (%+d)" text-scale-mode-amount)))
   'face nil))

(defun my/flycheck-modeline ()
  "Flycheck edits for mode line"
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      ('finished (if flycheck-current-errors
		     (let-alist (flycheck-count-errors flycheck-current-errors)
		       (let ((sum (+ (or .error 0) (or .warning 0))))
			 (my/ml-icon (if .error "error_outline" "remove_circle_outline")
					(number-to-string sum)
					(if .error 'my-modeline-urgent 'my-modeline-warning)
					-0.25)))
		   (my/ml-icon "check" nil 'my-modeline-info)))
      ('running     (my/ml-icon "access_time" nil 'font-lock-doc-face -0.25))
      ('no-checker  (my/ml-icon "visibility_off" nil 'font-lock-doc-face))
      ('errored     (my/ml-icon "sim_card_alert" "Flycheck Error" 'my-modeline-urgent))
      ('interrupted (my/ml-icon "pause" "Interrupted" 'font-lock-doc-face)))))

(setq-default mode-line-format
	      (list " "
		    'mode-line-front-space
		    '(:eval (my/file-icon))
		    '(:eval (my/buffer-info)) ; File name with parent dir
		    '(:eval (my/buffer-encoding)) ; line ending+encoding
		    '(:eval (my/vcs)) ; branch name (and perhaps some icon)
		    '(:eval (my/major-mode))	    ; Major mode name 
		    '(:eval (my/flycheck-modeline)) ; Flycheck UI edits
		    'mode-line-end-space))

(if t (force-mode-line-update t))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Prettify bullets to make document look nicer
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(global-visual-line-mode t)

(use-package cider
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package emmet-mode
  :ensure t)

;; Counsel is a dependency of Swiper
(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
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

(use-package auto-complete
  :ensure t
  :init (progn
          (ac-config-default)
          (global-auto-complete-mode t)))

(defalias 'list-buffers 'ibuffer)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  ; Tell Web-Mode about templating engines it should know about
  (setq web-mode-engines-alist
        '(("ejs"    . "\\.ejs\\'")))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t))

(use-package js2-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist
                       `(,(rx ".js" string-end) . js2-mode)))

(use-package rjsx-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist
                       '("components\\/.*\\.js\\'" . rjsx-mode)))
;; Consider adding rjsx-mode to the auto-mode-alist for `(,(rx ".js" string-end))

(use-package json-mode
  :ensure t
  :config
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(use-package flycheck
  :ensure t
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
  :ensure t
  :config
  (use-package smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-local-pair 'web-mode "%" "%")
  (sp-local-pair 'web-mode "<" ">"))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  )

(use-package magit
  :ensure t
  :init
  (progn
  (bind-key "C-x g" 'magit-status)))

(defun my/insert-comment-todo ()
  "Insert text `// TODO(gab): ' at point moving point forward."
  (interactive)
  (insert "// TODO(gab): "))

(global-set-key (kbd "C-c t") 'my/insert-comment-todo)

(defun my/insert-comment-note ()
  "Insert text `// NOTE(gab): ' at point moving point forward."
  (interactive)
  (insert "// NOTE(gab): "))

(global-set-key (kbd "C-c n") 'my/insert-comment-note)

(defun my/next-line-and-center ()
  "Move the point forwards one line and recenters the window."
  (interactive)
  (forward-line 1)
  (recenter))

(global-set-key (kbd "C-;") 'my/next-line-and-center)

(defun my/previous-line-and-center ()
  "Move the point backwards one line and recenters the window."
  (interactive)
  (forward-line -1)
  (recenter))

(global-set-key (kbd "C-'") 'my/previous-line-and-center)

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

;; (require 'haskell)

;; ;; add capability to submit code to interpreter and mark errors
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; ;; add missing keybindings for navigating errors
;; (define-key interactive-haskell-mode-map (kbd "M-n") 'haskell-goto-next-error)
;; (define-key interactive-haskell-mode-map (kbd "M-p") 'haskell-goto-prev-error)

;; ;; merge this with your existing custom-set-variables
;; (custom-set-variables
;;  ;; some options suggested in the haskell-mode documentation
;;  '(haskell-process-auto-import-loaded-modules t)
;;  '(haskell-process-log t)
;;  '(haskell-process-suggest-remove-import-lines t))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-basic-offset 2)))
