;; Set a dark theme, style a little
(load-theme 'wombat)
(set-face-background 'fringe "#000")
(set-background-color "#000")
(set-cursor-color "#aaa")
(set-face-attribute
 'highlight nil
 :inherit nil
 :background "#1e1e1e"
 :foreground 'unspecified
 :underline nil)

;; Remove visible stuff around the interface
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Display columns in modeline
(column-number-mode 1)

;; line numbers customized on the left side
(global-display-line-numbers-mode 1)

(set-face-foreground 'line-number "#555")
(set-face-attribute
 'line-number-current-line nil
 :foreground "orange"
 :background "#1e1e1e")

;; Set comments color
(set-face-foreground 'font-lock-comment-face "#555")

;; Highlight active line, preserving the colors.
(global-hl-line-mode 1)

;; Enable autoloading buffers when files changes on disk
(global-auto-revert-mode 1)

;; Setting some pointers
(setq
 ;; Clear frame title 
 frame-title-format nil

 ;; Disable GNU startup buffer
 inhibit-startup-message t

 ;; Set default window position and size
 default-frame-alist '((ns-transparent-titlebar . t)
                       (ns-appearance . dark)
                       (left . 0)
		       (width . 100)
		       (fullscreen . fullheight))

 ;; Set default identations with spaces only
 indent-tabs-mode nil
 js-indent-level 2
 css-indent-offset 2
 plantuml-indent-level 2

 ;; Enable packages only if used
 package-enable-at-startup nil

 ;; Customize modeline
 mode-line-position-column-line-format '("  %I  %l,%c")

 ;; Enable flashing the modeline instead of the anoying bell sound
 ring-bell-function (lambda ()
		      (let ((orig-fg (face-foreground 'mode-line)))
			(set-face-foreground 'mode-line "#ccc")
			(run-with-idle-timer 0.1
					     nil
					     (lambda (fg)
					       (set-face-foreground 'mode-line fg))
					     orig-fg))))

;; Require and initialize packages with straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrating straight with use-package for easy syntax
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Benchmarking emacs startup
(use-package benchmark-init :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Deal with garbadge collection with magic hack
(use-package gcmh :config (gcmh-mode 1))

;; No-littering dump emacs's autofiles where you want
(use-package no-littering
  :config
  ;; Create directories in wich you'd like to save the litter
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "~/.emacs.d/var/auto-save/") t))
	backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "~/.emacs.d/var/backup/")))))

;; Resolve MacOS tty $PATH discrepancy
(defun initialize-path ()
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package exec-path-from-shell :init (add-hook 'after-init-hook 'initialize-path))

;; Which-key helps with commands while waiting for the input
(use-package which-key :defer 0 :config (which-key-mode))

;; Easy minibuffer autocompletion with Vertico and Consult
(use-package vertico :config (vertico-mode 1))
(use-package consult :config (setq completion-styles '(substring basic)))

;; Company mode helps with in-buffer completions, enable it globally
(use-package company
  :defer t
  :config
  (setq
   ;; Set main configurations
   company-dabbrev-other-buffers t
   company-tooltip-minimum 10
   company-tooltip-flip-when-above t
   company-show-quick-access 'left
   company-tooltip-align-annotations t)
  
  ;; Set color for the quick access
  (custom-set-faces
   '(company-tooltip-quick-access
     ((t (:foreground "orange")))))
  
  ;; Set delay or activate manualy
  company-idle-delay 0
  company-tooltip-idle-delay 10
  company-require-match nil
  
  ;; List of enabled frontends
  company-frontends
  '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
    company-preview-frontend
    company-echo-metadata-frontend)

  ;; List of enabled backends
  company-backends '(company-capf)
  (global-company-mode))

;; Set keybind to activate tooltip manually
(global-set-key (kbd "C-<tab>")
                (lambda ()
                  (interactive)
                  (let ((company-tooltip-idle-delay 10))
                    (company-complete)
                    (and company-candidates
                         (company-call-frontends 'post-command)))))

;; Smartparens makes the pairs (), [], {}, etc. always even
(use-package smartparens
  :defer 0
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

;; MoveText helps with M-up/down line swap
(use-package move-text :defer 0 :config (move-text-default-bindings))

;; Multiple Cursors allow multi line selection for ease edition
(use-package multiple-cursors :defer 0)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Clojure-mode for support to .clj files and Cider for extra support
(use-package clojure-mode :defer t)
(use-package cider :defer t :config (setq cider-repl-display-help-banner nil))

;; Org mode for suppor to .org files
(use-package org
  :defer t
  :commands (org-mode org-capture org-agenda)
  :config
  (setq
   ;; Set visual references
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   
   ;; Set path so the agenda knows it's files
   org-agenda-files '("~/"))
  
  ;; Replace list hyphen with dot in org mode
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 ()
				  (compose-region (match-beginning 1)
						  (match-end 1)
						  "•")))))))

;; Set most used keybindings for org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Plantuml-mode helps versionize diagrams
(use-package plantuml-mode
  :defer t
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; Checking the time emacs takes to load
(defun echo-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections"
	   (format "%.2f secs"
		   (float-time
		    (time-subtract after-init-time
				   before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'echo-startup-time)
