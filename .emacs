;; Set a dark theme, style a little
(load-theme 'wombat)
(set-face-background 'fringe "#000")
(set-background-color "#000")
(set-cursor-color "#aaa")
(set-face-attribute 'highlight nil
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

;; Line numbers customized on the left side
(global-display-line-numbers-mode 1)
(set-face-foreground 'line-number "#555")
(set-face-attribute 'line-number-current-line nil
		    :foreground "orange"
		    :background "#1e1e1e")

;; Set comments color and highlight active line
(set-face-foreground 'font-lock-comment-face "#555")
(global-hl-line-mode 1)

;; Enable autoloading buffers when files changes on disk
(global-auto-revert-mode 1)

;; Modeline customization
(setq-default
 mode-line-format (list
		   ;; File status (modified, read-only)
		   '(:eval (cond (buffer-read-only " RO")
				 ((buffer-modified-p) " *")
				 (t " ")))

		   ;; Buffer name
		   '(:eval (propertize " %b " 'face '(:weight bold :foreground "#ccc")))
		   
		   ;; Line column and percentage
		   " %I %l,%c"

		   ;; Buffer percentage
		   " %p"

		   ;; Major mode
                   "  " '(:eval mode-name)

		   ;; Git branch
		   '(:eval (let ((branch (car (vc-git-branches))))
			     (when branch
			       (concat " "
				       (propertize (format " %s" branch)
						   'face '(:foreground "#ccc"))))))))

;; Ensure mode line is updated
(add-hook 'after-save-hook 'force-mode-line-update)

;; Setting some general pointers
(setq
 ;; Clear frame title 
 frame-title-format nil

 ;; Disable GNU startup buffer
 inhibit-startup-message t

 ;; Set default window position and size
 ;; default-frame-alist '((ns-transparent-titlebar . t)
 ;;                       (ns-appearance . dark)
 ;;                       (left . 0)
 ;; 		       (width . 100)
 ;; 		       (fullscreen . fullheight))

 ;; Set default identations with spaces only
 indent-tabs-mode nil
 js-indent-level 2
 css-indent-offset 2
 plantuml-indent-level 2

 ;; Enable packages only if used
 package-enable-at-startup nil

 ;; Enable flashing the modeline instead of the anoying bell sound
 ring-bell-function (lambda ()
		      (let ((orig-fg (face-foreground 'mode-line)))
			(set-face-foreground 'mode-line "#ccc")
			(run-with-idle-timer 0.1
					     nil
					     (lambda (fg)
					       (set-face-foreground 'mode-line fg))
					     orig-fg))))

;; Cycle windows forward/backward 
(global-set-key (kbd "C-x ]") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x [") (lambda () (interactive) (other-window +1)))

;; Add melpa to package archives
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Comment/uncomment this line to enable MELPA Stable if desired.
;; See `package-archive-priorities` and `package-pinned-packages`.
;; Most users will not need or want to do this.
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Require and initialize packages with straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
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

(use-package exec-path-from-shell
  :defer 0
  :init
  (add-hook 'after-init-hook 'initialize-path)
  :config
  (exec-path-from-shell-copy-env "DOCKER_HOST")
  (exec-path-from-shell-copy-env "TESTCONTAINERS_DOCKER_SOCKET_OVERRIDE")
  (exec-path-from-shell-copy-env "TESTCONTAINERS_RYUK_DISABLED"))


;; Which-key helps with commands while waiting for the input
(use-package which-key :defer 0 :config (which-key-mode))

;; Treemacs is way better than dired
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (set-face-attribute 'treemacs-root-face nil
                        :foreground "#eee"
			:height 1
                        :underline nil)
    (set-face-attribute 'treemacs-directory-face nil
                        :foreground "#ccc")
    (treemacs-resize-icons 16)))

;; Easy minibuffer autocompletion with Vertico and Consult
(use-package vertico :config (vertico-mode 1))
(use-package consult :config (setq completion-styles '(substring basic)))

;; Marginalia for extra info in minibuffer completions
(use-package marginalia :config (marginalia-mode))

;; Improve fuzzy completions with Orderless
(use-package orderless
  :init
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))))

;; Magit git porcelain with forge to handle PRs
(setq
 auth-sources '("~/.authinfo.gpg")
 epg-pinentry-mode 'loopback)

(use-package magit :defer t)
(use-package forge :after magit)

;; Clojure-mode for support to .clj files and Cider for extra support
(use-package clojure-mode :defer t)
(use-package cider
  :defer t
  :config
  (setq
   ;; Check https://docs.cider.mx/cider/repl/configuration.html
   cider-repl-display-help-banner nil
   cider-repl-pop-to-buffer-on-connect nil
   cider-repl-display-in-current-window t
   ;; Clojure debugger with the repl
   cider-jack-in-dependencies '(("com.github.flow-storm/flow-storm-dbg" "3.15.5"))))

;; Lsp backend protocols
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook '((clojure-mode . lsp)
	  (typescript-mode . lsp)
	  (yaml-ts-mode . lsp)
	  (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-completion-provider)
  :config
  (dolist (m '(clojure-mode
	       clojurec-mode
	       clojurescript-mode
	       clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  (setq
   lsp-lens-enable nil
   lsp-modeline-workspace-status-enable nil
   lsp-headerline-breadcrumb-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-completion-show-kind nil
   lsp-enable-file-watchers t
   lsp-ui-sideline-show-diagnostics t
   lsp-log-io nil
   lsp-response-timeout 1
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-position 'top
   lsp-eldoc-render-all nil
   lsp-signature-render-documentation t))

(use-package consult-lsp :after lsp-mode)

;; Company mode helps with in-buffer completions
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

;; Set company keybind to activate tooltip manually
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

(global-set-key (kbd "C-c <left>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-c <right>") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-c C-.") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-c C-,") 'sp-backward-barf-sexp)

;; MoveText helps with M-up/down line swap
(use-package move-text :defer 0 :config (move-text-default-bindings))

;; Multiple Cursors allow multi line selection for ease edition
(use-package multiple-cursors :defer 0)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Common lisp support with Slime
(use-package slime
  :defer t
  :config
  (setq
   inferior-lisp-program "/opt/homebrew/bin/sbcl"
   slime-contribs '(slime-fancy)
   slime-repl-history-file "~/.emacs.d/slime-history.eld"
   slime-repl-history-size 1000
   slime-repl-history-remove-duplicates t
   slime-net-coding-system 'utf-8-unix))

;; Org mode for support to .org files
;; (use-package org
;;   :defer t
;;   :commands (org-mode org-capture org-agenda)
;;   :config
;;   (setq
;;    ;; Set visual references
;;    org-hide-emphasis-markers t
;;    org-hide-leading-stars t
;;    org-adapt-indentation t
;;    org-indent-indentation-per-level 2)

;;   ;; Replace list hyphen with dot in org mode
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^ *\\([-]\\) "
;;                              (0 (prog1 ()
;;                                   (compose-region (match-beginning 1)
;;                                                   (match-end 1)
;;                                                   "•")))))))

;; ;; Keybindings for org
;; (global-set-key (kbd "C-c l") #'org-store-link)
;; (global-set-key (kbd "C-c a") #'org-agenda)
;; (global-set-key (kbd "C-c c") #'org-capture)

;; Plantuml-mode helps versionize diagrams
(use-package plantuml-mode
  :defer t
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; Dumb Jump helps find definitions
(use-package dumb-jump
  :defer 0
  :init (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Automaticaly updates packages
(use-package auto-package-update
  :custom
  (auto-package-update-interval 30)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Log startup benchmark
(defun echo-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections"
	   (format "%.2f secs"
		   (float-time
		    (time-subtract after-init-time
				   before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'echo-startup-time)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval progn
           (defun cider-send-reset nil
             "Send commands to CIDER to restart the development environment."
             (interactive)
             (cider-interactive-eval "(do (ns user) (reset))"))
           (defun cider-send-reload nil
             "Send commands to CIDER to reload the development environment."
             (interactive)
             (cider-interactive-eval "(do (ns user) (reload))"))
           (defun cider-send-reload-tests nil
             "Send commands to CIDER to reload test namespaces."
             (interactive)
             (cider-interactive-eval "(do (ns user) (reload-tests))"))
           (defun cider-send-go nil
             "Send commands to CIDER to start the system."
             (interactive)
             (cider-interactive-eval "(do (ns user) (go))"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-quick-access ((t (:foreground "orange")))))
