;; -*- lexical-binding: t; -*-
(defvar jp--initial-tabs-done nil)
(defvar jp/file-name-handler-alist-backup file-name-handler-alist)

;; Fast path: disable heavy features during init
(defconst jp/gc-high most-positive-fixnum)
(defconst jp/gc-normal (* 16 1024 1024))

(defun jp/setup-initial-frame-layout (&optional force)
  "Tab1: init.el | *Messages*
   Tab2: *scratch*
   With FORCE (C-u), rebuild even if already done."
  (interactive "P")
  (when (or force
	    (not jp--initial-tabs-done))
    ;; Tab 2 with scratch
    (tab-bar-new-tab)
    (switch-to-buffer (get-buffer "*scratch*"))
    (delete-other-windows)
    (ignore-errors (tab-bar-rename-tab "scratch"))

    ;; Tab 1: 1 left | 2 right
    (tab-bar-select-tab 1)
    (delete-other-windows)
    (find-file (or user-init-file
		   (expand-file-name "~/.emacs")))

    (let* ((w-right (split-window-right)) w-top w-bot)
      (select-window w-right)
      (set-window-buffer (selected-window) (get-buffer "*Messages*"))
      (set-window-buffer (split-window-below) (get-buffer "*scratch*")))

    (balance-windows)
    (ignore-errors (tab-bar-rename-tab "main"))

    (setq jp--initial-tabs-done t)))

(add-hook 'window-setup-hook
          (lambda ()
            (set-frame-parameter nil 'fullscreen 'fullheight)
            (set-frame-size nil 256 (floor (/ (display-pixel-height)
					      (frame-char-height))))))

(defun jp/flash-mode-line ()
  "Briefly flash mode-line instead of bell."
  (let ((orig (face-foreground 'mode-line)))
    (set-face-foreground 'mode-line "#ccc")
    (run-with-timer 0.1
                    nil
                    (lambda (c)
                      (set-face-foreground 'mode-line c))
                    orig)))

(defun jp/numbered-tab (tab i)
  (propertize
   (format
    (number-to-string i)
    (alist-get 'name tab))
   'face (list
	  (append '(:height 1.15)
                  (if (eq (car tab) 'current-tab)
		    '(:foreground "orange")
                    '(:foreground "#555"))))))

(setq
 ;; Startup behavior
 inhibit-startup-screen t
 inhibit-startup-message t
 package-enable-at-startup nil
 frame-inhibit-implied-resize t
 ;; Compilation and warnings
 byte-compile-warnings '(not obsolete)
 warning-suppress-log-types '((comp) (bytecomp))
 native-comp-async-report-warnings-errors 'silent
 ;; Performance: postpone GC, simplify frame painting
 gc-cons-threshold jp/gc-high
 gc-cons-percentage 0.6
 frame-title-format nil
 ;; Light bell
 ring-bell-function #'jp/flash-mode-line
 ;; Auto-revert tuning
 auto-revert-verbose nil
 auto-revert-avoid-polling t
 auto-revert-interval 5
 auto-revert-check-vc-info t
 ;; Indentation defaults
 indent-tabs-mode nil
 js-indent-level 2
 css-indent-offset 2
 plantuml-indent-level 2
 ;; Tab-bar customizations
 tab-bar-show t
 tab-bar-tab-hints nil
 tab-bar-separator " "
 tab-bar-close-button-show nil
 tab-bar-new-button-show nil
 tab-bar-button-margin nil
 tab-bar-auto-width nil
 tab-bar-new-tab-choice "*scratch*"
 tab-bar-select-tab-modifiers '(control)
 tab-bar-tab-name-format-function #'jp/numbered-tab)

(defun jp/after-startup ()
  "Post-startup restoration and lazy global enables."

  (setq
   ;; Restore performance settings
   gc-cons-threshold jp/gc-normal
   gc-cons-percentage 0.1
   file-name-handler-alist jp/file-name-handler-alist-backup)

  ;; Enable visual globals lazily to avoid stutter on first paint
  (run-with-idle-timer 0.30 nil #'global-display-line-numbers-mode 1)
  (run-with-idle-timer 0.50 nil #'global-hl-line-mode 1)
  (run-with-idle-timer 0.70 nil #'global-auto-revert-mode 1)

  ;; Divide the frame
  (jp/setup-initial-frame-layout)

  ;; OFF
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  ;; ON
  (column-number-mode 1)
  (winner-mode 1)
  (which-key-mode 1)
  (smartparens-global-mode 1)
  (vertico-mode 1)
  (global-company-mode 1)
  (marginalia-mode 1)

  (message "First install: ~121.8s")
  (message "Init time: %s | GC: %d" (emacs-init-time) gcs-done))

(add-hook 'emacs-startup-hook #'jp/after-startup)

;; Font faces
(defun jp/apply-faces ()
  (custom-theme-set-faces
   'user
   ;; base
   '(default ((t (:background "#000" :height 115))))
   '(fringe  ((t (:background "#000"))))
   '(cursor  ((t (:background "#aaa"))))
   ;; highlights
   '(highlight ((t (:background "#1e1e1e" :foreground unspecified :underline unspecified))))
   ;; line numbers
   '(line-number ((t (:foreground "#555" :background unspecified))))
   '(line-number-current-line ((t (:foreground "orange" :background "#1e1e1e"))))
   ;; tab-bar
   '(tab-bar ((t (:background "#000"))))
   ;; comments
   '(font-lock-comment-face ((t (:foreground "#555"))))))

;; Main theme
(advice-add 'load-theme :after (lambda (&rest _) (jp/apply-faces)))
(load-theme 'wombat t)

(add-to-list 'default-frame-alist '(background-color . "#000"))
(add-to-list 'default-frame-alist '(cursor-color . "#aaa"))
(add-to-list 'default-frame-alist '(tab-bar-lines . 1))

;; Global text scaling (named commands)
(defvar jp/base-height (face-attribute 'default :height))

(defun jp/global-text-scale (n)
  "N>0 grow, N<0 shrink, N=0 reset."
  (interactive "p")
  (let* ((cur (face-attribute 'default :height))
         (delta (cond ((= n 0) (- jp/base-height cur))
                      (t (* n 10)))))
    (set-face-attribute 'default nil :height (+ cur delta))))

;; Global zoom
(global-set-key (kbd "C-+") (lambda () (interactive) (jp/global-text-scale +1)))
(global-set-key (kbd "C--") (lambda () (interactive) (jp/global-text-scale -1)))
(global-set-key (kbd "C-M-0") (lambda () (interactive) (jp/global-text-scale 0)))

;; Branch resolver
(defun jp/current-branch ()
  (or (when buffer-file-name
        (ignore-errors (vc-working-branch buffer-file-name)))
      (when (and (executable-find "git")
                 (eq (vc-backend (or buffer-file-name default-directory)) 'Git))
        (with-temp-buffer
          (let ((default-directory (or (vc-root-dir) default-directory)))
            (when (eq 0 (call-process "git" nil t nil "symbolic-ref" "--short" "-q" "HEAD"))
              (string-trim (buffer-string))))))))

;; Window numbers by screen position (L/R, top/bottom)
(defvar jp/window-number-map nil)

(defun jp--renumber-windows ()
  (let* ((wins (window-list (selected-frame) 'no-mini))
         (sorted (sort (copy-sequence wins)
                       (lambda (a b)
                         (let ((ea (window-edges a))
                               (eb (window-edges b)))
                           (or (< (nth 1 ea) (nth 1 eb))
                               (and (= (nth 1 ea) (nth 1 eb))
                                    (< (nth 0 ea) (nth 0 eb)))))))))
    (setq jp/window-number-map nil)
    (let ((i 1))
      (dolist (w sorted)
        (set-window-parameter w 'jp/window-number i)
        (push (cons i w) jp/window-number-map)
        (setq i (1+ i))))
    (setq jp/window-number-map (nreverse jp/window-number-map))))

(add-hook 'window-configuration-change-hook #'jp--renumber-windows)
(add-hook 'buffer-list-update-hook #'jp--renumber-windows)

(defun jp/mode-line-window-number ()
  (let ((n (window-parameter nil 'jp/window-number)))
    (when n
      (propertize
       (number-to-string n)
       'face '(:foreground "orange")))))

;; Modeline
(setq-default
 mode-line-format
 '(" "
   (:eval (jp/mode-line-window-number))
   (:eval (cond (buffer-read-only " RO")
                ((buffer-modified-p) " *")
                (t "")))
   (:eval (propertize " %b" 'face '(:foreground "orange")))
   (:eval (propertize " %I %l,%c %p" 'face '(:foreground "#555")))
   " "
   (:eval mode-name)
   (:eval (let ((branch (jp/current-branch)))
            (when branch
              (concat " " (propertize (format " %s" branch)
                                      'face '(:foreground "#ccc"))))))))

(add-hook 'after-save-hook #'force-mode-line-update)

;; Change windows quickly
(defun jp/select-window-by-number (n)
  "Select window N by screen position."
  (interactive "nWindow number: ")
  (let ((w (cdr (assoc n jp/window-number-map))))
    (when (window-live-p w) (select-window w))))

(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key (kbd (format "M-%d" n))
		    `(lambda () (interactive) (jp/select-window-by-number ,n)))))

;; Windows history
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-c w r") #'winner-redo)

;; PACKAGE
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

(straight-use-package 'use-package)
(setq
 straight-use-package-by-default t
 use-package-always-defer t)

;; No-littering dump emacs's autofiles where you want
(use-package no-littering
  :demand t
  :config
  ;; Create directories in wich you'd like to save the litter
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "~/.emacs.d/var/auto-save/") t))
	backup-directory-alist
	`((".*" . ,(no-littering-expand-var-file-name "~/.emacs.d/var/backup/")))))

;; macOS GUI only
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables
        '("PATH" "MANPATH" "DOCKER_HOST"
          "TESTCONTAINERS_DOCKER_SOCKET_OVERRIDE"
          "TESTCONTAINERS_RYUK_DISABLED"))
  :config
  (exec-path-from-shell-initialize))

;; Smartparens makes the pairs (), [], {}, etc. always even
(use-package smartparens :demand t)

;; Slurp code
(with-eval-after-load 'smartparens
  (define-key smartparens-mode-map (kbd "C-c <left>") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c <right>") #'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c C-.") #'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c C-,") #'sp-backward-barf-sexp))

;; MoveText helps with M-up/down line swap
(use-package move-text :demand t :config (move-text-default-bindings))

;; Multiple Cursors allow multi line selection for ease edition
(use-package multiple-cursors :demand t)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Treemacs is way better than dired
(use-package treemacs
  :config
  (progn
    (set-face-attribute 'treemacs-root-face nil
                        :foreground "#eee"
			:height 1
                        :underline nil)
    (set-face-attribute 'treemacs-directory-face nil
                        :foreground "#ccc")
    (treemacs-resize-icons 16)))

;; Vertico
(use-package vertico
  :config
  (require 'vertico-directory)
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (global-set-key (kbd "M-R") #'vertico-repeat))

;; Marginalia annotations
(use-package marginalia)

;; Orderless: strong fuzzy matching with sane fallbacks
(use-package orderless
  :init
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   completion-ignore-case t
   orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)
   read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t))

;; Consult: fast, async searching and buffers
(use-package consult
  :init
  ;; Project root for Consult commands
  (setq consult-project-function
        (lambda (_)
          (when-let* ((proj (project-current t)))
            (car (project-roots proj)))))
  :config
  ;; Preview only when you press M-.
  (consult-customize consult-buffer
                     consult-recent-file
                     consult-ripgrep
                     consult-git-grep
                     consult-grep
                     :preview-key "M-.")

  ;; Use ripgrep and fd if present
  (setq
   consult-ripgrep-args "rg --null --line-buffered --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --color=never . -e"
   consult-fd-args "fd --color=never --hidden --follow --exclude .git ~"))

;; Optional: project-scoped sources on C-x b
(with-eval-after-load 'consult
  (setq consult-buffer-sources
        (list consult--source-hidden-buffer
              consult--source-modified-buffer
              consult--source-buffer
              consult--source-project-buffer
              consult--source-recent-file
              consult--source-project-recent-file)))

;; Company mode helps with in-buffer completions
(use-package company
  :config
  (setq
   ;; Set main configurations
   company-dabbrev-other-buffers t
   company-tooltip-minimum 10
   company-tooltip-flip-when-above t
   company-show-quick-access 'left
   company-tooltip-align-annotations t
   ;; Set delay or activate manualy
   company-idle-delay 0
   company-tooltip-idle-delay 3
   company-require-match nil
   ;; List of enabled frontends
   company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
     company-preview-frontend
     company-echo-metadata-frontend)
   ;; List of enabled backends
   company-backends '(company-capf))
  
  ;; Set color for the quick access
  (custom-set-faces
   '(company-tooltip-quick-access
     ((t (:foreground "orange")))))

  (global-set-key (kbd "C-<tab>")
                  (lambda ()
                    (interactive)
                    (let ((company-tooltip-idle-delay 10))
                      (company-complete)
                      (and company-candidates
                           (company-call-frontends 'post-command))))))

;; Magit git porcelain withforge to handle PRs
(setq
 auth-sources '("~/.authinfo.gpg")
 epg-pinentry-mode 'loopback)

(use-package magit
  :config
  (setq
   ;; Always reuse the current window for Magit buffers
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge :after magit)

;; Edit ROOT and DEPTH as needed
(setq magit-repository-directories '(("~/Projects/bp" . 2)))

(defun jp/forge-pull-all (&optional root)
  "Add to Forge if needed, then `forge-pull` for every Git repo under ROOT."
  (interactive)
  (let* ((root (file-name-as-directory (or root (caar magit-repository-directories))))
         (git-dirs (directory-files-recursively root "\\`\\.git\\'" t)))
    (dolist (g git-dirs)
      (let ((repo (file-name-directory g)))
        (when (file-directory-p repo)
          (let ((default-directory repo))
            (condition-case err
                (progn
                  ;; ensure repo is known to Forge, then pull topics/PRs
                  (ignore-errors (forge-add-repository))
                  (forge-pull)
                  ;; optional: also update Git remotes for branches
                  ;; (magit-fetch-all)
                  (message "forge-pull: %s" repo))
              (error (message "forge-pull failed in %s: %s" repo err)))))))))

;; Clojure-mode for support to .clj files and Cider for extra support
(use-package clojure-mode)
(use-package cider
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
   url-show-status nil
   url-queue-timeout 10
   lsp-keymap-prefix "C-c l"
   lsp-clojure-custom-server-command '("/opt/homebrew/bin/clojure-lsp")
   lsp-completion-show-kind nil
   lsp-headerline-breadcrumb-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-workspace-status-enable nil
   lsp-enable-file-watchers t
   lsp-log-io nil
   lsp-lens-enable nil
   lsp-response-timeout 1
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-position 'top
   lsp-ui-sideline-show-diagnostics t
   lsp-eldoc-render-all nil
   lsp-signature-render-documentation t))

(use-package consult-lsp :after lsp-mode)

;; Common lisp support with Slime
(use-package slime
  :config
  (setq
   inferior-lisp-program "/opt/homebrew/bin/sbcl"
   slime-contribs '(slime-fancy)
   slime-repl-history-file "~/.emacs.d/slime-history.eld"
   slime-repl-history-size 1000
   slime-repl-history-remove-duplicates t
   slime-net-coding-system 'utf-8-unix))

;; Plantuml-mode helps versionize diagrams
(use-package plantuml-mode
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; Dumb Jump helps find definitions
(use-package dumb-jump
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))
