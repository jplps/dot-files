(load (expand-file-name "jp.el" user-emacs-directory))

;; Full height emacs with minimal width
(add-hook 'window-setup-hook
          (lambda ()
            (set-frame-parameter nil 'fullscreen 'fullheight)
            (set-frame-size nil 256 (floor (/ (display-pixel-height)
					      (frame-char-height))))))

;; Prepare init stuff
(setq
 ;; Compilation and warnings
 byte-compile-warnings '(not obsolete)
 warning-suppress-log-types '((comp) (bytecomp))
 native-comp-async-report-warnings-errors 'silent
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

;; Add custom startup hook
(add-hook 'emacs-startup-hook #'jp/after-startup)

;; Main theme
(advice-add 'load-theme :after (lambda (&rest _) (jp/apply-faces)))
(load-theme 'wombat t)

;; Global zoom
(global-set-key (kbd "C-+") (lambda () (interactive) (jp/global-text-scale +1)))
(global-set-key (kbd "C--") (lambda () (interactive) (jp/global-text-scale -1)))
(global-set-key (kbd "C-M-0") (lambda () (interactive) (jp/global-text-scale 0)))

(add-hook 'window-configuration-change-hook #'jp/renumber-windows)
(add-hook 'buffer-list-update-hook #'jp/renumber-windows)

;; Control where the result goes
(global-set-key (kbd "C-c w") #'jp/with-output-to-window)

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

;; Binds to handle windows controls
(dotimes (i 9)
  (let ((n (1+ i)))
    (global-set-key (kbd (format "M-%d" n))
		    `(lambda () (interactive) (jp/select-window-by-number ,n)))))

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

;; GC tuning via GCMH (Garbage Collector Magic Hack)
(use-package gcmh
  :init
  (setq
   gcmh-idle-delay 5
   ;; When busy, allow big allocations before GC
   gcmh-high-cons-threshold (* 256 1024 1024)
   ;; When idle / normal, keep this as the "small" threshold
   gcmh-low-cons-threshold  (* 16 1024 1024))
  :config
  (gcmh-mode 1))

;; No-littering dump emacs's autofiles where you want
(use-package no-littering
  :init
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "~/.emacs.d/var/auto-save/") t))
        backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "~/.emacs.d/var/backup/")))))

;; macOS GUI only
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-arguments '()
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-quiet t)

  (exec-path-from-shell-copy-envs
   '("PATH" "NVM_DIR" "DOCKER_HOST" "TESTCONTAINERS_RYUK_DISABLED")))

;; Smartparens makes the pairs (), [], {}, etc. always even
(use-package smartparens
  :hook ((prog-mode . smartparens-mode))
  :config
  (define-key smartparens-mode-map (kbd "C-c <left>") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c <right>") #'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-c C-.") #'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-c C-,") #'sp-backward-barf-sexp))

;; MoveText helps with M-up/down line swap
(use-package move-text :demand t :config (move-text-default-bindings))

;; Multiple Cursors allow multi line selection for ease edition
(use-package multiple-cursors
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

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

;; Ivy handles completion; Counsel provides commands; Swiper handles in-buffer searching.
(use-package ivy
  :demand t
  :init
  (setq
   ;; Show index (current/total) in the minibuffer
   ivy-count-format "%d/%d "
   ivy-use-selectable-prompt t
   ivy-use-virtual-buffers t
   ivy-wrap nil
   ivy-extra-directories nil

   ;; Use fuzzy matching where it makes sense
   ivy-re-builders-alist
   '((swiper-isearch . ivy--regex-plus)
     (counsel-rg . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1))

;; Configure Counsel as Ivy-powered replacements for core commands
(use-package counsel
  :after ivy
  :init (setq counsel-mode-override-describe-bindings t)
  :config
  (counsel-mode 1)
  ;; Use Counsel for common global bindings
  (global-set-key (kbd "M-x")     #'counsel-M-x)
  (global-set-key (kbd "C-x C-f") #'counsel-find-file)
  (global-set-key (kbd "C-h f")   #'counsel-describe-function)
  (global-set-key (kbd "C-h v")   #'counsel-describe-variable)
  (global-set-key (kbd "C-h b")   #'counsel-descbinds)
  (global-set-key (kbd "C-x b")   #'ivy-switch-buffer)
  (global-set-key (kbd "C-c s r") #'counsel-rg))

;; Use Swiper for in-buffer incremental searching
(use-package swiper
  :after ivy
  :bind
  ;; Replace default C-s with Swiper-based search
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)))

;; Make Ivy display richer information (buffer size, modes, etc.)
(use-package ivy-rich
  :after ivy
  :init (setq ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
  (ivy-rich-project-root-cache-mode 1))

;; Use ripgrep with projectile
(use-package ripgrep)

;; Projectile for fast project navigation
(use-package projectile
  :init
  (setq
   projectile-project-search-path '("~/Projects")
   projectile-indexing-method 'alien
   projectile-enable-caching t
   projectile-globally-ignored-directories
   '(".git" ".svn" ".hg" "node_modules" "dist" "build" "target")
   projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode 1))

;; Integrate Projectile with Ivy via counsel-projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :init (setq counsel-projectile-sort-files t)
  :config
  (counsel-projectile-mode 1)
  ;; Main project entrypoints
  (define-key projectile-mode-map (kbd "C-c p p") #'counsel-projectile-switch-project)
  (define-key projectile-mode-map (kbd "C-c p f") #'counsel-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c p s r") #'counsel-projectile-rg)
  (define-key projectile-mode-map (kbd "C-c p b") #'counsel-projectile-switch-to-buffer))

;; Company mode helps with in-buffer completions
(use-package company
  :bind (("C-<tab>" . jp/company-complete-once))
  :init (jp/company-complete-once)
  :config
  (setq
   company-dabbrev-other-buffers t
   company-tooltip-minimum 10
   company-tooltip-flip-when-above t
   company-show-quick-access 'left
   company-tooltip-align-annotations t
   company-idle-delay 0
   company-tooltip-idle-delay 3
   company-require-match nil
   company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
     company-echo-metadata-frontend)
   company-backends '(company-capf))

  (custom-set-faces
   '(company-tooltip-quick-access
     ((t (:foreground "orange"))))))

;; Magit git porcelain withforge to handle PRs
(use-package magit
  :commands (magit-status magit-dispatch)
  :init
  (setq
   auth-sources '("~/.authinfo.gpg")
   epg-pinentry-mode 'loopback
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
   magit-repository-directories '(("~/Projects/bp" . 2))))

(use-package forge :after magit)

;; Clojure-mode for support to .clj files and Cider for extra support
(use-package clojure-mode
  :mode ("\\.clj[csx]?\\'" . clojure-mode))

(use-package cider
  :after clojure-mode
  :commands (cider-jack-in cider-connect cider-connect-clj cider-connect-cljs)
  :init
  (setq
   cider-repl-display-help-banner nil
   cider-repl-print-help nil
   cider-repl-pop-to-buffer-on-connect nil
   cider-repl-display-in-current-window t
   cider-jack-in-dependencies '(("com.github.flow-storm/flow-storm-dbg" "3.15.5"))))

;; Lsp backend protocols
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((clojure-mode     . lsp-deferred)
         (typescript-mode  . lsp-deferred)
         (yaml-ts-mode     . lsp-deferred)
         (lsp-mode         . lsp-enable-which-key-integration))
  :config
  (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq
   url-show-status nil
   url-queue-timeout 10
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

;; Common lisp support with Slime
(use-package slime
  :commands (slime slime-connect)
  :init
  (setq
   inferior-lisp-program "/opt/homebrew/bin/sbcl"
   slime-contribs '(slime-fancy)
   slime-repl-history-file "~/.emacs.d/slime-history.eld"
   slime-repl-history-size 1000
   slime-repl-history-remove-duplicates t
   slime-net-coding-system 'utf-8-unix))

;; Plantuml-mode helps versionize diagrams
(use-package plantuml-mode
  :mode ("\\.plantuml\\'" "\\.puml\\'")
  :init
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t))))
