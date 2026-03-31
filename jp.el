;; -*- lexical-binding: t; -*-
(defvar jp--initial-tabs-done nil)

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

    (let* ((w-right (split-window-right)))
      (select-window w-right)
      (set-window-buffer (selected-window) (get-buffer "*Messages*"))
      (set-window-buffer (split-window-below) (get-buffer "*scratch*")))

    (balance-windows)
    (ignore-errors (tab-bar-rename-tab "main"))

    (setq jp--initial-tabs-done t)))

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
  "Enumerate tabs for easy handles"
  (propertize
   (number-to-string i)
   'face (if (eq (car tab) 'current-tab)
	     '(:foreground "orange")
           '(:foreground "#555"))))

(defun jp/after-startup ()
  "Post-startup restoration and lazy global enables."
  ;; Divide the frame
  (jp/setup-initial-frame-layout)

  ;; ON
  (global-hl-line-mode 1)
  (global-auto-revert-mode 1)
  (global-display-line-numbers-mode 1)
  (column-number-mode 1)
  (which-key-mode 1)

  (message "Init: %s | GC: %d | ~%.1f MB consed"
           (emacs-init-time)
           gcs-done
           (/ (* (float cons-cells-consed) 16) 1024 1024)))

(defun jp/apply-faces ()
  "Used to apply faces after loading main theme"
  (custom-theme-set-faces
   'user
   ;; base
   '(default ((t (:background "#000" :height 115))))
   '(fringe  ((t (:background "#222"))))
   '(cursor  ((t (:background "#aaa"))))
   ;; current line
   '(hl-line ((t (:background "#1e1e1e"))))
   ;; minibuffer
   '(minibuffer-prompt ((t (:foreground "orange"))))
   ;; highlights
   '(highlight ((t (:background "#1e1e1e" :foreground unspecified :underline unspecified))))
   ;; line numbers
   '(line-number ((t (:foreground "#555" :background unspecified))))
   '(line-number-current-line ((t (:foreground "orange" :background "#1e1e1e"))))
   ;; tab-bar
   '(tab-bar ((t (:background "#000" :height 135 :box (:line-width 2 :color "#000")))))
   ;; comments
   '(font-lock-comment-face ((t (:foreground "#555"))))
   ;; mode-line
   '(mode-line ((t (:background "#222" :box (:line-width 2 :color "#222")))))
   '(mode-line-inactive ((t (:foreground "#555" :background "#222" :box (:line-width 2 :color "#222")))))))

;; Global text scaling (named commands)
(defvar jp/base-height (face-attribute 'default :height))

(defun jp/global-text-scale (n)
  "N>0 grow, N<0 shrink, N=0 reset."
  (interactive "p")
  (let* ((cur (face-attribute 'default :height))
         (delta (cond ((= n 0) (- jp/base-height cur))
                      (t (* n 10)))))
    (set-face-attribute 'default nil :height (+ cur delta))))

(defvar-local jp--cached-branch 'unset)

(defun jp/current-branch ()
  "Git branch resolver with per-buffer cache."
  (when (eq jp--cached-branch 'unset)
    (setq jp--cached-branch
          (or (when buffer-file-name
                (ignore-errors (vc-working-branch buffer-file-name)))
              (when (and (executable-find "git")
                         (eq (vc-backend (or buffer-file-name
                                             default-directory))
                             'Git))
                (with-temp-buffer
                  (let ((default-directory (or (vc-root-dir)
                                               default-directory)))
                    (when (eq 0 (call-process "git" nil t nil
                                              "symbolic-ref" "--short" "-q" "HEAD"))
                      (string-trim (buffer-string)))))))))
  jp--cached-branch)

(defun jp/invalidate-branch-cache ()
  (setq jp--cached-branch 'unset))

(add-hook 'after-save-hook #'jp/invalidate-branch-cache)
(add-hook 'find-file-hook #'jp/invalidate-branch-cache)
(add-hook 'after-revert-hook #'jp/invalidate-branch-cache)

;; Map windows by number
(defvar jp--window-number-map nil)

(defun jp/renumber-windows ()
  "Window numbers by screen position (L/R, top/bottom)"
  (let* ((wins (window-list (selected-frame) 'no-mini))
         (sorted (sort (copy-sequence wins)
                       (lambda (a b)
                         (let ((ea (window-edges a))
                               (eb (window-edges b)))
                           (or (< (nth 1 ea) (nth 1 eb))
                               (and (= (nth 1 ea) (nth 1 eb))
                                    (< (nth 0 ea) (nth 0 eb)))))))))
    (setq jp--window-number-map nil)
    (let ((i 1))
      (dolist (w sorted)
        (set-window-parameter w 'jp/window-number i)
        (push (cons i w) jp--window-number-map)
        (setq i (1+ i))))
    (setq jp--window-number-map (nreverse jp--window-number-map))))

(defun jp/mode-line-window-number ()
  "Enumerate windows for easy handles"
  (let ((n (window-parameter nil 'jp/window-number)))
    (when n
      (propertize
       (number-to-string n)
       'face '(:foreground "orange")))))

(defvar jp/target-window-number nil)

(defun jp/display-buffer-in-window-number (buffer alist)
  "Display BUFFER in window whose jp/window-number is jp/target-window-number."
  (let* ((win (cdr (assoc jp/target-window-number jp--window-number-map))))
    (when (window-live-p win)
      (set-window-buffer win buffer)
      win)))

(defun jp/with-output-to-window (n command)
  "Run COMMAND, routing any display-buffer output to window N."
  (interactive "nWindow number: \nCCommand: ")
  (let ((jp/target-window-number n)
        (display-buffer-overriding-action
         '((jp/display-buffer-in-window-number) . nil)))
    (call-interactively command)))

;; Change windows quickly
(defun jp/select-window-by-number (n)
  "Select window N by screen position."
  (interactive "nWindow number: ")
  (let ((w (cdr (assoc n jp--window-number-map))))
    (when (window-live-p w) (select-window w))))

(defun jp/forge-pull-all (&optional root)
  "Add to Forge if needed, then `forge-pull` for every Git repo under ROOT."
  (interactive)
  (let* ((root (file-name-as-directory (or root (caar magit-repository-directories))))
         (git-dirs (directory-files-recursively root "\\`\\.git\\'" t)))
    (dolist (g git-dirs)
      (let ((repo (file-name-directory g)))
        (when (and (file-directory-p g) (file-directory-p repo))
          (let ((default-directory repo))
            (condition-case err
                (progn
                  (ignore-errors (forge-add-repository))
                  (forge-pull)
                  (magit-fetch-all)
                  (message "forge-pull: %s" repo))
              (error (message "forge-pull failed in %s: %s" repo err)))))))))

(defun jp/company-complete-once ()
    (interactive)
    (company-complete)
    (when (and (bound-and-true-p company-candidates)
               company-candidates)
      (company-call-frontends 'post-command)))

(defun jp/sync-all-bp-projects ()
  "Sync all git projects in ~/Projects/bp, pruning and pulling."
  (interactive)
  (let ((base-dir "~/Projects/bp"))
    (if (file-directory-p base-dir)
        (let ((repos (directory-files base-dir t "^[^.]")))
          (dolist (repo repos)
            (when (file-directory-p (expand-file-name ".git" repo))
              (message "Syncing %s..." repo)
              (let ((default-directory repo))
                (magit-call-git "fetch" "--prune" "--all")
                (magit-call-git "pull" "--all")
                (magit-call-git "remote" "prune" "origin"))))
          (message "All projects updated!"))
      (message "Directory %s not found." base-dir))))
