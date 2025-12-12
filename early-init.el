;; No package.el, straight will handle packages
(setq package-enable-at-startup nil)

;; Make startup visually clean and avoid flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Faster startup: GC + frame settings
(setq
 inhibit-startup-screen t
 inhibit-startup-message t
 frame-inhibit-implied-resize t
 ;; big GC during init; you can define jp/gc-high in jp.el or just inline:
 gc-cons-threshold (* 256 1024 1024)
 gc-cons-percentage 0.6
 frame-title-format nil)

;; Optional: default frame settings as early as possible
(add-to-list 'default-frame-alist '(background-color . "#000"))
(add-to-list 'default-frame-alist '(cursor-color . "#aaa"))
(add-to-list 'default-frame-alist '(tab-bar-lines . 1))

;; Optional: speed up loading by disabling file-name handlers during init
(defvar jp/file-name-handler-alist-backup file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist jp/file-name-handler-alist-backup)
            ;; drop GC back to sane levels after startup
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))
