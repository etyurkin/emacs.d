;;; early-init.el --- Pre-frame, pre-package setup -*- lexical-binding: t; -*-

;; Bump GC threshold for startup (restored in config.org Post init).
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Prevent package.el from auto-initializing; init.el handles this.
(setq package-enable-at-startup nil)

;; Suppress GUI chrome and set initial colors before the frame is drawn --
;; avoids the white flash and toolbar flicker on startup.
;; Background/foreground match doom-one so the frame starts dark immediately;
;; the real theme refines these once it loads.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (ns-transparent-titlebar . t)
        (ns-appearance . dark)
        (background-color . "#282c34")
        (foreground-color . "#bbc2cf")))

(unless (eq system-type 'darwin)
  (push '(menu-bar-lines . 0) default-frame-alist))

;; Suppress startup clutter.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      use-file-dialog nil
      use-dialog-box nil)

;; Don't resize the frame pixel-by-pixel when setting fonts/themes --
;; prevents a sequence of resize events on startup.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
