;;; early-init.el --- Pre-frame, pre-package setup -*- lexical-binding: t; -*-

;; Bump GC threshold for startup (restored in config.org Post init).
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Prevent package.el from auto-initializing; init.el handles this.
(setq package-enable-at-startup nil)

;; Several old third-party package autoloads lack a `lexical-binding' cookie.
;; Emacs 30+ warns about each on load, which pops up the *Warnings* window on
;; every startup (an extra buffer/window in the layout).  Keep logging them but
;; stop them from raising the window.  These are still recorded in *Warnings*.
(setq warning-suppress-types '((files)))

;; Keep auto-generated state (caches written on exit) out of the top-level
;; .emacs.d directory.  Defined here because early-init.el is the first thing
;; to read these caches; config.org and the theme component write to it.
(defvar kwarks/cache-directory
  (expand-file-name "cache/" user-emacs-directory)
  "Directory for auto-generated runtime caches.")
(make-directory kwarks/cache-directory t)

;; Make frame sizing pixel-precise so restored geometry is exact on a HiDPI
;; display (otherwise size is rounded to whole character cells).
(setq frame-resize-pixelwise t)

;; Suppress GUI chrome before the frame is drawn.
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (ns-transparent-titlebar . t)))

(unless (eq system-type 'darwin)
  (push '(menu-bar-lines . 0) default-frame-alist))

;; Read cached theme colors saved by the previous session so the frame
;; starts with the right background immediately instead of flashing white.
;; The cache is written by `kwarks/save-theme-cache' (see theme component).
(let ((cache (expand-file-name "theme-cache.el" kwarks/cache-directory)))
  (when (file-exists-p cache)
    (condition-case err
        (load cache nil t t)
      (error
       (message "kwarks: ignoring invalid theme-cache.el (%S)" err)
       (ignore-errors (delete-file cache))))))

;; Read cached frame geometry (size + position) saved by the previous session
;; and push it into `default-frame-alist' so the FIRST frame is created at the
;; right place and size.  We restore geometry at frame-creation time -- not via
;; desktop.el's post-creation `set-frame-position' -- because the latter drifts
;; on macOS multi-monitor/mixed-DPI setups (bug#65840, open since Emacs 29).
;; The cache is written on exit by `kwarks/save-frame-geometry' (see config.org
;; Sessions).  Geometry is also filtered out of the frameset there so desktop
;; restores buffers/splits but never touches size or position.
(let ((cache (expand-file-name "frame-geometry.el" kwarks/cache-directory)))
  (when (file-exists-p cache)
    (load cache nil t t)))

;; Suppress startup clutter.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      use-file-dialog nil
      use-dialog-box nil)

;; Never let internal changes (font, menubar, toolbar, modeline) implicitly
;; resize the frame.  This is what kept shrinking the desktop-restored frame
;; after startup.  It does not affect manual or programmatic resizing.
;; Emacs 31 adds the value `force', which also inhibits implied resizing while
;; the frame is being CREATED (t only covers post-creation); fall back to t on
;; older versions.
(setq frame-inhibit-implied-resize (if (>= emacs-major-version 31) 'force t))

;;; early-init.el ends here
