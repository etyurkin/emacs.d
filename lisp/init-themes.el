;; (require-package 'color-theme-sanityinc-solarized)
;; (require-package 'color-theme-sanityinc-tomorrow)

;; ;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(sanityinc-solarized-light))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-doom-theme/")
(load "~/.emacs.d/themes/emacs-doom-theme/doom.el")
(load-theme 'doom-one t)

;; transparency settings
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

(setq-default custom-enabled-themes '(doom-one))

(use-package powerline :ensure t)

;; Display date/time in mode line
(setq display-time-day-and-date t)
(setq display-time-string-forms
      '((propertize (format-time-string "%a %b %d, %R" now) 'face 'bold)))

(display-time-mode t)

(setq powerline-display-hud nil)

(custom-set-faces
 '(mode-line ((t (:underline nil :foreground "light sea green" :background "#152F3F" :box nil))))
 '(mode-line-inactive ((t (:underline nil))))
 '(powerline-active1 ((t (:background "#154662" :foreground "light sea green"))))
 '(powerline-active2 ((t (:background "#152F3F" :foreground "dark cyan"))))
 '(powerline-arrow-shape (quote arrow)))

(powerline-default-theme)

(when (not (display-graphic-p))
  (set-background-color "black")
  (set-face-background 'hl-line "black")
  (set-face-background 'mode-line "black")
  (set-face-background 'mode-line-highlight "black")
  (set-face-background 'powerline-inactive1 "black")
  (set-face-background 'powerline-inactive2 "black")
  (set-face-background 'linum "black"))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;;(add-hook 'after-init-hook 'reapply-themes)


;; ;;------------------------------------------------------------------------------
;; ;; Toggle between light and dark
;; ;;------------------------------------------------------------------------------
;; (defun light ()
;;   "Activate a light color theme."
;;   (interactive)
;;   (color-theme-sanityinc-solarized-light))

;; (defun dark ()
;;   "Activate a dark color theme."
;;   (interactive)
;;   (color-theme-sanityinc-solarized-dark))


(provide 'init-themes)
