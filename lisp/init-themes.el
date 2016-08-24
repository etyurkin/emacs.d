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
