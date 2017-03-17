(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-doom-theme/")
(load "~/.emacs.d/themes/emacs-doom-theme/doom.el")
(load-theme 'doom-one t)

;; transparency settings
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

(setq-default custom-enabled-themes '(doom-one))

;; Display date/time in mode line
(setq display-time-day-and-date t)
(setq display-time-string-forms
      '((propertize (format-time-string "%a %b %d, %R" now) 'face 'bold)))

(display-time-mode t)

(use-package spaceline :ensure t)
(require 'spaceline-config)
(spaceline-emacs-theme)

(setq-default whitespace-style '(face))

(require 'org)
(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")))

(set-face-background 'org-level-1 nil)

(custom-set-faces
 '(org-level-1 ((t (:box nil :height 130)))))

;; Toggle display of entities as UTF-8 characters.
(setq org-pretty-entities t)

(setq org-todo-keywords
      '((sequence "TODO" "ACTIVE" "|" "DONE" "DELEGATED")))

;; hide formatting markers in org
(setq org-hide-emphasis-markers t)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333333")

(when (not (display-graphic-p))
  (set-background-color "black")
  (set-face-background 'hl-line "black")
  (set-face-background 'mode-line "black")
  (set-face-background 'mode-line-highlight "black"))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;;(add-hook 'after-init-hook 'reapply-themes)

;; white mouse cursor
(set-mouse-color "white")

(provide 'init-themes)
