;;; init-themes --- Summary
;;; Commentary:
;;; Code:

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Display date/time in mode line
(setq display-time-day-and-date t)
(setq display-time-string-forms
      '((propertize (format-time-string "%a %b %d, %R" now) 'face 'bold)))
(display-time-mode t)

(use-package doom-themes)
(setq doom-one-brighter-comments t)
(load-theme 'doom-one t)

(use-package spaceline)
(require 'spaceline-config)

(set-face-attribute 'mode-line nil :foreground "gray" :background nil :box nil)
(set-face-attribute 'mode-line-inactive nil :foreground "SkyBlue4" :background nil :box nil)
(set-face-attribute 'powerline-active1 nil :foreground "SkyBlue1" :background "SkyBlue4")
(set-face-attribute 'powerline-active2 nil :foreground "white" :background nil)
(set-face-attribute 'powerline-inactive1 nil :foreground "SkyBlue4" :background nil)
(set-face-attribute 'powerline-inactive2 nil :foreground "SkyBlue4" :background nil)
(set-face-attribute 'spaceline-highlight-face nil :foreground "SkyBlue1" :background "SkyBlue4")
(setq powerline-default-separator 'curve)

(spaceline-emacs-theme)
(spaceline-toggle-minor-modes-off)

;; Cursor color
(set-cursor-color "#C2C8D3")

;; (set-face-foreground 'vertical-border "SkyBlue4")

;; transparency settings
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; Put modeline to the top
;; (setq-default header-line-format mode-line-format)
;; (setq-default mode-line-format nil)

;; (setq-default custom-enabled-themes '(doom-one))

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
;;; init-themes.el ends here
