;;; init-local --- Summary

;;; Commentary:

;;; Code:

;; fix emacs GPG error: "no usable configuration"
(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)

(use-package elpa-mirror)
(setq elpamr-default-output-directory (expand-file-name "elpa-mirror" user-emacs-directory))

(use-package suggest)

(use-package hound)

(use-package paradox)

;; zygospore lets you revert C-x 1 (delete-other-window) by pressing C-x 1 again
(use-package zygospore)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(use-package multi-eshell)
(defalias 'eshell 'multi-eshell)

;; http documentation
;; provider http-status-code, http-header, http-method, http-relation, media-type
(use-package know-your-http-well)

;; disable minimize emacs by ctrl-z
(put 'suspend-frame 'disabled t)

;; init backups
(setq
 backup-by-copying t
 backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
 delete-old-versions t
 kept-new-versions 16
 kept-old-versions 2
 version-control t)

;; restclient mode
(use-package restclient)
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))
(setq restclient-inhibit-cookies t)

;; An extension to restclient that provides org-babel support.
(use-package ob-restclient)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

;; Company-mode completion back-end for restclient-mode.
(use-package company-restclient)
(add-to-list 'company-backends 'company-restclient)

(use-package httprepl)

(use-package transpose-frame)

;; display “lambda” as “λ”
(use-package pretty-lambdada)
(pretty-lambda-for-modes)

;; Structure and Interpretation of Computer Programs book
(use-package sicp)

;; dynamic evaluation replacement with emacs
(use-package litable)

(use-package swift-mode)
(use-package scala-mode)
(use-package csharp-mode)
(use-package helm)

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)) ;; keep my ~/ clean
(setq-default save-place t)                  ;; activate it for all buffers
(use-package saveplace)            ;; get the package

(setq tab-width 4
      indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'org-mode-hook #'whitespace-mode)

;; move between windows with the [Cmd+→],[Cmd+←], [Cmd+↓], [Cmd+↑] keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Display function documentation in minibuffer
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)



(provide 'init-local)
;;; init-local ends here
