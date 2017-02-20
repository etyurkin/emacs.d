;;; init-local --- Summary

;;; Commentary:

;;; Code:

;; fix emacs GPG error: "no usable configuration"
(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)

(use-package elpa-mirror :ensure t)
(setq elpamr-default-output-directory (expand-file-name "elpa-mirror" user-emacs-directory))

(use-package suggest :ensure t)

(use-package hound :ensure t)

(use-package paradox :ensure t)

;; zygospore lets you revert C-x 1 (delete-other-window) by pressing C-x 1 again
(use-package zygospore :ensure t)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(use-package multi-eshell :ensure t)
(defalias 'eshell 'multi-eshell)

(use-package multi-term :ensure t)
(setq multi-term-program "ESHELL")

;; https://github.com/areina/helm-dash
(use-package helm-dash :ensure t)
(setq helm-dash-browser-func 'eww)

;; http documentation
;; provider http-status-code, http-header, http-method, http-relation, media-type
(use-package know-your-http-well :ensure t)

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

;;
;; org mode toc, see https://github.com/snosov1/toc-org
;;
(use-package toc-org :ensure t)
(add-hook 'org-mode-hook 'toc-org-enable)
(setq toc-org-skip-pre-toc-headings t)

(defcustom toc-org-skip-pre-toc-headings nil
  "Leave headings out of the TOC that occur before the TOC itself."
  :group 'toc-org :type 'boolean)

(defun toc-org-raw-toc ()
  "Return the \"raw\" table of contents of the current file,  i.e. simply flush everything that's not a heading and strip tags."
  (let ((content (buffer-substring-no-properties
                  (point-min) (point-max))))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (keep-lines "^\*+[ ]")

      ;; don't include the TOC itself
      (goto-char (point-min))
      (re-search-forward toc-org-toc-org-regexp nil t)
      (beginning-of-line)
      (delete-region (if toc-org-skip-pre-toc-headings
                         (point-min)
                       (point))
                     (progn (forward-line 1) (point)))

      ;; strip states
      (goto-char (point-min))
      (while (re-search-forward toc-org-states-regexp nil t)
        (replace-match "" nil nil nil 1))

      ;; strip tags
      ;; TODO :export: and :noexport: tags semantic should be probably
      ;; implemented
      (goto-char (point-min))
      (while (re-search-forward toc-org-tags-regexp nil t)
        (replace-match "" nil nil))

      ;; flatten links
      (goto-char (point-min))
      (while (re-search-forward toc-org-links-regexp nil t)
        (replace-match "\\2" nil nil))

      (buffer-substring-no-properties
       (point-min) (point-max)))))

;; restclient mode
(use-package restclient :ensure t)
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))
(setq restclient-inhibit-cookies t)

;; An extension to restclient that provides org-babel support.
(use-package ob-restclient :ensure t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

;; Company-mode completion back-end for restclient-mode.
(use-package company-restclient :ensure t)
(add-to-list 'company-backends 'company-restclient)

(use-package httprepl :ensure t)

(use-package transpose-frame :ensure t)

;; display “lambda” as “λ”
(use-package pretty-lambdada :ensure t)
(pretty-lambda-for-modes)

;; Structure and Interpretation of Computer Programs book
(use-package sicp :ensure t)

;; dynamic evaluation replacement with emacs
(use-package litable :ensure t)

(use-package swift-mode :ensure t)
(use-package scala-mode :ensure t)
(use-package csharp-mode :ensure t)
(use-package helm :ensure t)

;; Calendar settings
(setq calendar-week-start-day 1)

;; Show entries for 3 days
(setq diary-number-of-entries 3)

(setq org-agenda-include-diary t)

;; Automatically show diary events
(run-at-time "11:00am" (* 24 3600) 'diary)

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)) ;; keep my ~/ clean
(setq-default save-place t)                  ;; activate it for all buffers
(use-package saveplace :ensure t)            ;; get the package

(setq tab-width 4
      indent-tabs-mode nil)

(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'org-mode-hook #'whitespace-mode)

;; move between windows with the [Cmd+→],[Cmd+←], [Cmd+↓], [Cmd+↑] keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Display function documentation in minibuffer
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; https://www.emacswiki.org/emacs/dired-single.el
(use-package dired-single :ensure t)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."

  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; Insert the org entity (if available) when user does C-u SYMBOL; works for C-u *, C-u /, C-u =, etc.
(defun modi/org-entity-get-name (char)
  "Return the entity name for CHAR.  For example, return \"ast\" for *."
  (let ((ll (append org-entities-user
                    org-entities))
        e name utf8)
    (catch 'break
      (while ll
        (setq e (pop ll))
        (when (not (stringp e))
          (setq utf8 (nth 6 e))
          (when (string= char utf8)
            (setq name (car e))
            (throw 'break name)))))))

(defun modi/org-insert-org-entity-maybe (orig-fun &rest args)
  "When the universal prefix C-u is used before entering any character,
insert the character's `org-entity' name if available."
  (let ((pressed-key (this-command-keys))
        entity-name)
    (when (and (listp args) (eq 4 (car args)))
      (setq entity-name (modi/org-entity-get-name pressed-key))
      (when entity-name
        (setq entity-name (concat "\\" entity-name "{}"))
        (insert entity-name)
        (message (concat "Inserted `org-entity' "
                         (propertize entity-name
                                     'face 'font-lock-function-name-face)
                         " for the symbol "
                         (propertize pressed-key
                                     'face 'font-lock-function-name-face)
                         "."))))
    (when (null entity-name)
      (apply orig-fun args))))

(advice-add 'org-self-insert-command :around #'modi/org-insert-org-entity-maybe)

(provide 'init-local)
;;; init-local ends here
