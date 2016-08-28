;;; init-local --- Summary

;;; Commentary:

;;; Code

(use-package hound :ensure t)

;; org mode toc, see https://github.com/snosov1/toc-org
(use-package toc-org :ensure t)
(add-hook 'org-mode-hook 'toc-org-enable)
(setq toc-org-skip-pre-toc-headings t)

;; disable minimize emacs by ctrl-z
(put 'suspend-frame 'disabled t)

;; turn off the auto-backup feature
(setq make-backup-files nil)

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

(use-package restclient :ensure t)
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))

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

;; Highlight part of lines exceeding 115 characters length
(setq-default
 whitespace-line-column 115
 whitespace-style       '(face lines-tail))

(require 'org)
(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")))


;; Toggle display of entities as UTF-8 characters.
(setq org-pretty-entities t)

(setq org-todo-keywords
      '((sequence "TODO" "ACTIVE" "|" "DONE" "DELEGATED")))

;;Calendar settings
(setq calendar-week-start-day 1)

;; Automatically show diary events
(run-at-time "11:00am" (* 24 3600) 'diary)

;; hide formatting markers in org
(setq org-hide-emphasis-markers t)

;; Highlight current line
(global-hl-line-mode 1)

;; white mouse cursor
(set-mouse-color "white")

(setq save-place-file "~/emacs.d/saveplace") ;; keep my ~/ clean
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

(defun trim (str)
  "Remove leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun empty-string-p (string)
  "Return true if the STRING is empty or nil.  Expects string."
  (or (null string)
      (zerop (length (trim string)))))

(defun run-shell-command (cmd)
  (let ((shell-process (get-buffer-process "*shell*")))
    (cond ((not (null shell-process))
           (process-send-string "shell" (concat cmd "\n")))
          (t (message "Shell process is not running")))))

(defun proxy-on ()
  "Set proxy."
  (interactive)
  (let ((proxy-host
         (read-string "proxy host: "
                      (if (bound-and-true-p *proxy-host*) *proxy-host* ""))))
    (unless (empty-string-p proxy-host)
      (setq url-proxy-services
            `(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . ,proxy-host)
              ("https" . ,proxy-host)))
      (setenv "http_proxy" (concat "http://" proxy-host))
      (setenv "https_proxy" (concat "http://" proxy-host))
      (setenv "HTTP_PROXY" (concat "http://" proxy-host))
      (setenv "HTTPS_PROXY" (concat "http://" proxy-host))
      (run-shell-command (concat "export http_proxy=http://" proxy-host))
      (run-shell-command (concat "export https_proxy=http://" proxy-host))
      (run-shell-command (concat "export HTTP_PROXY=http://" proxy-host))
      (run-shell-command (concat "export HTTPS_PROXY=http://" proxy-host))
      (message "proxy is set to %s" proxy-host))))

(defun proxy-off ()
  "Unset proxy."
  (interactive)
  (setq url-proxy-services ())
  (setenv "http_proxy" "")
  (setenv "https_proxy" "")
  (setenv "HTTP_PROXY" "")
  (setenv "HTTPS_PROXY" "")
  (run-shell-command "unset http_proxy")
  (run-shell-command "unset https_proxy")
  (run-shell-command "unset HTTP_PROXY")
  (run-shell-command "unset HTTPS_PROXY")
  (message "proxy is off"))

(defun init-ssh-agent ()
  "Add personal rsa key to ssh agent."
  (interactive)
  (run-shell-command "eval \"$(ssh-agent -s)\"")
  (run-shell-command "ssh-add ~/.ssh/personal_rsa"))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an Emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved Emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

(defun clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; to use - select xml region, M-x xml-pretty-print-region
(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region from BEGIN to END.
You need to have 'nxml-mode'
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do this.
The function inserts linebreaks to separate tags that have nothing
but whitespace between them.
It then indents the markup by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun sudo-edit (&optional arg)
  "Reopens current buffer or ARG as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

(defun hash-pass ()
  "Generate strong password based on parameter and master password."
  (interactive)
  (let ((param (read-string "parameter: "))
        (password (read-passwd "master password: ")))
    (setq hash
          (shell-command-to-string
           (format "echo -n %s | openssl dgst -sha1 -binary -hmac %s | openssl enc -base64 | cut -c 1-8" param password)))
    (kill-new hash)
    (message "Generated hash has been copied to clipboard")))

(provide 'init-local)
;;; init-local ends here
