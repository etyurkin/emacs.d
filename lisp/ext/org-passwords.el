;;; C-c C-p u - get user name
;;; C-c C-p p - get password
;;; C-c C-p g - generate password
;;; C-c C-p h - generate hashed password

;;; For password generation install pwgen
;;; For hashed passwords install openssl

;;; See https://bitbucket.org/alfaromurillo/org-passwords.el/src/master/org-passwords.el
;;; See http://hashapass.com/

;; disable encrypted & temporary files scanning
(setq dabbrev-ignored-buffer-regexps '(".*\.gpg$" "^ [*].*"))

(defun kwarks--company-dabbrev-ignore (buffer)
  (let (res)
    ;; don't search in encrypted files, or hidden buffers
    (dolist (re dabbrev-ignored-buffer-regexps res)
      (if (string-match-p re (buffer-name buffer))
          (setq res t)))))

(setq company-dabbrev-ignore-buffers 'kwarks--company-dabbrev-ignore)

(require 'org)

(define-derived-mode kwarks/org-passwords-mode org-mode
  "org-passwords-mode"
  "Mode for storing passwords"
  nil)

(defgroup kwarks/org-passwords nil
  "Options for password management."
  :group 'org)

(defcustom kwarks/org-passwords-file "~/Dropbox/private/passwords.org.gpg"
  "Default file name for the file that contains the passwords."
  :type 'file
  :group 'kwarks/org-passwords)

(defcustom kwarks/org-passwords-kill-password-buffer-timer-time 60
  "Time in seconds that the password file will remain open.  It
  may be an integer or a floating point number."
  :type 'number
  :group 'kwarks/org-passwords)

(setq kwarks/org-passwords-kill-password-buffer-timer nil)

(defcustom kwarks/org-passwords-wait-time "30 sec"
  "The default period to wait before erasing the password from the clipboard.
Must be compatible with `run-at-time'."
  :type 'string
  :group 'kwarks/org-passwords)

(defcustom kwarks/org-passwords-system-clipboard-only nil
  "If T password will not put generated password to Emacs kill ring."
  :type 'boolean
  :group 'kwarks/org-passwords)

(defun kwarks/string->clipboard (string)
  "Copy STRING to system clipboard."
  (if kwarks/org-passwords-system-clipboard-only
      (funcall interprogram-cut-function string)
    (kill-new string)))

(defun kwarks/org-passwords-copy-username ()
  "Put username associated with the entry at the location of the cursor into system clipboard."
  (interactive)
  (kwarks/string->clipboard (org-entry-get (point) "USERNAME" t)))

(defun kwarks/org-passwords-copy-password ()
  "Put password associated with the entry at the location of the cursor into system clipboard."
  (interactive)
  (kwarks/string->clipboard (org-entry-get (point) "PASSWORD")))

(defun kwarks/org-passwords-open-url ()
  "Browse the URL associated with the entry at the location of the cursor."
  (interactive)
  (browse-url (org-entry-get (point) "URL" t)))

(defun kwarks/org-passwords-random-password ()
  (interactive)
  (let ((pwd (string-trim (shell-command-to-string "pwgen -n -c -N1 10"))))
    (kwarks/string->clipboard pwd)
    (run-at-time kwarks/org-passwords-wait-time nil (lambda () (kwarks/string->clipboard "")))
    (message "Generated password has been copied to clipboard")))

(defun kwarks/org-passwords-hashed-password ()
  "Generate strong password based on parameter and master password."
  (interactive)
  (let* ((param (read-string "parameter: "))
         (password (read-passwd "master password: "))
         (hashed-pwd (trim (shell-command-to-string
                            (format "echo -n %s | openssl dgst -sha1 -binary -hmac %s | openssl enc -base64 | cut -c 1-8" param password)))))
    (kwarks/string->clipboard hashed-pwd)
    (run-at-time kwarks/org-passwords-wait-time nil (lambda () (kwarks/string->clipboard "")))
    (message "Generated password has been copied to clipboard")))

(defun kwarks/org-passwords (&optional arg)
  (interactive "P")
  (if kwarks/org-passwords-file
      (progn
        (add-to-list 'auto-mode-alist
                     (cons
                      (regexp-quote
                       (expand-file-name kwarks/org-passwords-file))
                      'kwarks/org-passwords-mode))
        (if kwarks/org-passwords-kill-password-buffer-timer
            (setq kwarks/org-passwords-kill-password-buffer-timer nil))
        (and (or
              (and arg (find-file kwarks/org-passwords-file))
              (find-file-read-only kwarks/org-passwords-file))
             (kwarks/org-passwords-set-up-kill-password-buffer-timer)))
    (minibuffer-message "No default password file defined.  Set the variable `org-password-file'.")))

(defun kwarks/org-passwords-set-up-kill-password-buffer-timer ()
  (setq kwarks/org-passwords-kill-password-buffer-timer
        (run-with-idle-timer kwarks/org-passwords-kill-password-buffer-timer-time
                             nil
                             '(lambda ()
                                (if (get-file-buffer kwarks/org-passwords-file)
                                    (kill-buffer
                                     (get-file-buffer kwarks/org-passwords-file)))))))

(define-key kwarks/org-passwords-mode-map (kbd "C-c C-p u") 'kwarks/org-passwords-copy-username)
(define-key kwarks/org-passwords-mode-map (kbd "C-c C-p p") 'kwarks/org-passwords-copy-password)
(define-key kwarks/org-passwords-mode-map (kbd "C-c C-p g") 'kwarks/org-passwords-random-password)
(define-key kwarks/org-passwords-mode-map (kbd "C-c C-p h") 'kwarks/org-passwords-hashed-password)
(define-key kwarks/org-passwords-mode-map (kbd "C-c C-p o") '(lambda ()
                                                               (interactive)
                                                               (kwarks/org-passwords-copy-password)
                                                               (kwarks/org-passwords-open-url)))
