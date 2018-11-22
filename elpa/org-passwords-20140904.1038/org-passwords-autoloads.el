;;; org-passwords-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-passwords" "org-passwords.el" (0 0 0 0))
;;; Generated autoloads from org-passwords.el

(autoload 'org-passwords-mode "org-passwords" "\
Mode for storing passwords

\(fn)" t nil)

(autoload 'org-passwords "org-passwords" "\
Open the password file. Open the password file defined by the
variable `org-password-file' in read-only mode and kill that
buffer later according to the value of the variable
`org-passwords-time-opened'. It also adds the `org-password-file'
to the auto-mode-alist so that it is opened with its mode being
`org-passwords-mode'.

With prefix arg ARG, the command does not set up a timer to kill the buffer.

With a double prefix arg \\[universal-argument] \\[universal-argument], open the file for editing.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-passwords" '("org-passwords-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-passwords-autoloads.el ends here
