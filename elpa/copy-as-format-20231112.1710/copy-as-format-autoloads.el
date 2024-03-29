;;; copy-as-format-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from copy-as-format.el

(autoload 'copy-as-format "copy-as-format" "\
Copy the current line or active region and add it to the kill ring as
GitHub/Slack/JIRA/HipChat/... formatted code.  Format defaults to
`copy-as-format-default'.  The buffer will not be modified.

With a prefix argument prompt for the format." t)
 (autoload 'copy-as-format-asciidoc  "copy-as-format" nil t)
 (autoload 'copy-as-format-bitbucket "copy-as-format" nil t)
 (autoload 'copy-as-format-disqus    "copy-as-format" nil t)
 (autoload 'copy-as-format-github    "copy-as-format" nil t)
 (autoload 'copy-as-format-gitlab    "copy-as-format" nil t)
 (autoload 'copy-as-format-hipchat   "copy-as-format" nil t)
 (autoload 'copy-as-format-html      "copy-as-format" nil t)
 (autoload 'copy-as-format-jira      "copy-as-format" nil t)
 (autoload 'copy-as-format-markdown  "copy-as-format" nil t)
 (autoload 'copy-as-format-mediawiki "copy-as-format" nil t)
 (autoload 'copy-as-format-org-mode  "copy-as-format" nil t)
 (autoload 'copy-as-format-pod       "copy-as-format" nil t)
 (autoload 'copy-as-format-rst       "copy-as-format" nil t)
 (autoload 'copy-as-format-slack     "copy-as-format" nil t)
 (autoload 'copy-as-format-telegram  "copy-as-format" nil t)
 (autoload 'copy-as-format-whatsapp  "copy-as-format" nil t)
(register-definition-prefixes "copy-as-format" '("copy-as-format-"))

;;; End of scraped data

(provide 'copy-as-format-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; copy-as-format-autoloads.el ends here
