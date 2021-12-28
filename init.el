;;; Package --- Summary

;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration file
;; or tangling and loading a literate org configuration file.

;;; Code:

;; Don't attempt to find/apply special file handlers to files loaded during startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "config.elc" user-emacs-directory))
      (load-file (expand-file-name "config.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration

    ;; load core emacs `org-mode', but don't native compile it
    (let ((comp-deferred-compilation nil))
      (require 'org))
    
    ;; on first load we shadow the core emacs org-mode and when trying to quit
    ;; emacs calls the non-existent function `org-clocking-buffer'. Define a dummy
    ;; to allow us to exit cleanly on initial run
    (defun org-clocking-buffer (&rest _))

    (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))))

;;; init.el ends here
