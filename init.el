;;; Package --- Summary

;;; Commentary:
;; Emacs init file responsible for either loading a pre-compiled configuration file
;; or tangling and loading a literate org configuration file.

;;; Code:

(defun kwarks/emacs-config-synced-p ()
  "Returns T if config.el exists and config.org is older that config.el"
  (let* ((org-file (expand-file-name "config.org" user-emacs-directory))
         (el-file (expand-file-name "config.el" user-emacs-directory))
         (org-mod-time (file-attribute-modification-time (file-attributes org-file)))
         (el-mod-time (file-attribute-modification-time (file-attributes el-file))))
    (and (file-exists-p el-file)
         (time-less-p org-mod-time el-mod-time))))

;; Don't attempt to find/apply special file handlers to files loaded during startup.
(let ((file-name-handler-alist nil))
  (if (kwarks/emacs-config-synced-p)
      (load-file (expand-file-name "config.el" user-emacs-directory)))

  (message "compiling config.org...")
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

;; load private extensions from lisp/private folder
(let ((private (expand-file-name "lisp/private" user-emacs-directory)))
  (if (file-directory-p private)
      (progn
        (message "Loading private extensions...")
        (mapc 'load (file-expand-wildcards (concat private "/*.el"))))))

;;; init.el ends here
(put 'downcase-region 'disabled nil)
