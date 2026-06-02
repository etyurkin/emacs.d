;;; init.el --- Component-based literate config loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Reads the component manifest (components.org), tangles config.org plus every
;; enabled component file under components/ into a single config.el, byte
;; compiles it and loads the result.  Toggle components by editing the
;; checkboxes in components.org and restarting Emacs.

;;; Code:

(defun kwarks/enabled-components ()
  "Return the list of component file paths checked off in components.org.
Each entry is a relative path like \"components/foo.org\"."
  (let ((file (expand-file-name "components.org" user-emacs-directory))
        results)
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward
                "^[ \t]*-[ \t]+\\[[Xx]\\][ \t]+\\[\\[file:\\([^]]+\\)\\]\\[[^]]*\\]\\]"
                nil t)
          (push (match-string 1) results))))
    (nreverse results)))

(defun kwarks/config-source-files ()
  "Return the ordered list of org files that make up the configuration."
  (let ((dir user-emacs-directory))
    (cons (expand-file-name "config.org" dir)
          (delq nil
                (mapcar (lambda (rel)
                          (let ((f (expand-file-name rel dir)))
                            (and (file-exists-p f) f)))
                        (kwarks/enabled-components))))))

(defun kwarks/config-synced-p ()
  "Return non-nil when config.el is newer than every input that feeds it."
  (let* ((dir user-emacs-directory)
         (el-file (expand-file-name "config.el" dir))
         (inputs (append (list (expand-file-name "components.org" dir)
                               (expand-file-name "init.el" dir))
                         (kwarks/config-source-files))))
    (and (file-exists-p el-file)
         (let ((el-time (file-attribute-modification-time
                         (file-attributes el-file))))
           (seq-every-p
            (lambda (input)
              (or (not (file-exists-p input))
                  (time-less-p (file-attribute-modification-time
                                (file-attributes input))
                               el-time)))
            inputs)))))

(defun kwarks/tangle-config ()
  "Tangle config.org and enabled components into a single config.el.
Each source file is tangled to its own derived .el (because blocks use
`:tangle yes'), the results are concatenated into config.el and the
intermediate files are removed."
  (require 'org)
  (let* ((dir user-emacs-directory)
         (out (expand-file-name "config.el" dir))
         (files (kwarks/config-source-files)))
    (with-temp-file out
      (insert ";;; config.el --- generated, do not edit -*- lexical-binding: t; -*-\n")
      (insert ";; Generated from config.org + components.org. Edit those instead.\n")
      (dolist (f files)
        (insert (format "\n;; ==== %s ====\n" (file-name-nondirectory f)))
        (goto-char (point-max))
        (dolist (tangled (org-babel-tangle-file f))
          (insert-file-contents tangled)
          (goto-char (point-max))
          (delete-file tangled)))
      (insert "\n(provide 'config)\n;;; config.el ends here\n"))
    (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local
                                      obsolete docstrings lambda interactive-only))
    (byte-compile-file out)
    out))

;; Don't attempt to find/apply special file handlers to files loaded at startup.
(let ((file-name-handler-alist nil))
  ;; Ensure installed package autoloads are available even when Emacs did not
  ;; auto-activate them (e.g. batch mode or a future early-init.el).  Idempotent.
  (when (fboundp 'package-activate-all)
    (package-activate-all))
  (unless (kwarks/config-synced-p)
    (message "Regenerating config.el from enabled components...")
    (kwarks/tangle-config))
  (load (expand-file-name "config" user-emacs-directory) nil nil nil))

;;; init.el ends here
(put 'downcase-region 'disabled nil)
