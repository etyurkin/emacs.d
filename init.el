;;; init.el --- Component-based literate config loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Reads the component manifest (components.org), tangles config.org plus every
;; enabled component file under components/ into a single config.el, byte
;; compiles it and loads the result.  Third-party packages are installed by
;; Elpaca (see installer below), not package.el.
;;
;; If Emacs misbehaves after config edits, stale generated files may be the
;; cause — run `make clean' and restart; out-of-sync sources also drop
;; `config.el'/`config.elc' before retangle on startup.

;;; Code:

;; Elpaca installer — must run before any other Elpaca macro (see
;; https://github.com/progfolio/elpaca).
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment if your OS cannot create symlinks (see Elpaca README):
;; (elpaca-no-symlink-mode)

(elpaca elpaca-use-package
  ;; Route `use-package' :ensure through Elpaca.
  (elpaca-use-package-mode))
;; Install `elpaca-use-package' before tangling/byte-compiling `config.el' so
;; `use-package' expands :ensure via Elpaca (see Elpaca wiki / Migrating).
(elpaca-wait)

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
  "Return non-nil only when config.el is up to date with all tangling inputs.
If config.org, init.el, components.org, or any enabled component .org is newer
than config.el, return nil so we tangle again.  After load,
`kwarks/byte-compile-config-if-needed' rebuilds config.elc whenever config.el
is newer than config.elc."
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
      ;; `no-native-compile' is critical: async native compilation re-expands
      ;; `use-package' in an isolated subprocess that lacks
      ;; `elpaca-use-package-mode', producing a config-*.eln that `require's
      ;; packages directly instead of queuing Elpaca orders.  Emacs then prefers
      ;; that broken .eln over the (correct) in-session byte-compiled .elc, so
      ;; nothing installs and theme/completion silently fail to load.
      (insert ";;; config.el --- generated, do not edit -*- lexical-binding: t; no-native-compile: t; -*-\n")
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

(defun kwarks/delete-stale-config-native-compile ()
  "Remove `config-*.eln' under `eln-cache/' so startup cannot load outdated Natcomp."
  (let ((cache (expand-file-name "eln-cache" user-emacs-directory)))
    (when (file-directory-p cache)
      (dolist (subdir (directory-files cache t "^[0-9]" t))
        (when (file-directory-p subdir)
          (dolist (eln (directory-files subdir t "^config-.*\\.eln\\'" t))
            (message "Removing stale native-compiled %s" (file-name-nondirectory eln))
            (delete-file eln)))))))

(defun kwarks/delete-config-el-and-elc ()
  "Delete generated `config.el' and `config.elc' if they exist."
  (let ((el (expand-file-name "config.el" user-emacs-directory))
        (elc (expand-file-name "config.elc" user-emacs-directory)))
    (when (file-exists-p el)
      (message "Deleting %s" el)
      (delete-file el))
    (when (file-exists-p elc)
      (message "Deleting %s" elc)
      (delete-file elc))))

(defun kwarks/byte-compile-config-if-needed ()
  "Ensure config.elc matches config.el when the latter is newer.
Stale bytecode otherwise wins on `load' and you keep old definitions even
after regenerating `config.el' (e.g. tangle without compile, or compile error).
Before recompiling, remove any existing `config.elc' so a failed compile cannot
leave stale bytecode, and remove matching `config-*.eln' in `eln-cache/' so
native code cannot mask fixes in `config.el'."
  (let ((el (expand-file-name "config.el" user-emacs-directory))
        (elc (expand-file-name "config.elc" user-emacs-directory)))
    (when (and (file-exists-p el)
               (or (not (file-exists-p elc))
                   (file-newer-than-file-p el elc)))
      (when (file-exists-p elc)
        (message "config.el newer than config.elc; removing stale config.elc...")
        (delete-file elc))
      (setq byte-compile-warnings
            '(not free-vars unresolved noruntime lexical make-local
                  obsolete docstrings lambda interactive-only))
      (kwarks/delete-stale-config-native-compile)
      (byte-compile-file el))))

;; Don't attempt to find/apply special file handlers to files loaded at startup.
(let ((file-name-handler-alist nil))
  ;; Tangle when config.org (or init.el / any component org) is newer than config.el;
  ;; `kwarks/byte-compile-config-if-needed' then refreshes config.elc if needed.
  (unless (kwarks/config-synced-p)
    (message "Regenerating config.el from enabled components...")
    (kwarks/delete-config-el-and-elc)
    (kwarks/tangle-config))
  (kwarks/byte-compile-config-if-needed)
  (load (expand-file-name "config" user-emacs-directory) nil nil nil))

;; Process every order queued while loading `config.el(c)' so packages exist
;; before their `use-package' :init blocks run (Elpaca is async by default).
(elpaca-wait)

;;; init.el ends here
(put 'downcase-region 'disabled nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; End:
