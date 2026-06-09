;;; clef-mode.el --- Major mode for Clef (.clef) sources  -*- lexical-binding: t; -*-

;; Clef live evaluation uses nREPL (CIDER).
;; Loaded via kwarks/autoload-ext in config.org; configured in components/clef.org.

(require 'lisp-mode)

;; Stock Emacs defines `common-lisp-mode' as part of `lisp-mode'.  If you use
;; a richer CL mode (e.g. common-lisp-el), load it before `clef-mode'.

(defgroup clef nil
  "Major mode for Clef (.clef) source files."
  :group 'languages)

(defcustom clef-nrepl-host "localhost"
  "Default host for `clef-connect'."
  :type 'string
  :group 'clef)

(defcustom clef-nrepl-port 4005
  "Default port for `clef-connect' when .nrepl-port is absent."
  :type 'integer
  :group 'clef)

(defvar clef-mode-map (make-sparse-keymap)
  "Keymap for `clef-mode'.")

(defconst clef-font-lock-keywords--shebang
  (list
   (rx buffer-start
       "#!"
       (0+ not-newline)
       eol)
   '(0 font-lock-comment-face))
  "Font-lock rule for a leading Unix shebang line.")

(defun clef--strip-shebang (content)
  "Remove a leading Unix shebang line from CONTENT, if present."
  (if (string-prefix-p "#!" content)
      (if-let ((pos (and (string-match "\n" content)
                         (1+ (match-beginning 0)))))
          (substring content pos)
        "")
    content))

(defun clef--buffer-string-without-shebang (&optional buffer)
  "Return BUFFER contents with an initial shebang line removed."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (clef--strip-shebang (substring-no-properties (buffer-string))))))

;;;###autoload
(define-derived-mode clef-mode common-lisp-mode "Clef"
  "Major mode for editing Clef (.clef) programs.
Live evaluation uses nREPL — connect with \\[clef-connect]."
  :group 'clef
  (setq-local indent-tabs-mode nil)
  (font-lock-add-keywords nil clef-font-lock-keywords--shebang)
  (when (bound-and-true-p sly-mode)
    (sly-mode -1))
  (when (fboundp 'cider-mode)
    (cider-mode 1))
  (define-key clef-mode-map (kbd "C-c C-k") #'clef-load-buffer))

(defun clef--read-nrepl-port ()
  "Return nREPL port from `.nrepl-port' in a dominating directory, else `clef-nrepl-port'."
  (let ((file (and default-directory
                   (locate-dominating-file default-directory ".nrepl-port"))))
    (if file
        (string-to-number
         (string-trim
          (with-temp-buffer
            (insert-file-contents (expand-file-name ".nrepl-port" file))
            (buffer-string))))
      clef-nrepl-port)))

;;;###autoload
(defun clef-connect (&optional port)
  "Connect CIDER to a Clef nREPL server on PORT (default: auto-discover)."
  (interactive (list (when current-prefix-arg
                       (read-number "nREPL port: " (clef--read-nrepl-port)))))
  (require 'cider)
  (let ((port (or port (clef--read-nrepl-port))))
    (cider-connect-clj clef-nrepl-host port)))

;;;###autoload
(defun clef-load-buffer (&optional buffer)
  "Load BUFFER in nREPL, omitting a leading `#!/…' shebang line."
  (interactive)
  (require 'cider)
  (setq buffer (or buffer (current-buffer)))
  (let ((orig-default-directory default-directory))
    (with-current-buffer buffer
      (check-parens)
      (let ((default-directory orig-default-directory))
        (unless buffer-file-name
          (user-error "Buffer `%s' is not associated with a file" (current-buffer)))
        (when (and cider-save-file-on-load
                   (buffer-modified-p)
                   (or (eq cider-save-file-on-load t)
                       (y-or-n-p (format "Save file %s? " buffer-file-name))))
          (save-buffer))
        (remove-overlays nil nil 'cider-temporary t)
        (when (fboundp 'cider--clear-compilation-highlights)
          (cider--clear-compilation-highlights))
        (when (fboundp 'cider--quit-error-window)
          (cider--quit-error-window))
        (let ((filename (buffer-file-name buffer))
              (ns-form (when (fboundp 'cider-ns-form) (cider-ns-form))))
          (cider-map-repls :auto
                           (lambda (repl)
                             (when ns-form
                               (cider-repl--cache-ns-form ns-form repl))
                             (cider-request:load-file
                              (clef--buffer-string-without-shebang buffer)
                              (funcall cider-to-nrepl-filename-function
                                       (cider--server-filename filename))
                              (file-name-nondirectory filename)
                              repl)))
          (message "Loading %s..." filename))))))

(defalias 'clef-eval-buffer 'clef-load-buffer
  "Alias for `clef-load-buffer' (matches CIDER's eval-buffer naming).")

(provide 'clef-mode)
;;; clef-mode.el ends here
