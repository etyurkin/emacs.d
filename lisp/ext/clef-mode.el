;;; clef-mode.el --- Major mode for Clef (.clef) sources  -*- lexical-binding: t; -*-

;; Clef live evaluation uses nREPL (CIDER), not SLIME/SLY.
;; Loaded via kwarks/autoload-ext in config.org; configured in components/clef.org.

(require 'lisp-mode)

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

;;;###autoload
(define-derived-mode clef-mode lisp-mode "Clef"
  "Major mode for editing Clef (.clef) programs.
Live evaluation uses nREPL — connect with \\[clef-connect]."
  :group 'clef
  (setq-local indent-tabs-mode nil)
  (when (bound-and-true-p sly-mode)
    (sly-mode -1))
  (when (fboundp 'cider-mode)
    (cider-mode 1)))

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

(provide 'clef-mode)
;;; clef-mode.el ends here
