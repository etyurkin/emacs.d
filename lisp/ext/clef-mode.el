;;; clef-mode.el --- Major mode for Clef (.clef) sources  -*- lexical-binding: t; -*-

;; Clef live evaluation uses nREPL (CIDER).

(require 'lisp-mode)

;; Emacs 30 defines `common-lisp-mode' as an alias of `lisp-mode'.  Derive
;; from `lisp-mode' directly so Common Lisp font-lock and indentation apply.

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

(declare-function cider-connect-clj "cider" (&optional params))
(declare-function cider-eval-defun-at-point "cider-eval" ())
(declare-function cider-eval-last-sexp "cider-eval" (&optional eob-p))
(declare-function cider-eval-region "cider-eval" (&optional start end))
(declare-function cider-switch-to-repl-buffer "cider-repl" (&optional show))
(declare-function cider-mode "cider-mode" (&optional arg))
(declare-function sly-mode "sly" (&optional arg))
(declare-function sly-editing-mode "sly" (&optional arg))

(defvar clef-mode-map (make-sparse-keymap)
  "Keymap for `clef-mode'.")

(define-key clef-mode-map (kbd "C-c C-c") #'cider-eval-defun-at-point)
(define-key clef-mode-map (kbd "C-x C-e") #'cider-eval-last-sexp)
(define-key clef-mode-map (kbd "C-c C-e") #'cider-eval-last-sexp)
(define-key clef-mode-map (kbd "C-c C-r") #'cider-eval-region)
(define-key clef-mode-map (kbd "C-c C-k") #'clef-load-buffer)
(define-key clef-mode-map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
(define-key clef-mode-map (kbd "C-c C-v") #'clef-connect)

(defconst clef-font-lock-keywords--shebang
  '(("\\`#![^\n]*" 0 font-lock-comment-face))
  "Font-lock rule for a leading Unix shebang line.")

(defun clef-mode--disable-sly ()
  "Turn off SLY in Clef buffers; clef-mode uses CIDER/nREPL."
  (when (fboundp 'sly-editing-mode)
    (sly-editing-mode -1))
  (when (fboundp 'sly-mode)
    (sly-mode -1)))

(defun clef-mode--disable-sly-on-lisp-hook ()
  "Run from `lisp-mode-hook' after SLY; keep SLY out of `clef-mode' buffers."
  (when (eq major-mode 'clef-mode)
    (clef-mode--disable-sly)))

(defun clef-mode--setup-font-lock ()
  "Apply Common Lisp font-lock plus Clef shebang highlighting."
  (unless font-lock-defaults
    (lisp-mode-variables nil t nil))
  (font-lock-add-keywords nil clef-font-lock-keywords--shebang)
  (when font-lock-mode
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (font-lock-fontify-buffer))))

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
(define-derived-mode clef-mode lisp-mode "Clef"
  "Major mode for editing Clef (.clef) programs.
Live evaluation uses nREPL — connect with \\[clef-connect]."
  :group 'clef
  (setq-local indent-tabs-mode nil)
  (clef-mode--disable-sly)
  (clef-mode--setup-font-lock)
  (when (fboundp 'cider-mode)
    (cider-mode 1)))

(add-hook 'lisp-mode-hook #'clef-mode--disable-sly-on-lisp-hook 100)

(defun clef--nrepl-port-file ()
  "Return path to `.nrepl-port' in a dominating directory, or nil."
  (and default-directory
       (locate-dominating-file default-directory ".nrepl-port")))

(defun clef--read-nrepl-port-from-file ()
  "Return port number from `.nrepl-port', or nil when the file is absent."
  (when-let ((root (clef--nrepl-port-file)))
    (string-to-number
     (string-trim
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".nrepl-port" root))
        (buffer-string))))))

(defun clef--read-nrepl-port ()
  "Return nREPL port from `.nrepl-port' in a dominating directory, else `clef-nrepl-port'."
  (or (clef--read-nrepl-port-from-file) clef-nrepl-port))

(defun clef--suggested-nrepl-port ()
  "Return a port string for CIDER connect prompts, or nil."
  (when-let ((port (or (clef--read-nrepl-port-from-file)
                       (and (derived-mode-p 'clef-mode) clef-nrepl-port))))
    (number-to-string port)))

(defun clef--completing-read-port-advice (orig host ports)
  "Inject Clef `.nrepl-port' into CIDER's port prompt default."
  (if-let ((port (clef--suggested-nrepl-port)))
      (let ((ports (cons (list "clef" port) ports)))
        (funcall orig host ports))
    (funcall orig host ports)))

(defun clef--infer-ports-advice (orig &rest args)
  "Inject Clef `.nrepl-port' into CIDER port discovery (skip lsof check)."
  (let ((ports (apply orig args)))
    (if-let* ((root (clef--nrepl-port-file))
              (port (clef--read-nrepl-port-from-file)))
        (let ((entry (list (file-name-nondirectory (directory-file-name root))
                           (number-to-string port))))
          (if (seq-find (lambda (p) (equal (cadr p) (cadr entry))) ports)
              ports
            (cons entry ports)))
      ports)))

(defun clef--install-cider-port-advice ()
  "Ensure CIDER connect prompts suggest Clef nREPL ports."
  (unless (get 'cider--completing-read-port 'clef-port-advice)
    (put 'cider--completing-read-port 'clef-port-advice t)
    (advice-add #'cider--completing-read-port :around #'clef--completing-read-port-advice)
    (advice-add #'cider--infer-ports :around #'clef--infer-ports-advice)))

(with-eval-after-load 'cider #'clef--install-cider-port-advice)

;;;###autoload
(defun clef-connect (&optional _prompt)
  "Connect CIDER to a Clef nREPL server.
Prompts for host and port; `.nrepl-port' in a dominating directory is suggested.
With \\[universal-argument], connect immediately without prompting."
  (interactive "P")
  (require 'cider)
  (clef--install-cider-port-advice)
  (if current-prefix-arg
      (cider-connect-clj `(:host ,clef-nrepl-host
                           :port ,(clef--read-nrepl-port)
                           :--context-buffer ,(current-buffer)))
    (let ((host (completing-read "Host: "
                                 (list clef-nrepl-host "localhost")
                                 nil nil nil 'cider-host-history
                                 clef-nrepl-host))
          (port (read-number "Port: " (clef--read-nrepl-port))))
      (cider-connect-clj `(:host ,host :port ,port
                           :--context-buffer ,(current-buffer))))))

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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.clef\\'" . clef-mode))

(provide 'clef-mode)
;;; clef-mode.el ends here
