;;; geiser-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "geiser" "geiser.el" (0 0 0 0))
;;; Generated autoloads from geiser.el

(defconst geiser-elisp-dir (file-name-directory (or load-file-name buffer-file-name)) "\
Directory containing Geiser's Elisp files.")

(mapc (lambda (group) (custom-add-load group (symbol-name group)) (custom-add-load 'geiser (symbol-name group))) '(geiser geiser-edit geiser-repl geiser-autodoc geiser-doc geiser-debug geiser-faces geiser-mode geiser-image geiser-implementation geiser-xref))

;;;***

;;;### (autoloads nil "geiser-autodoc" "geiser-autodoc.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from geiser-autodoc.el

(register-definition-prefixes "geiser-autodoc" '("geiser-autodoc-"))

;;;***

;;;### (autoloads nil "geiser-base" "geiser-base.el" (0 0 0 0))
;;; Generated autoloads from geiser-base.el

(register-definition-prefixes "geiser-base" '("geiser--"))

;;;***

;;;### (autoloads nil "geiser-capf" "geiser-capf.el" (0 0 0 0))
;;; Generated autoloads from geiser-capf.el

(register-definition-prefixes "geiser-capf" '("geiser-capf-"))

;;;***

;;;### (autoloads nil "geiser-compile" "geiser-compile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from geiser-compile.el

(register-definition-prefixes "geiser-compile" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-completion" "geiser-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from geiser-completion.el

(register-definition-prefixes "geiser-completion" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-connection" "geiser-connection.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from geiser-connection.el

(register-definition-prefixes "geiser-connection" '("geiser-con"))

;;;***

;;;### (autoloads nil "geiser-custom" "geiser-custom.el" (0 0 0 0))
;;; Generated autoloads from geiser-custom.el

(register-definition-prefixes "geiser-custom" '("geiser-custom-"))

;;;***

;;;### (autoloads nil "geiser-debug" "geiser-debug.el" (0 0 0 0))
;;; Generated autoloads from geiser-debug.el

(register-definition-prefixes "geiser-debug" '("geiser-debug-"))

;;;***

;;;### (autoloads nil "geiser-doc" "geiser-doc.el" (0 0 0 0))
;;; Generated autoloads from geiser-doc.el

(register-definition-prefixes "geiser-doc" '("geiser-doc"))

;;;***

;;;### (autoloads nil "geiser-edit" "geiser-edit.el" (0 0 0 0))
;;; Generated autoloads from geiser-edit.el

(register-definition-prefixes "geiser-edit" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-eval" "geiser-eval.el" (0 0 0 0))
;;; Generated autoloads from geiser-eval.el

(register-definition-prefixes "geiser-eval" '("geiser-eval-"))

;;;***

;;;### (autoloads nil "geiser-image" "geiser-image.el" (0 0 0 0))
;;; Generated autoloads from geiser-image.el

(register-definition-prefixes "geiser-image" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-impl" "geiser-impl.el" (0 0 0 0))
;;; Generated autoloads from geiser-impl.el
 (defvar geiser-active-implementations nil)
 (defvar geiser-implementations-alist nil)

(defun geiser-activate-implementation (impl) (add-to-list 'geiser-active-implementations impl))

(defun geiser-impl--add-to-alist (kind what impl &optional append) (add-to-list 'geiser-implementations-alist (list (list kind what) impl) append))

(defun geiser-implementation-extension (impl ext) "\
Add to `geiser-implementations-alist' an entry for extension EXT." (geiser-impl--add-to-alist 'regexp (format "\\.%s\\'" ext) impl t))

(register-definition-prefixes "geiser-impl" '("define-geiser-implementation" "geiser-" "with--geiser-implementation"))

;;;***

;;;### (autoloads nil "geiser-log" "geiser-log.el" (0 0 0 0))
;;; Generated autoloads from geiser-log.el

(register-definition-prefixes "geiser-log" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-menu" "geiser-menu.el" (0 0 0 0))
;;; Generated autoloads from geiser-menu.el

(register-definition-prefixes "geiser-menu" '("geiser-menu--"))

;;;***

;;;### (autoloads nil "geiser-mode" "geiser-mode.el" (0 0 0 0))
;;; Generated autoloads from geiser-mode.el

(autoload 'geiser-mode "geiser-mode" "\
Toggle Geiser's mode.

This is a minor mode.  If called interactively, toggle the
`Geiser mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `geiser-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Geiser mode is enabled, a host of nice utilities for
interacting with the Geiser REPL is at your disposal.
\\{geiser-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'turn-on-geiser-mode "geiser-mode" "\
Enable `geiser-mode' (in a Scheme buffer)." t nil)

(autoload 'geiser-mode--maybe-activate "geiser-mode" nil nil nil)

(add-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)

(register-definition-prefixes "geiser-mode" '("geiser-" "turn-off-geiser-mode"))

;;;***

;;;### (autoloads nil "geiser-popup" "geiser-popup.el" (0 0 0 0))
;;; Generated autoloads from geiser-popup.el

(register-definition-prefixes "geiser-popup" '("geiser-popup-"))

;;;***

;;;### (autoloads nil "geiser-reload" "geiser-reload.el" (0 0 0 0))
;;; Generated autoloads from geiser-reload.el

(autoload 'geiser-unload "geiser-reload" "\
Unload all Geiser modules." t nil)

(register-definition-prefixes "geiser-reload" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-repl" "geiser-repl.el" (0 0 0 0))
;;; Generated autoloads from geiser-repl.el

(define-obsolete-function-alias 'run-geiser 'geiser "Geiser 0.26")

(autoload 'geiser "geiser-repl" "\
Start a new Geiser REPL.

\(fn IMPL)" t nil)

(autoload 'geiser-connect "geiser-repl" "\
Start a new Geiser REPL connected to a remote Scheme process.

\(fn IMPL &optional HOST PORT)" t nil)

(autoload 'geiser-connect-local "geiser-repl" "\
Start a new Geiser REPL connected to a remote Scheme process
over a Unix-domain socket.

\(fn IMPL SOCKET)" t nil)

(autoload 'geiser-repl-switch "geiser-repl" "\
Switch to running Geiser REPL.

If REPL is the current buffer, switch to the previously used
scheme buffer.

With prefix argument, ask for which one if more than one is running.
If no REPL is running, execute `geiser' to start a fresh one.

\(fn &optional ASK IMPL BUFFER)" t nil)

(register-definition-prefixes "geiser-repl" '("geiser-"))

;;;***

;;;### (autoloads nil "geiser-syntax" "geiser-syntax.el" (0 0 0 0))
;;; Generated autoloads from geiser-syntax.el

(register-definition-prefixes "geiser-syntax" '("geiser-syntax--"))

;;;***

;;;### (autoloads nil "geiser-table" "geiser-table.el" (0 0 0 0))
;;; Generated autoloads from geiser-table.el

(register-definition-prefixes "geiser-table" '("geiser-table-"))

;;;***

;;;### (autoloads nil "geiser-xref" "geiser-xref.el" (0 0 0 0))
;;; Generated autoloads from geiser-xref.el

(register-definition-prefixes "geiser-xref" '("geiser-xref"))

;;;***

;;;### (autoloads nil nil ("geiser-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; geiser-autoloads.el ends here
