;;; native-complete-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "native-complete" "native-complete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from native-complete.el

(autoload 'native-complete-setup-bash "native-complete" "\
Setup support for native-complete enabled bash shells.
This involves not sending the `--noediting' argument as well as
setting `TERM' to a value other then dumb." t nil)

(autoload 'native-complete-at-point "native-complete" "\
Get the candidates from the underlying shell.
This should behave the same as sending TAB in an terminal
emulator." nil nil)

(autoload 'native-complete-check-config "native-complete" nil t nil)

(register-definition-prefixes "native-complete" '("native-complete-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; native-complete-autoloads.el ends here
