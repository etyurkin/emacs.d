;;; horoscope-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "horoscope" "horoscope.el" (0 0 0 0))
;;; Generated autoloads from horoscope.el

(autoload 'horoscope "horoscope" "\
Generate a random horoscope.
If called interactively, display the resulting horoscope in a buffer.
If called with a prefix argument or the Lisp argument INSERTP non-nil,
insert the resulting horoscope into the current buffer.

\(fn &optional INSERTP)" t nil)

(autoload 'horoscope-psychoanalyze "horoscope" "\
The astrologist goes to the analyst.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "horoscope" '("horoscope--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; horoscope-autoloads.el ends here
