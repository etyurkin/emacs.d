;;; init-functions --- Summary

;;; Commentary:

;;; Code:

(defun insert-divider ()
  "Insert horizontal line, i.e. FORM FEED (FF)."
  (interactive)
  (insert #x0C))

(defun trim (str)
  "Remove leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun empty-string-p (string)
  "Return true if the STRING is empty or nil.  Expects string."
  (or (null string)
      (zerop (length (trim string)))))

(defun run-shell-command (cmd)
  (let ((shell-process (get-buffer-process "*shell*")))
    (cond ((not (null shell-process))
           (process-send-string "shell" (concat cmd "\n")))
          (t (message "Shell process is not running")))))

(defun set-proxy (proxy-host)
  (setq url-proxy-services
        `(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . ,proxy-host)
          ("https" . ,proxy-host)))
  (setenv "http_proxy" (concat "http://" proxy-host))
  (setenv "https_proxy" (concat "http://" proxy-host))
  (setenv "HTTP_PROXY" (concat "http://" proxy-host))
  (setenv "HTTPS_PROXY" (concat "http://" proxy-host))
  (run-shell-command (concat "export http_proxy=http://" proxy-host))
  (run-shell-command (concat "export https_proxy=http://" proxy-host))
  (run-shell-command (concat "export HTTP_PROXY=http://" proxy-host))
  (run-shell-command (concat "export HTTPS_PROXY=http://" proxy-host)))

(defun proxy-on ()
  "Set proxy."
  (interactive)
  (let ((proxy-host
         (read-string "proxy host: "
                      (if (bound-and-true-p *proxy-host*) *proxy-host* ""))))
    (unless (empty-string-p proxy-host)
      (set-proxy proxy-host)
      (message "proxy is set to %s" proxy-host))))

(defun proxy-off ()
  "Unset proxy."
  (interactive)
  (setq url-proxy-services ())
  (setenv "http_proxy" "")
  (setenv "https_proxy" "")
  (setenv "HTTP_PROXY" "")
  (setenv "HTTPS_PROXY" "")
  (run-shell-command "unset http_proxy")
  (run-shell-command "unset https_proxy")
  (run-shell-command "unset HTTP_PROXY")
  (run-shell-command "unset HTTPS_PROXY")
  (message "proxy is off"))

(defun init-ssh-agent ()
  "Add personal rsa key to ssh agent."
  (interactive)
  (run-shell-command "eval \"$(ssh-agent -s)\"")
  (run-shell-command "ssh-add ~/.ssh/personal_rsa"))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an Emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved Emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

(defun clear-shell ()
  "Cleans shell buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; to use - select xml region, M-x xml-pretty-print-region
(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region from BEGIN to END.
You need to have 'nxml-mode'
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do this.
The function inserts linebreaks to separate tags that have nothing
but whitespace between them.
It then indents the markup by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun sudo-edit (&optional arg)
  "Reopens current buffer or ARG as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; hash-pass http://hashapass.com/
(defvar hash-pass-password-wait-time "30 sec"
  "The default period to wait before erasing the password from the clipboard.
Must be compatible with `run-at-time'.")

(defvar hash-pass-system-clipboard-only nil
  "If T hash-pass will not put generated password to Emacs kill ring.")

(defun string->clipboard (string)
  "Copy STRING to system clipboard."
  (if hash-pass-system-clipboard-only
      (funcall interprogram-cut-function string)
    (kill-new string)))

(defun hash-pass ()
  "Generate strong password based on parameter and master password."
  (interactive)
  (let ((param (read-string "parameter: "))
        (password (read-passwd "master password: ")))
    (string->clipboard (trim
                        (shell-command-to-string
                         (format "echo -n %s | openssl dgst -sha1 -binary -hmac %s | openssl enc -base64 | cut -c 1-8" param password))))

    (run-at-time hash-pass-password-wait-time nil (lambda () (string->clipboard "")))
    (message "Generated hash has been copied to clipboard")))


(defun client-save-kill-emacs(&optional display)
  "This is a function that can bu used to shutdown save buffers and
shutdown the emacs daemon. It should be called using
emacsclient -e '(client-save-kill-emacs)'.  This function will
check to see if there are any modified buffers or active clients
or frame.  If so an x window will be opened and the user will
be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

                                        ; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
					(> (length (frame-list)) 1)
                                        ))

                                        ; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      ;; (when (not (eq window-system 'x))
      ;;   (message "Initializing x windows system.")
      ;;   (x-initialize-window-system))
      ;; (when (not display) (setq display (getenv "DISPLAY")))
      ;; (message "Opening frame on display: %s" display)
      ;; (select-frame (make-frame-on-display display '((window-system . x)))))
      (select-frame (make-frame '((window-system . ns)))))
                                        ; Save the current frame.
    (setq new-frame (selected-frame))


                                        ; When displaying the number of clients and frames:
                                        ; subtract 1 from the clients for this client.
                                        ; subtract 2 from the frames this frame (that we just created) and the default frame.
    (when ( or (not active-clients-or-frames)
	       (yes-or-no-p
                (format "There are currently %d clients and %d frames. Exit anyway?"
                        (- (length server-clients) 1)
                        (- (length (frame-list)) 2))))

                                        ; If the user quits during the save dialog then don't exit emacs.
                                        ; Still close the terminal though.
      (let((inhibit-quit t))
                                        ; Save buffers
	(with-local-quit
	  (save-some-buffers))

	(if quit-flag
            (setq quit-flag nil)
                                        ; Kill all remaining clients
	  (progn
	    (dolist (client server-clients)
	      (server-delete-client client))
                                        ; Exit emacs
	    (kill-emacs)))
	))

                                        ; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )


(defun modified-buffers-exist()
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
		 (buffer-modified-p buffer)
		 (not (buffer-base-buffer buffer))
		 (or
		  (buffer-file-name buffer)
		  (progn
		    (set-buffer buffer)
		    (and buffer-offer-save (> (buffer-size) 0))))
		 )
	(setq modified-found t)
	)
      )
    modified-found
    )
  )


;; -----------------------------------------------------------------------------------------------------------
;; port of common lisp (format nil "~r" 1234) and (format nil "~:r" 1234) functions:
;; (number-to-english-cardinal 1234) ==> "one thousand two hundred thirty-four"
;; (number-to-english-ordinal 1234) ==> "one thousand two hundred thirty-fourth"
;;
;; see https://github.com/sbcl/sbcl/blob/e95100470561cfda5a2f8efd70274509d340686a/src/code/target-format.lisp
;; -----------------------------------------------------------------------------------------------------------

(defvar *cardinal-ones*
  (vector nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defvar *cardinal-tens*
  (vector nil nil "twenty" "thirty" "forty"
          "fifty" "sixty" "seventy" "eighty" "ninety"))

(defvar *cardinal-teens*
  (vector "ten" "eleven" "twelve" "thirteen" "fourteen"
          "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defvar *cardinal-periods*
  (vector "" " thousand" " million" " billion" " trillion" " quadrillion"
          " quintillion" " sextillion" " septillion" " octillion" " nonillion"
          " decillion" " undecillion" " duodecillion" " tredecillion"
          " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
          " octodecillion" " novemdecillion" " vigintillion"))

(defvar *ordinal-ones*
  (vector nil "first" "second" "third" "fourth"
          "fifth" "sixth" "seventh" "eighth" "ninth"))

(defvar *ordinal-tens*
  (vector nil "tenth" "twentieth" "thirtieth" "fortieth"
          "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

(defun number-to-english-small-cardinal (n)
  (let ((result "")
        (hundreds (truncate n 100))
        (rem (% n 100)))
    (when (plusp hundreds)
      (setq result (concat result
                           (elt *cardinal-ones* hundreds)
                           " hundred"))
      (when (plusp rem)
        (setq result (concat result " "))))
    (when (plusp rem)
      (let ((tens (truncate rem 10))
            (ones (% rem 10)))
        (cond ((< 1 tens)
               (setq result (concat result (elt *cardinal-tens* tens)))
               (when (plusp ones)
                 (setq result (concat result
                                      "-"
                                      (elt *cardinal-ones* ones)))))
              ((= tens 1)
               (setq result (concat result (elt *cardinal-teens* ones))))
              ((plusp ones)
               (setq result (concat result (elt *cardinal-ones* ones)))))))
    result))

;; (number-to-english-small-cardinal 100)

(defun number-to-english-cardinal-aux (n period err)
  (let ((result "")
        (beyond (truncate n 1000))
        (here (% n 1000)))
    (unless (<= period 21)
      (error "Number too large to print in English: %d" err))
    (unless (zerop beyond)
      (setq result (concat result
                           (number-to-english-cardinal-aux beyond (1+ period) err))))
    (unless (zerop here)
      (unless (zerop beyond)
        (setq result (concat result " ")))
      (setq result (concat result
                           (number-to-english-small-cardinal here)
                           (elt *cardinal-periods* period))))
    result))

;; (number-to-english-cardinal-aux 1000 0 1000)

(defun number-to-english-cardinal (n)
  "Return number N as a human readable english cardinal number."
  (interactive "nNumber? ")
  (let ((result ""))
    (cond ((minusp n)
           (setq result (concat result
                                "negative "
                                (number-to-english-cardinal-aux (- n) 0 n))))
          ((zerop n)
           (setq result "zero"))
          (t
           (setq result (number-to-english-cardinal-aux n 0 n))))
    (when (called-interactively-p 'any)
      (print result))
    result))

;; (number-to-english-cardinal 10000432320000123)

(defun number-to-english-ordinal (n)
  "Return number N as a human readable english ordinal number."
  (interactive "nNumber? ")
  (let ((result ""))
    (when (minusp n)
      (setq result "negative "))
    (let ((number (abs n)))
      (let ((top (truncate number 100))
            (bot (% number 100)))
        (unless (zerop top)
          (setq result (concat result (number-to-english-cardinal (- number bot)))))
        (when (and (plusp top) (plusp bot))
          (setq result (concat result " ")))
        (let ((tens (truncate bot 10))
              (ones (% bot 10)))
          (cond ((= bot 12)
                 (setq result (concat result "twelfth")))
                ((= tens 1)
                 (setq result (concat result
                                      (elt *cardinal-teens* ones)
                                      "th")))
                ((and (zerop tens) (plusp ones))
                 (setq result (concat result (elt *ordinal-ones* ones))))
                ((and (zerop ones)(plusp tens))
                 (setq result (concat result (elt *ordinal-tens* tens))))
                ((plusp bot)
                 (setq result (concat result
                                      (elt *cardinal-tens* tens)
                                      "-"
                                      (elt *ordinal-ones* ones))))
                ((plusp number)
                 (setq result (concat result "th")))
                (t
                 (setq result (concat result "zeroth")))))))
    (when (called-interactively-p 'any)
      (print result))
    result))

;; (osx-say (number-to-english-ordinal 12345))
;; (number-to-english-ordinal 12345)

(provide 'init-functions)
;;; init-functions ends here
