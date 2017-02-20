;;; init-osx -- Adds some useful osx funtions
;;; Commentary:
;;; Code:

(when *is-a-mac*
  (use-package osx-lib :ensure t)

  ;; Reset Option key from CMD back to Option
  ;;(setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq-default default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)

  (after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))

  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h

  (defun osx-lock-screen ()
    "Start screensaver on OSX."
    (interactive)
    (start-process
     "screensaver" nil
     "open" "-a" "/System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources/ScreenSaverEngine.app"))

  (defun osx-say (phrase)
    "Speak PHRASE."
    (interactive "MSay what? ")
    (start-process "say" nil "say" phrase))

  (defun osx-notify (title message)
    "Show standard OSX notification with TITLE and MESSAGE."
    (start-process "notify" nil
                   "/usr/bin/osascript" "-e"
                   (format "display notification \"%s\" with title \"%s\"" message title)))

  ;; brew install mas
  (defun osx-update-check ()
    "Check for OSX updates."
    (interactive)
    (let ((buf "*osx-updates*"))
      (start-process "osx-updates" buf "softwareupdate" "-l")
      (start-process "osx-updates" buf "/usr/local/bin/mas" "outdated")
      (switch-to-buffer-other-window buf)))

  (defun osx-update (password)
    "Update OSX software."
    (interactive (list (read-passwd "Sudo password for updates install: ")))
    (let* ((buf "*osx-updates*")
           (sys-proc (start-process "osx-system-updates" buf "sudo" "softwareupdate" "-ia" "--verbose"))
           (usr-proc (start-process "osx-software-updates" buf "/usr/local/bin/mas" "upgrade")))
      (switch-to-buffer-other-window buf)
      (process-send-string sys-proc (concat password "\r"))
      (process-send-eof sys-proc)))

  (defun ns-raise-emacs ()
    "Raise Emacs."
    (osx-lib-run-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided FRAME."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (when (display-graphic-p)
    (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
    (ns-raise-emacs)
    ))

(provide 'init-osx)
;;; init-osx.el ends here
