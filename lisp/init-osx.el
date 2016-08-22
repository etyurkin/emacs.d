(when *is-a-mac*
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
    (start-process "say" nil "say" phrase))

  (defun osx-notify (title message)
    "Show standard OSX notification with TITLE and MESSAGE."
    (start-process "notify" nil
                   "/usr/bin/osascript" "-e"
                   (format "display notification \"%s\" with title \"%s\"" message title)))

  (use-package osx-lib :ensure t)

  (defun ns-raise-emacs ()
    "Raise Emacs."
    (osx-lib-run-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided FRAME."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
  (when (display-graphic-p)
    (ns-raise-emacs))
  )

(provide 'init-osx)
;;; init-osx.el ends here
