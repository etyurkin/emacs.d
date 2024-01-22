;;; Open IntelliJ file in Emacs
;;;
;;; Create new External Tool in IntelliJ with the following parameters:
;;;
;;; Program: emacsclient
;;; Arguments: --eval "(intellij-open-in-emacs \"$FilePath$\" $LineNumber$ $ColumnNumber$)"
;;; Working directory: $FileDir$
;;;
;;; Set a shortcut like CMD-Shift-o

(defun intellij-open-in-emacs (file &optional line col)
  (find-file file)

  (when line
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char (1- col)))

  (when (string-equal system-type "darwin")
    (ns-do-applescript "tell application \"Emacs\" to activate")
    (raise-frame)))
