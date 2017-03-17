;;; init-dash --- Support for the http://kapeli.com/dash documentation browser
;;;
;;; Commentary:
;;;
;;; see https://github.com/areina/helm-dash
;;;
;;; Code:

(use-package helm-dash)

(setq helm-dash-browser-func 'eww)
(global-set-key (kbd "C-c h") 'helm-dash-at-point)

(defun helm-dash--use-docset (docset)
  "Install DOCSET if it's not installed yet."
  (unless (member docset (helm-dash-installed-docsets))
    (helm-dash-install-docset docset)))

(helm-dash--use-docset "Common_Lisp")
(add-hook 'lisp-mode-hook (lambda ()
                            (setq-local helm-dash-docsets '("Common_Lisp"))))

(provide 'init-dash)

;;; init-dash.el ends here
