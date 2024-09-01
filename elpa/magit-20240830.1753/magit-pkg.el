(define-package "magit" "20240830.1753" "A Git porcelain inside Emacs"
  '((emacs "26.1")
    (compat "30.0.0.0")
    (dash "2.19.1")
    (git-commit "4.0.0")
    (magit-section "4.0.0")
    (seq "2.24")
    (transient "0.7.4")
    (with-editor "3.4.1"))
  :commit "2996e2f2e9bfb017ee7520d67be22c902eea9c7f" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers
  '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
    ("Kyle Meyer" . "kyle@kyleam.com"))
  :maintainer
  '("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
