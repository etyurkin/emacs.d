((magit-cherry-pick
  ("--ff"))
 (magit-dispatch nil)
 (magit-rebase
  ("--autostash")
  nil)
 (magit-revert
  ("--edit")))
