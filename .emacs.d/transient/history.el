((magit-commit nil
	       ("--reuse-message=ORIG_HEAD"))
 (magit-diff
  ("--no-ext-diff" "--stat"))
 (magit-dispatch nil)
<<<<<<< Updated upstream
 (magit-lfs nil)
 (magit-lfs-install nil)
 (magit-lfs-push nil)
 (magit-log
  ("-n256" "--graph" "--decorate"))
 (magit-pull nil)
<<<<<<< Updated upstream
=======
 (magit-pull
  ("--ff-only")
  ("--rebase"))
>>>>>>> Stashed changes
 (magit-push nil))
=======
 (magit-push nil)
 (magit-rebase
  ("--autostash"))
 (magit-revision-history "ORIG_HEAD"))
>>>>>>> Stashed changes
