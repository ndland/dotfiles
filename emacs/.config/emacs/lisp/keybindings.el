(use-package general
  :straight t
  :config
  ;; Create definer for leader keys
  (general-create-definer my-leader-def
    :prefix "SPC")

  (general-create-definer my-local-leader-def
    :prefix "SPC l")

  ;; Global Keybindings
  (my-leader-def
    :states 'normal
    "b" '(:ignore t :which-key "Buffers")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bk" '(kill-this-buffer :which-key "Kill buffer")
    "be" '(embark-act :which-key "Embark act")

    "f" '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")
    "fr" '(consult-recent-file :which-key "Recent files")

    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Git status")

    "l" '(:ignore t :which-key "Local leader")
    "lb" '(embark-bindings :which-key "List buffer keybindings")

    "n" '(:ignore t :which-key "Notes")
    "nf" '(org-roam-node-find :which-key "Find roam node")
    "nd" '(org-roam-dailies-goto-today :which-key "Go to today")
    "nr" '(org-roam-node-random :which-key "Open random note")

    "p" '(:ignore t :which-key "Projects")
    "pa" '(projectile-add-known-project :which-key "Add known project")
    "pf" '(projectile-find-file :which-key "Find file")
    "ps" '(projectile-switch-project :which-key "Switch projects")

    "s" '(:ignore t :which-key "Search")
    "sb" '(consult-line :which-key "Search current buffer")
    "sf" '(consult-line-multi :which-key "Search buffer files")
    "sg" '(consult-grep :which-key "Grep files")
    "sr" '(consult-ripgrep :which-key "Ripgrep files")

    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Org agenda")
    "oc" '(org-capture :which-key "Org Capture")
    "ot" '(:ignore t :which-key "Org timer")
    "ots" '(org-timer-set-timer :which-key "Set timer")
    "otp" '(org-timer-pause-or-continue :which-key "Pause or continue timer")
    "ott" '(org-timer-stop :which-key "Stop timer"))

  ;; Org Mode bindings
  (my-local-leader-def
    :states 'normal
    :keymaps 'org-mode-map
    "f" '(org-refile :which-key "Org refile")
    "p" '(org-priority :which-key "Org priority")
    "r" '(:ignore t :which-key "Roam")
    "rf" '(org-roam-node-find :which-key "Find node")
    "ri" '(org-roam-node-insert :which-key "Insert node")
    "t" '(org-set-tags-command :which-key "Set tags"))

  ;; Org Agenda bindings
  (general-define-key
   :keymaps 'org-agenda-mode-map
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line))
