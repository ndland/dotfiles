(use-package general
  :straight t
  :config
    ;; This file contains all your keybindings
    (require 'general)
    ;; * Prefix Keybindings
    ;; :prefix can be used to prevent redundant specification of prefix keys
    ;; again, variables are not necessary and likely not useful if you are only
    ;; using a definer created with `general-create-definer' for the prefixes
    ;; (defconst my-leader "SPC")
    ;; (defconst my-local-leader "SPC m")

    (general-create-definer my-leader-def
    ;; :prefix my-leader
    :prefix "SPC")

    (general-create-definer my-local-leader-def
    ;; :prefix my-local-leader
    :prefix "SPC l"
    "" '(:ignore t :which-key "Local leader"))

    ;; ** Global Keybindings
    (my-leader-def
    :keymaps 'normal
    "b" '(:ignore t :which-key "Buffers")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bk" '(kill-this-buffer :which-key "Kill buffer")

    "f" '(:ignore t :which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fs" '(save-buffer :which-key "Save file")
    "fr" '(consult-recent-file :which-key "Recent files")

    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Git status")

    "n" '(:ignore t :which-key "Notes")
    "nf" '(org-roam-node-find :which-key "Find roam node")
    "nd" '(org-roam-dailies-goto-today :which-key "Go to today")

    "p" '(:ignore t :which-key "Projects")
    "pa" '(projectile-add-known-project :which-key "Add known project")
    "pf" '(projectile-find-file :which-key "Find file")
    "ps" '(projectile-switch-project :which-key "Switch projects")

    "s" '(:ignore t :which-key "Search")
    "sb" '(consult-line :which-key "Search current buffer")
    "sf" '(consult-line-multi :which-key "Search project files")
    "sr" '(consult-ripgrep :which-key "Ripgrep files")

    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Org agenda")
    "ot" '(:ignore t :which-key "org timer")
    "ots" '(org-timer-set-timer :which-key "Set timer")
    "otp" '(org-timer-pause-or-continue :which-key "Pause or continue timer")
    "ott" '(org-timer-stop :which-key "Stop timer"))

    ;; For all local buffers
    (my-local-leader-def
    :states 'normal
    "b" '(embark-bindings :which-key "List buffer key bindings"))

    ;; Org Mode bindings
    (my-local-leader-def
    :states 'normal
    :keymaps 'org-mode-map
    "r" '(:ignore t :which-key "roam")
    "rf" 'org-roam-node-find
    "ri" 'org-roam-node-insert

    "t" 'org-set-tags-command)

    ;; Org Agenda bindings
    (general-define-key
    :keymaps 'org-agenda-mode-map
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line))
