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
  "b" '(:ignore t :which-key "buffer")
  "bb" '(consult-buffer :which-key "Switch buffer")
  "bk" '(kill-this-buffer :which-key "Kill buffer")

  "f" '(:ignore t :which-key "file")
  "ff" '(find-file :which-key "Find file")
  "fs" '(save-buffer :which-key "Save file")

  "g" '(:ignore t :which-key "git")
  "gs" '(magit-status :which-key "Git status")

  "n" '(:ignore t :which-key "notes")
  "nf" '(org-roam-node-find :which-key "Find roam node")
  "nd" '(org-roam-dailies-goto-today :which-key "Go to today")

  "s" '(:ignore t :which-key "Search")
  "sb" '(consult-line :which-key "Search current buffer")
  "sf" '(consult-line-multi :which-key "Search project files")
  "sr" '(consult-ripgrep :which-key "Ripgrep files")

  "o" '(:ignore t :which-key "org")
  "oa" '(org-agenda :which-key "Org agenda")
  "ot" '(:ignore t :which-key "org timer")
  "ots" '(org-timer-set-timer :which-key "Set timer")
  "otp" '(org-timer-pause-or-continue :which-key "Pause or continue timer")
  "ott" '(org-timer-stop :which-key "Stop timer"))

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
  "k" 'org-agenda-previous-line)
