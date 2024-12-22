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

;; TODO: this could be SPC l, and that would map to whatever major mode map
;; I'm currently in.
(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC o"
  "" '(:ignore t :which-key "org"))

;; ** Global Keybindings
(my-leader-def
  :keymaps 'normal
  "f" '(:ignore t :which-key "file")
  "ff" 'find-file)

;; Org Mode bindings
(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "r" '(:ignore t :which-key "roam")
  "rf" 'org-roam-node-find
  "ri" 'org-roam-node-insert
  "y" 'org-store-link
  "p" 'org-insert-link)
