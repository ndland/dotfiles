;; -*- lexical-binding: t; -*-

;; Install Org Mode
(use-package org
  :straight t
  :config
  (setq org-directory "~/org"))

;; Install Org Roam
(use-package org-roam
  :straight t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (expand-file-name "~/org/roam")))

;; Install Org Roam UI
(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (org-roam-ui-mode))

(use-package org-bullets
  :straight t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(eval-after-load "org"
  '(require 'ox-md nil t))

;; Enable auto-fill-mode for Org-mode with a line width of 80 characters
(defun my-org-mode-setup ()
  "Custom configurations for Org mode."
  (setq-local fill-column 80) ;; Set the column at which text wraps
  (auto-fill-mode 1))        ;; Enable automatic line breaking

;; Hook the setup function to Org mode
(add-hook 'org-mode-hook #'my-org-mode-setup) (add-hook 'org-mode-hook 'org-indent-mode)

(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/projects.org"
                         "~/org/next-actions.org"
                         "~/org/waiting-for.org"
                         "~/org/someday-maybe.org"))
(setq org-refile-targets
      '(("~/org/inbox.org" :maxlevel . 1)
        ("~/org/projects.org" :maxlevel . 2)
        ("~/org/next-actions.org" :maxlevel . 2)
        ("~/org/waiting-for.org" :maxlevel . 1)
        ("~/org/someday-maybe.org" :maxlevel . 1)))

;; Ensure you can create new headings when refiling
;; (setq org-refile-allow-creating-parent-nodes 'confirm)

;; Org Roam setup
(setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
	org-roam-db-autosync-mode t)

(setq org-archive-location "~/org/archive.org::* Archive")

;; Ensure only top-level headings are expanded on file open
(setq org-startup-folded 'content)  ;; Fold everything except top-level headings

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#FF5733" :weight bold))       ; Bright red
        ("NEXT" . (:foreground "#FFA500" :weight bold))       ; Orange
        ("WAITING" . (:foreground "#FFD700" :weight bold))    ; Yellow
        ("DONE" . (:foreground "#33FF57" :weight bold))       ; Green
        ("CANCELLED" . (:foreground "gray" :weight bold))))   ; Gray

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/inbox.org")
         "* TODO %?\n  %i\n  %a")
        ("p" "Project" entry (file "~/org/projects.org")
         "* %?\n  %i\n  %a")
        ("w" "Waiting For" entry (file "~/org/waiting-for.org")
         "* WAITING %?\n  %i\n  %a")
        ("s" "Someday/Maybe" entry (file "~/org/someday-maybe.org")
         "* %?\n  %i\n  %a")
        ("r" "Reference" entry (file "~/org/reference.org")
         "* %?\n  %i\n  %a")))

(setq org-agenda-custom-commands
      '(("i" "Inbox"
         ((alltodo ""))  ;; Display all TODO entries in the file
         ((org-agenda-files '("~/org/inbox.org"))))  ;; Scope to the inbox file
        ("n" "Next Actions" todo "NEXT")
        ("p" "Projects" tags "project")
        ("w" "Waiting For" todo "WAITING")
        ("s" "Someday/Maybe" tags "someday")
        ("r" "Weekly Review"
         ((agenda "")
          (tags "project")
          (todo "WAITING")
          (tags "someday")))
	("u" "Untagged tasks" tags "-{.+}")))

;; Log done timestamps into a drawer
(setq org-log-done 'time)            ; Log a timestamp when marking a task as DONE
(setq org-log-into-drawer t)         ; Store the log in a drawer
