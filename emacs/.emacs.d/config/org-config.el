;; -*- lexical-binding: t; -*-

;; Enable auto-fill-mode for Org-mode with a line width of 80 characters
(defun my-org-mode-setup ()
  "Custom configurations for Org mode."
  (setq-local fill-column 80) ;; Set the column at which text wraps
  (auto-fill-mode 1))        ;; Enable automatic line breaking

;; Hook the setup function to Org mode
(add-hook 'org-mode-hook #'my-org-mode-setup)

(setq org-agenda-files '("~/org/todo.org"))

;; Org Roam setup
(setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
	org-roam-db-autosync-mode t)

(setq org-archive-location "~/org/archive.org::* Archive")
