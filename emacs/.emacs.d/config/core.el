;; Basic settings
(setq inhibit-startup-message t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(setq display-line-numbers t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable recentf-mode to track recently opened files
(setq recentf-max-menu-items 25)      ;; Number of recent files to display in the menu
(setq recentf-max-saved-items 100)    ;; Number of recent files to save

(recentf-mode 1)  ;; Enable recentf-mode
