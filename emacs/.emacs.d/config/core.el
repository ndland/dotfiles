;; Basic settings
(setq inhibit-startup-message t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(setq display-line-numbers t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
