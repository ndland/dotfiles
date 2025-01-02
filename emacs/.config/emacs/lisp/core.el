;; Basic settings
(setq inhibit-startup-message t)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(setq display-line-numbers t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable recentf-mode to track recently opened files
(setq recentf-max-menu-items 25)      ;; Number of recent files to display in the menu
(setq recentf-max-saved-items 100)    ;; Number of recent files to save

(recentf-mode 1)  ;; Enable recentf-mode

(use-package tree-sitter
  :straight t
  :hook ((prog-mode . turn-on-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  (setq tree-sitter-highlight-method 'font-lock)) ;; Optional: Use 'overlay' for an alternative method

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))
