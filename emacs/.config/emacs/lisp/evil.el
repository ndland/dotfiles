(use-package evil
  :straight t
  :init
  (setq evil-want-integration t) ;; Integrates with standard Emacs packages
  (setq evil-want-keybinding nil) ;; Disable default keybindings for more control
  :config
  (evil-mode 1)) ;; Enable Evil globally

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))
