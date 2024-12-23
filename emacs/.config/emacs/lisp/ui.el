;; UI enhancements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package catppuccin-theme
  :straight t
  :config
  (setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha
  (load-theme 'catppuccin t))

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5) ;; Delay before the menu pops up
  (setq which-key-idle-secondary-delay 0.1))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package helpful
  :straight t
  :commands (helpful-callable helpful-function helpful-variable helpful-key)
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C" . helpful-command))
  :config
  (setq helpful-max-buffers 10))  ;; Optional: limit the number of helpful buffers
