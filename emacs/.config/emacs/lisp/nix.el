(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :config
  ;; Optional: Hook to enable automatic formatting with nixpkgs-fmt
  (add-hook 'nix-mode-hook #'nixpkgs-fmt-on-save-mode))
