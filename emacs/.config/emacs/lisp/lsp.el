;; LSP setup
(use-package lsp-mode
  :straight t
  :commands lsp
  :hook ((python-mode js-mode typescript-mode) . lsp)
  :config
  (setq lsp-prefer-flymake nil) ;; Use flycheck
  (setq lsp-idle-delay 0.500)
  (setq lsp-headerline-breadcrumb-enable nil))

;; UI enhancements for LSP
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 0.3)
  (setq lsp-ui-sideline-enable t))

;; File browser with LSP
(use-package treemacs
  :straight t)

(use-package lsp-treemacs
  :straight t
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;; Corfu integration with LSP
(use-package lsp-mode
  :config
  (setq lsp-completion-provider :none) ;; Use Corfu
  (defun corfu-lsp-setup ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (add-hook 'lsp-completion-mode-hook #'corfu-lsp-setup)
  (add-to-list 'completion-at-point-functions #'lsp-completion-at-point))
