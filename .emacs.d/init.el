(setq ring-bell-function 'ignore)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install 'use-package' if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install and configure the Dracula theme
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; Set up Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h ." . helpful-at-point)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (completion-styles '(orderless)))

(use-package orderless
  :ensure t)

(use-package consult
  :ensure t
  :bind (("C-x C-r" . consult-find)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'elisp))

;; Additional languages can be added similarly
;; (tree-sitter-require 'javascript)

(use-package org
  :ensure t)

(use-package org-roam
  :ensure t
  :config
  (org-roam-mode))

(setq org-roam-directory "~/org/roam")

;; Set up Org mode
(require 'org)

;; org-mode is 80 characters until line break
(add-hook 'org-mode-hook
          (lambda ()
            (setq fill-column 80)
            (turn-on-auto-fill)))

;; Enable flyspell in Org mode
(add-hook 'org-mode-hook #'turn-on-flyspell)

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Set the flyspell language to American English
(setq ispell-dictionary "american")

(setq org-directory "~/org")

(global-set-key (kbd "C-c n r") 'org-roam)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n t") 'org-roam-dailies-goto-today)

;; Customize the Tab key for Org mode headings in Evil mode
(evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
(evil-define-key 'visual org-mode-map (kbd "<tab>") 'org-cycle)
(evil-define-key 'insert org-mode-map (kbd "<tab>") 'org-cycle)
(evil-define-key 'normal org-agenda-mode-map (kbd "<tab>") 'org-agenda-fold-toggle)
(evil-define-key 'visual org-agenda-mode-map (kbd "<tab>") 'org-agenda-fold-toggle)
(evil-define-key 'insert org-agenda-mode-map (kbd "<tab>") 'org-agenda-fold-toggle)

;; Set up variable pitch font for Org mode
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Iosevka Etoile"))))
 '(fixed-pitch ((t (:weight normal :height 1.2 :family "Iosevka Nerd Font Mono")))))

;; Set the default font to Iosevka Nerd Font Mono
(set-frame-font "Iosevka Nerd Font Mono-12" nil t)

;; Additional configuration for Org mode
(setq org-startup-indented t
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

;; Customize other variables as needed

;; Save and load the customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
