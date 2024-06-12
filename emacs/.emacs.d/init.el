(setq ring-bell-function 'ignore)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Start the server if it's not already started
(load "server")
(unless (server-running-p)
  (server-start))

(cond
 ((string-equal system-type "windows-nt") ; For Windows
  (set-frame-font "MonaspiceAr NFM-13" nil t))
 ((string-equal system-type "darwin") ; For macOS
  (set-frame-font "MonaspiceAr Nerd Font-13" nil t))
 ((string-equal system-type "gnu/linux") ; For Linux
  (set-frame-font "MonaspiceAr Nerd Font-13" nil t)))

;; Set up proxy server settings
(when (string= system-name "GMMACANCN6345JM")
  (setq my-proxy (getenv "HTTP_PROXY"))
  (setq url-proxy-services
	`(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . ,my-proxy)
          ("https" . ,my-proxy))))

 ;
; Set up package management
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Set up Evil mode
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(use-package restart-emacs
  :straight t
  :config
  (global-set-key (kbd "M-r") 'restart-emacs))

(use-package helpful
  :straight t
  :config
    ;; Note that the built-in `describe-function' includes both functions
    ;; and macros. `helpful-function' is functions only, so we provide
    ;; `helpful-callable' as a drop-in replacement.
    (global-set-key (kbd "C-h f") #'helpful-callable)

    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h x") #'helpful-command)

    ;; Lookup the current symbol at point. C-c C-d is a common keybinding
    ;; for this in lisp modes.
    (global-set-key (kbd "C-c C-d") #'helpful-at-point)

    ;; Look up *F*unctions (excludes macros).
    ;;
    ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
    ;; already links to the manual, if a function is referenced there.
    (global-set-key (kbd "C-h F") #'helpful-function))

(use-package dracula-theme
  :straight t
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'dracula t))

(defun nl/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :straight t
  :after evil
  :hook (org-mode . nl/org-mode-setup)
  :config

  (dolist (face '((org-level-1 . 1.7)
		  (org-level-2 . 1.6)
		  (org-level-3 . 1.5)
		  (org-level-4 . 1.4)
		  (org-level-5 . 1.3)
		  (org-level-6 . 1.2)
		  (org-level-7 . 1.2)
		  (org-level-8 . 1.2)))
    (set-face-attribute (car face) nil :font "ETBembo" :weight 'regular :height (cdr face)))

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-todo-keyword-faces
	'(("TODO" . "#FFB86C")
	  ("WAITING" . "#8BE9FD")
	  ("DONE" . "#50FA7B")
	  ("CANCELLED" . (:foreground "#FF5555" :weight bold))))

  ;; Make sure org-indent face is available
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)

  ;; (defun my-org-archive-done-tasks ()
  ;;   "Automatically archive tasks that are marked 'DONE'"
  ;;   (when (string= (org-get-todo-state) "DONE")
  ;;     (org-archive-subtree)))

  ;; (add-hook 'org-after-todo-state-change-hook 'my-org-archive-done-tasks)

  (setq org-agenda-custom-commands
	'(("A" "Archived tasks from the last week"
	   agenda ""
	   ((org-agenda-start-day "-7d")  ; start 7 days ago
	    (org-agenda-span 7)           ; for the next 7 days
	    (org-agenda-show-log t)       ; display logged tasks
	    (org-agenda-log-mode-items '(closed)) ; only show closed tasks
	    (org-agenda-archives-mode t)  ; include archive files
	    (org-agenda-files (org-agenda-files nil 'archives)))))) ; only archive files

  ;; When a TODO is set to a done state, record a timestamp
  (setq org-log-done 'time)

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

  (setq org-startup-with-inline-images t)

  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("w" "Web bookmark" entry
           (file+headline "~/org/bookmarks.org" "Web Bookmarks")
           "* %^{Description}\n Source: %^{URL}\n\n  %i"
           :empty-lines 1)))

  ;; Make the indentation look nicer
  (add-hook 'org-mode-hook 'org-indent-mode)

  ;; Wrap the lines in org mode so that tjngs are easier to read
  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; Customize the Tab key for Org mode headings in Evil mode
  (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
  (evil-define-key 'visual org-mode-map (kbd "<tab>") 'org-cycle)
  (evil-define-key 'insert org-mode-map (kbd "<tab>") 'org-cycle)
  (evil-define-key 'normal org-agenda-mode-map (kbd "<tab>") 'org-agenda-fold-toggle)
  (evil-define-key 'visual org-agenda-mode-map (kbd "<tab>") 'org-agenda-fold-toggle)
  (evil-define-key 'insert org-agenda-mode-map (kbd "<tab>") 'org-agenda-fold-toggle)

  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (setq org-agenda-files '("~/org")))

(use-package org-contrib
  :straight t
  :config
  (require 'org-checklist))

(use-package org-roam
  :straight t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-directory "~/org/roam")

  ;; From https://www.orgroam.com/manual.html#Setting-up-Org_002droam
  (org-roam-db-autosync-mode)

  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  
  (org-roam-mode))

(use-package org-contacts
  :straight t
  :after org
  :config
  (setq org-contacts-files '("~/org/contacts.org")))

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ob-mermaid
  :straight t
  :config
  (setq ob-mermaid-cli-path "/Users/tz4m2z/Library/Caches/fnm_multishells/85950_1715192108704/bin/mmdc")

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((mermaid . t)
      (scheme . t))))

(use-package mermaid-mode
  :straight t
  :config
  (setq mermaid-mmdc-location "/Users/tz4m2z/Library/Caches/fnm_multishells/85950_1715192108704/bin/mmdc"))

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :custom
  (completion-styles '(orderless)))

(use-package orderless
  :straight t)

(use-package consult
  :straight t
  :bind (("C-x C-r" . consult-find)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package company
  :straight t
  :config
  ;; No delay in showing suggestions.
  (setq company-idle-delay 0)

  ;; Show suggestions after entering one character.
  (setq company-minimum-prefix-length 1)

  ;; Wrap around the list of suggestions.
  (setq company-selection-wrap-around t)

  ;; Use Tab key to cycle through suggestions (tab-and-go).
  (company-tng-configure-default)

  ;; Enable Company Mode globally.
  (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (((c-mode lisp-mode) . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (add-hook 'lisp-mode-hook #'lsp-lisp-alive-start-ls))

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

;; optional if you want which-key integration
(use-package which-key
  :straight t
  :config
  (which-key-mode)

  (global-set-key (kbd "C-x y") 'yadm)
  (global-set-key (kbd "C-x e") 'eval-region)

  ;; Define your prefix key
  (global-set-key (kbd "C-c o") (make-sparse-keymap))

  ;; Give a name to your prefix
  (which-key-add-key-based-replacements "C-c o" "Org")

  ;; Add bindings under your prefix
  (define-key (global-key-binding (kbd "C-c o")) (kbd "a") 'org-agenda)
  (define-key (global-key-binding (kbd "C-c o")) (kbd "s") 'org-search-view)
  (define-key (global-key-binding (kbd "C-c o")) (kbd "c") 'org-contacts)
  (define-key (global-key-binding (kbd "C-c o")) (kbd "p") 'org-capture)

  ;; Org mode specific bindings
  (with-eval-after-load 'org
    ;; Bind "C-x a t" to 'insert-now-time in org-mode-map
    (define-key org-mode-map (kbd "C-c t") 'insert-now-time)
    
    ;; Add which-key description for the new binding
    (which-key-add-key-based-replacements "C-c t" "Insert Now Time")))

(use-package emacs-everywhere
  :straight t)

(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package org-download
  :straight t
  :after org)

(use-package magit-lfs
  :straight t)

(use-package vundo
  :straight t
  :bind
  ("C-c v" . vundo)
  :config
  (setq vundo-compact-display t))

(use-package deft
  :straight t
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(use-package pocket-reader
  :straight t)

(use-package tramp
  :straight t
  :config
  (add-to-list 'tramp-methods
	       '("yadm"
		 (tramp-login-program "yadm")
		 (tramp-login-args (("enter")))
		 (tramp-login-env (("SHELL") ("/bin/sh")))
		 (tramp-remote-shell "/bin/sh")
		 (tramp-remote-shell-args ("-c")))))

(use-package langtool
  :straight t
  :config
  (setq langtool-bin "/opt/homebrew/bin/languagetool")
  (setq langtool-default-language "en-US")

  (add-hook 'org-mode-hook
	    (lambda ()
	      (add-hook 'before-save-hook 'langtool-check nil 'local))))

;; Ensure projectile is installed and configured using use-package
(use-package projectile
  :straight t
  :init
  ;; Optional: Enable projectile globally
  (projectile-mode +1)
  :config
  ;; Optional: Set the keymap prefix for Projectile commands
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; Optional: Enable caching
  (setq projectile-enable-caching t))

(use-package counsel-projectile
  :after (projectile counsel)
  :straight t
  :config
  (counsel-projectile-mode))

;; Install and configure ini-mode
(use-package ini-mode
  :straight t
  :mode ("\\.ini\\'" . ini-mode))

(use-package devdocs
  :straight t
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup))

(use-package nov
  :straight t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(defun yadm ()
  (interactive)
  (magit-status "/yadm::"))

(defun insert-now-time ()
  "Insert the current time (hh:mm) in Org Mode."
  (interactive)
  (insert (format-time-string "%H:%M" (current-time))))

;; Save and load the customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
