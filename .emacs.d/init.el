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

;; Set up package management
(require 'package)
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

(use-package org
  :straight t
  :after evil
  :hook (org-mode . (lambda ()
		       (setq fill-column 80)
		       (auto-fill-mode 1)))
  :bind ("C-x a" . org-agenda)
  :config

  (defun my-org-archive-done-tasks ()
    "Automatically archive tasks that are marked 'DONE'"
    (when (string= (org-get-todo-state) "DONE")
      (org-archive-subtree)))

  (add-hook 'org-after-todo-state-change-hook 'my-org-archive-done-tasks)

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
         "* TODO %?\n  %i\n  %a")))

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

(use-package org-roam
  :straight t
  :config
  (setq org-roam-directory "~/org/roam")

  ;; From https://www.orgroam.com/manual.html#Setting-up-Org_002droam
  (org-roam-db-autosync-mode)
  
  (global-set-key (kbd "C-c n r") 'org-roam)
  (global-set-key (kbd "C-c n f") 'org-roam-node-find)
  (global-set-key (kbd "C-c n g") 'org-roam-graph)
  (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c n t") 'org-roam-dailies-goto-today)
  (org-roam-mode))

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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

  ;; Define your prefix key
  (global-set-key (kbd "C-x a") (make-sparse-keymap))

  ;; Give a name to your prefix
  (which-key-add-key-based-replacements "C-x a" "Org Agenda")

  ;; Add bindings under your prefix
  (define-key (global-key-binding (kbd "C-x a")) (kbd "a") 'org-agenda)

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
