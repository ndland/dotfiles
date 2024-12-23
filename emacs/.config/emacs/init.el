(setq user-emacs-directory (expand-file-name "~/.config/emacs/"))

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

;; Configure straight.el with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Directory for modular configuration files
(defvar my-config-dir (expand-file-name "lisp/" user-emacs-directory)
  "Directory for modular configuration files.")

;; Ensure the directory exists
(unless (file-exists-p my-config-dir)
  (make-directory my-config-dir))

;; Add modular configuration directory to load-path
(add-to-list 'load-path my-config-dir)

;; Load all `.el` files in my-config-dir
(mapc (lambda (file)
        (load (file-name-sans-extension file)))
      (directory-files my-config-dir 'full "\\.el$"))

;; Set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
