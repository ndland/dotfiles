;;
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

;; Setup directories
(defvar my-config-dir (expand-file-name "config/" user-emacs-directory)
  "Directory for modular configuration files.")

(unless (file-exists-p my-config-dir)
  (make-directory my-config-dir))

;; Ensure straight.el works with use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t)

;; Load modular configuration files
(mapc 'load (directory-files my-config-dir 'full "\\.el$"))

;; Set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (not (file-exists-p custom-file))
  (write-region "" nil custom-file))
(load custom-file)
