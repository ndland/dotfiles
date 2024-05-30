;;; pocket-reader-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from pocket-reader.el

(autoload 'pocket-reader "pocket-reader" "\
Show Pocket reading list." t)
(autoload 'pocket-reader-add-link "pocket-reader" "\
Add link at point to Pocket.
This function tries to work in multiple major modes, such as w3m,
eww, elfeed, and Org." t)
(autoload 'pocket-reader-eww-add-link "pocket-reader" "\
Add link at point to Pocket in eww buffers." t)
(autoload 'pocket-reader-org-add-link "pocket-reader" "\
Add link at point to Pocket in Org buffers." t)
(with-eval-after-load 'w3m-lnum (cl-defun pocket-reader-w3m-lnum-add-link (&key (type 1)) "Add link to Pocket with lnum in w3m buffers." (interactive) (w3m-with-lnum type "" (when-let ((num (car (w3m-lnum-read-interactive "Anchor number: " 'w3m-lnum-highlight-anchor type last-index w3m-current-url))) (info (w3m-lnum-get-anchor-info num)) (url (car info))) (when (pocket-lib-add-urls url) (message "Added: %s" url))))))
(with-eval-after-load 'w3m (defun pocket-reader-w3m-add-link nil "Add link at point to Pocket in w3m buffers." (interactive) (if-let ((url (or (get-text-property (point) 'w3m-href-anchor) (unless (bolp) (save-excursion (get-text-property (1- (point)) 'w3m-href-anchor))) (unless (eolp) (save-excursion (get-text-property (1+ (point)) 'w3m-href-anchor))) (thing-at-point-url-at-point)))) (when (pocket-lib-add-urls url) (message "Added: %s" url)) (if (member 'w3m-lnum-mode minor-mode-list) (pocket-reader-w3m-lnum-add-link) (message "No URL found around point.")))))
(autoload 'pocket-reader-shr-add-link "pocket-reader" "\
Add link at point in `shr-mode' buffer to Pocket." t)
(with-eval-after-load 'elfeed (defun pocket-reader-elfeed-search-add-link nil "Add links for selected entries in Elfeed search-mode buffer to Pocket.
This is only for the elfeed-search buffer, not for entry buffers." (interactive) (when-let ((entries (elfeed-search-selected)) (links (mapcar #'elfeed-entry-link entries))) (when (pocket-lib-add-urls links) (message "Added: %s" (s-join ", " links)) (elfeed-search-untag-all-unread)))) (defun pocket-reader-elfeed-entry-add-link nil "Add links for selected entries in elfeed-show-mode buffer to Pocket.
This is only for the elfeed-entry buffer, not for search buffers." (interactive) (when-let ((link (elfeed-entry-link elfeed-show-entry))) (when (pocket-lib-add-urls link) (message "Added: %s" link)))))
(autoload 'pocket-reader-generic-add-link "pocket-reader" "\
Try to add URL at point to Pocket using `thing-at-pt'." t)
(register-definition-prefixes "pocket-reader" '("pocket-reader-"))

;;; End of scraped data

(provide 'pocket-reader-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; pocket-reader-autoloads.el ends here
