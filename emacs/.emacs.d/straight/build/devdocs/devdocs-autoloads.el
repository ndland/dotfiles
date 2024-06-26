;;; devdocs-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from devdocs.el

(autoload 'devdocs-delete "devdocs" "\
Delete DevDocs documentation.
DOC is a document metadata alist.

(fn DOC)" t)
(autoload 'devdocs-install "devdocs" "\
Download and install DevDocs documentation.
DOC is a document slug or metadata alist.  If the document is
already installed, reinstall it.

(fn DOC)" t)
(autoload 'devdocs-update-all "devdocs" "\
Reinstall all documents with a new version available." t)
(autoload 'devdocs-lookup "devdocs" "\
Look up a DevDocs documentation entry.

Display entries in the documents `devdocs-current-docs' for
selection.  With a prefix argument (or, from Lisp, if ASK-DOCS is
non-nil), first read the name of one or more installed documents
and set `devdocs-current-docs' for this buffer.

If INITIAL-INPUT is not nil, insert it into the minibuffer.

(fn &optional ASK-DOCS INITIAL-INPUT)" t)
(autoload 'devdocs-peruse "devdocs" "\
Read a document from the first page.

(fn DOC)" t)
(autoload 'devdocs-search "devdocs" "\
Search for QUERY in the DevDocs website.

(fn QUERY)" t)
(register-definition-prefixes "devdocs" '("devdocs-"))

;;; End of scraped data

(provide 'devdocs-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; devdocs-autoloads.el ends here
