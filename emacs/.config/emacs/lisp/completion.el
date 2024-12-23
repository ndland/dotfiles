;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :straight t
  :custom
  (corfu-auto t)          ;; Enable auto completion
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  :bind
  ;; Another key binding can be used, such as S-SPC.
  ;; (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package cape
  :straight t
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add Cape completion functions to `completion-at-point-functions`
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)  ;; Uncomment if needed
  (add-hook 'completion-at-point-functions #'cape-symbol)   ;; Add more completion types as needed
)
