(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c z") 'query-replace-regexp)
(global-set-key (kbd "C-c o") 'previous-multiframe-window)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c g") 'grep)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c h") 'show-paren-mode)
(global-set-key (kbd "C-c r") 'toggle-word-wrap)
(global-set-key (kbd "C-c n") 'kill-line-number)
(global-set-key (kbd "C-c e") 'recursive-edit)
(global-set-key (kbd "C-c p") 'shell)
(global-set-key (kbd "C-c f") 'shell)
(global-set-key (kbd "C-c q") 'rename-buffer)
(global-set-key (kbd "C-c d") 'rainbow-delimiters-mode)
(global-set-key (kbd "C-c m") 'magit-status)

(global-set-key (kbd "C-}") 'forward-page)
(global-set-key (kbd "C-{") 'backward-page)

;;; for defuns.el
(global-set-key (kbd "C-c x") 'query-replace-region)
(global-set-key (kbd "C-c n") 'set-marker-name)
(global-set-key (kbd "C-c j") 'goto-named-marker)
(global-set-key (kbd "C-c b") 'kill-buffer-name)
(global-set-key (kbd "C-c k") 'read-to-kill-ring)
(global-set-key (kbd "C-c a") 'add-abbreviation)
(global-set-key (kbd "C-c s") 'get-abbreviation)
(global-set-key (kbd "C-c t") 'toggle-indent-tabs-mode)
 ;; C-8 will increase opacity (== decrease transparency)
 ;; C-9 will decrease opacity (== increase transparency
 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)
															 (progn
																 (djcb-opacity-modify)
																 ;; (print "decreased opacity")
																 )
															 ))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))



;; (derived-mode-map-name 'python-mode)

(require 'python)
(define-key python-mode-map (kbd "M-n") 'forward-symbol)

