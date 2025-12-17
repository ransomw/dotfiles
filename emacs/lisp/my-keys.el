;; keychords for unbound functions
(global-set-key (kbd "C-c o") 'previous-multiframe-window)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c g") 'grep)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c h") 'show-paren-mode)
(global-set-key (kbd "C-c r") 'toggle-word-wrap)
(global-set-key (kbd "C-c d") 'rainbow-delimiters-mode)

;; avoid duplicate keybindings: use defaults

(provide 'my-keys)
