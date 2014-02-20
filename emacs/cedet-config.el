;(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(global-semantic-tag-folding-mode)
;(global-srecode-minor-mode 1)            ; Enable template insertion
(global-set-key (kbd "C-c =") 'semantic-tag-folding-show-block)
(global-set-key (kbd "C-c -") 'semantic-tag-folding-fold-block)
(global-set-key (kbd "C-c +") 'semantic-tag-folding-show-all)
(global-set-key (kbd "C-c _") 'semantic-tag-folding-fold-all)
(global-set-key (kbd "C-c )") 'semantic-tag-folding-show-children)
(global-set-key (kbd "C-c (") 'semantic-tag-folding-fold-children)
