(setq-default show-trailing-whitespace t)
(display-time-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default js2-basic-offset 2) ;; js2-mode javascript
(setq js-indent-level 2) ;; javascript-mode
(setq css-indent-offset 2)
(server-start)
(put 'narrow-to-region 'disabled nil)
(desktop-save-mode 1)
;(setq x-select-enable-clipboard nil)
(setq x-select-enable-clipboard t)

;; x-emacs config
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(toggle-menu-bar-mode-from-frame)
(toggle-tool-bar-mode-from-frame)
(scroll-bar-mode 0)
(tool-bar-mode -1)
(menu-bar-mode 0)
(custom-set-faces
  '(default ((t (:background "black" :foreground "grey"))))
  '(fringe ((t (:background "black")))))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
