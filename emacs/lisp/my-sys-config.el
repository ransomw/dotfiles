(setq-default show-trailing-whitespace t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(put 'narrow-to-region 'disabled nil)


(set-face-attribute 'region nil :background "#999")


(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;; x-emacs config
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(toggle-menu-bar-mode-from-frame)
(toggle-tool-bar-mode-from-frame)
(scroll-bar-mode 0)
(tool-bar-mode -1)
(menu-bar-mode 0)


(provide 'my-sys-config)
