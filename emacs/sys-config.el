;; ### todo
;; * auto-installer a-la `pythonrc.py`


(setq-default show-trailing-whitespace t)
(display-time-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
;; js2-mode javascript and javascript-mode, resp.
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(server-start)
(put 'narrow-to-region 'disabled nil)
(desktop-save-mode 1)
;(setq x-select-enable-clipboard nil)
(setq x-select-enable-clipboard t)

(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

;; (define-clojure-indent
;;   ;; compojure
;;   (defroutes 'defun)
;;   (GET 2)
;;   (POST 2)
;;   (PUT 2)
;;   (DELETE 2)
;;   (HEAD 2)
;;   (ANY 2)
;;   (context 2)
;;   )

(add-hook 'clojure-mode-hook #'enable-paredit-mode)

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

;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.rt$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.purs$" . haskell-mode))
