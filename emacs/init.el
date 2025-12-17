(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
	("MELPA"        . "https://melpa.org/packages/")
	("ORG"          . "https://orgmode.org/elpa/")
	("MELPA Stable" . "https://stable.melpa.org/packages/")
	("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("GNU ELPA"     . 20)
	("MELPA"        . 15)
	("ORG"          . 10)
	("MELPA Stable" . 5)
	("nongnu"       . 0)))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'geiser)
(require 'python)

(require 'my-sys-config) ;; global settings
(require 'my-defuns) ;; custom functions via M-x
(require 'my-keys) ;; keybindings


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(geiser geiser-guile paredit rainbow-delimiters)))

(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(fringe ((t (:background "black")))))
