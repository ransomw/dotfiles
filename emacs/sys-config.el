(setq-default show-trailing-whitespace t)
(display-time-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default tab-width 2)
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
(custom-set-faces
  '(default ((t (:background "black" :foreground "grey"))))
  '(fringe ((t (:background "black")))))


;;;; todo: put in seperate files
;;;;;;;;;;;;;recognize haskell pragmas, etc.
(setq file-auto-mode-ignore
      (list "^#!" "^'\\\\\"" "^{-#.*#-}$"))

(defun set-auto-mode-1 ()
  "Find the -*- spec in the buffer.
Call with point at the place to start searching from.
If one is found, set point to the beginning
and return the position of the end.
Otherwise, return nil; point may be changed."
  (let (beg end)
    (and
     ;; Don't look for -*- if this file name matches any
     ;; of the regexps in inhibit-first-line-modes-regexps.
     (let ((temp inhibit-first-line-modes-regexps)
	   (name (if buffer-file-name
		     (file-name-sans-versions buffer-file-name)
		   (buffer-name))))
       (while (let ((sufs inhibit-first-line-modes-suffixes))
		(while (and sufs (not (string-match (car sufs) name)))
		  (setq sufs (cdr sufs)))
		sufs)
	 (setq name (substring name 0 (match-beginning 0))))
       (while (and temp
		   (not (string-match (car temp) name)))
	 (setq temp (cdr temp)))
       (not temp))

     (search-forward "-*-" (line-end-position
                            ;; If the file begins with "#!"
                            ;; (exec interpreter magic), look
                            ;; for mode frobs in the first two
                            ;; lines.  You cannot necessarily
                            ;; put them in the first line of
                            ;; such a file without screwing up
                            ;; the interpreter invocation.
                            ;; The same holds for
                            ;;   '\"
                            ;; in man pages (preprocessor
                            ;; magic for the `man' program).
                            (and (memq 't (mapcar 'looking-at file-auto-mode-ignore)) 2)) t)
     (progn
       (skip-chars-forward " \t")
       (setq beg (point))
       (search-forward "-*-" (line-end-position) t))
     (progn
       (forward-char -3)
       (skip-chars-backward " \t")
       (setq end (point))
       (goto-char beg)
       end))))
