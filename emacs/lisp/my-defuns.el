(defun kill-buffer-name (num)
  "appends buffer name to kill ring. defaults to current buffer,
    otherwise, appends the name of (other-window NUM).
    returns the buffer name."
  (interactive "P") ; prefix arg
  (if num
      (other-window num))
  (kill-new (buffer-name) nil)
  (if num
      (other-window (* -1 num)))
  (print (car kill-ring)))

(defun read-to-kill-ring ()
  "reads text from minibuffer into kill ring"
  (interactive)
  (kill-new (read-from-minibuffer "append to kill ring: ") nil))

;;;;
;; maintain an assoc list,
;;     name -> location
;; setter + getter nested in "goto marker" defun
;;
;; ### todo
;; * tab-complete in nested getter minibuffer
;; *~ take naming convention more seriously
(setq marker-names ())
(defun set-marker-name ()
  (interactive)
  (let* ((name (read-from-minibuffer "name: "))
        (m (point-marker))
        (curr-pair (assoc name marker-names)))
    (if curr-pair
        (remove curr-pair marker-names))
    (setq marker-names (cons (cons name m) marker-names))
    (print name)))

(defun goto-named-marker (show-marker-names)
  (interactive "P")
	(if show-marker-names
			(print (mapcar 'car marker-names))
		(let* ((name (read-from-minibuffer "goto marker name: "))
					 (m (cdr (assoc name marker-names))))
			(switch-to-buffer (marker-buffer m))
			(goto-char (marker-position m)))))

(defun query-replace-region ()
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (region-str (buffer-substring beg end))
         (repl-txt (read-from-minibuffer
                    (concat "Query replace " region-str " with: "))))
    (goto-char beg)
    (deactivate-mark)
    (save-excursion
      (query-replace region-str repl-txt))))

(defun insert-current-date () (interactive)
	(insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun insert-current-time () (interactive)
	(insert (shell-command-to-string "echo -n $(date +%H-%M)")))

;; todo: make-scriptable calls like (quote-lines "{" "}") possible
(defun quote-lines (ask-for-quote?)
  "add quotation marks around each line of text.
    prefix arg C-u will allow \"quoting\" with other strings
    hitting C-u twice will allow a seperate end quote"
  (interactive "P")			; prefix arg
  (let ((quote-str
         (if ask-for-quote?
             (read-from-minibuffer "quote string: ")
           "\""))
        (quote-str-end
         (if (listp ask-for-quote?)
             (if (eq (car ask-for-quote?) 16)
                 (read-from-minibuffer "end quote string: ")))))
    (replace-regexp
     "\\(.*\\)"
     (concat quote-str "\\1"
             (if quote-str-end
                 quote-str-end
               quote-str)))))

(provide 'my-defuns)
