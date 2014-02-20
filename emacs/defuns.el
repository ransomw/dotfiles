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

(setq abbrev-list ())
(defun add-abbreviation ()
  (interactive)
  (let ((abbrev (read-from-minibuffer "abbreviation: "))
        (val (read-from-minibuffer "for string: ")))
    (if (assoc abbrev abbrev-list)
        (remove (assoc abbrev abbrev-list) abbrev-list))
    (setq abbrev-list (cons (cons abbrev val) abbrev-list))
    (print abbrev)))

;; todo: popup new buffer and hit enter on some abbre
;;       if given the universal arg
(defun get-abbreviation (list-abbrevs)
  (interactive "P")
	(if list-abbrevs
			(print abbrev-list)
		(progn
			(setq enable-recursive-minibuffers t)
			(let* ((abbrev (read-from-minibuffer "abbreviation: "))
						 (val (cdr (assoc abbrev abbrev-list)))
						 (ins-str (if val val "")))
				(insert ins-str))
			(setq enable-recursive-minibuffers nil))))

;not even sure what this is supposed to do....
(defun replace-kill-ring-regexp (idx)
  (interactive "P")
  (let ((i (if (not idx) 0 idx)))
    (print (format "%S" i))))

(defun toggle-indent-tabs-mode ()
  (interactive)
  (if indent-tabs-mode
      (print (setq-default indent-tabs-mode nil))
    (print (setq-default indent-tabs-mode t))))

(defun generate-makefile-targets (filenames)
  "given a list of .c filenames, insert implicit makefile
   rules at the end of the current buffer"
  (dolist (filename filenames)
    (grep (concat "grep \"include\" " filename))
    (next-multiframe-window)
    (sleep-for 1)
    (copy-region-as-kill (point-min) (point-max))
    (previous-multiframe-window)
    (let ((grep-begin (point)))
      (yank)
      (let ((grep-end (point)))
        (narrow-to-region grep-begin grep-end)
        (beginning-of-buffer)
        (save-excursion
          (keep-lines "^#include"))
        (save-excursion
          (while (search-forward-regexp "^.*\"\\(.*\\)\"$" nil t)
            (replace-match "\\1 ")))
        (save-excursion
          (while (search-forward-regexp "\n" nil t)
            (replace-match " ")))
        (delete-trailing-whitespace)
        (save-excursion
          (insert "\n")
          (insert
           (concat (substring filename 0 -2) ".o"))
          (insert " : ")
          (end-of-buffer) (insert "\n"))
        (widen)))))

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


;; (defun my-search-replace ()
;; 	"""use the first argument of this function
;;   to specify one of the (many) available search &/or replace
;;   functions and in so doing, curcumvent the unfortunate shortage of
;;   reserverd user keybinding"""
;; 	(interactive "P")
(defun insert-time ()
	""" read mintues and seconds from minibuffer,
insert seconds at point
"""
;	(interactive)
	(let ((min (read-from-minibuffer "minutes"))
				(sec (read-from-minibuffer "seconds")))
		(insert (number-to-string
						 (+(* 60 (string-to-number min))
							 (string-to-number sec))))))

(defun insert-current-date () (interactive)
	(insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun insert-current-time () (interactive)
	(insert (shell-command-to-string "echo -n $(date +%H-%M)")))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
(replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
)

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))
