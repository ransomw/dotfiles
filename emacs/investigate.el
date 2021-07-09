;; ideas
;;;;;;;;;;;
;; ??? elisp comment-out s-exp?
;; *install rainbow-parens*<<>>
;; zprint elisp buffers
;; toggle scroll bar for one buffer at an time:
;;     not toggle all buffers
;;;;;;;;;

;;;
;; goal:
;;   decide where *grep* results open -- don't commandeer
;;    the wrong window
;; `compile-goto-error` is the function that opens *grep*
;; results.



(defun event-end--orig (event)
  "Return the ending position of EVENT.
EVENT should be a click, drag, or key press event.

The following accessor functions are used to access the elements
of the position:

`posn-window': The window the event is in.
`posn-area': A symbol identifying the area the event occurred in,
or nil if the event occurred in the text area.
`posn-point': The buffer position of the event.
`posn-x-y': The pixel-based coordinates of the event.
`posn-col-row': The estimated column and row corresponding to the
position of the event.
`posn-actual-col-row': The actual column and row corresponding to the
position of the event.
`posn-string': The string object of the event, which is either
nil or (STRING . POSITION)'.
`posn-image': The image object of the event, if any.
`posn-object': The image or string object of the event, if any.
`posn-timestamp': The time the event occurred, in milliseconds.

For more information, see Info node `(elisp)Click Events'."
  (if (consp event) (nth (if (consp (nth 2 event)) 2 1) event)
    (or (posn-at-point)
        (list (selected-window) (point) '(0 . 0) 0))))


(defun cge-event-introspect (event)
  (if (not (consp event))
      "NOT event is cons cell"
    (let* ((eventsthird (nth 2 event)))
      (list "event is cons cell"
            (cons "second"
                  (nth 1 event))
            (cons "[ooone] third"
                  eventsthird)
            "and"
            (if (consp eventsthird)
                (concat
                 "third item is cell, "
                 "so `event-end` will eval to it")
              (concat
               "third item is not cell"
               " => `event-end` will eval "
               "to second item"))))))

(defun narrate-event-end (event)
  (let*
      ((event-end-pre-dat
        (if event
            (if-let* ((_  (cge-event-introspect event))
                      (__ (consp _)))
                _ ;; intropsection result a list
              ;; or event isn't a cons cell, so move on
              (if-let ((curr-posn-at-pt (posn-at-point)))
                  (format (concat "have position at point %S "
                                  "`event-end` will return it")
                          curr-posn-at-pt)
                (let ((event-end-rv
                       (list (selected-window) (point) '(0 . 0) 0)))
                  (concat "failing event introspection"
                          "and getting position from point,"
                          " `event-end` will return "
                          (format "%S" event-end-rv)
                          " that's "
                          "(list (selected-window) (point) '(0 . 0) 0)"))))
          (list (concat "event not present "))))
       (event-end-dat
        (list
         ;; event
         ;; last-input-event
         (if (eq event last-input-event)
             "event eq last-input-event"
           "NOT event eq last-input-event")
         (if (= event last-input-event)
             "(= event last-input-event)"
           "NOT (= event last-input-event)")
         event-end-pre-dat)))
    (print (format "%S" event-end-dat))))


(defun compile-goto-error--orig (&optional event)
  "Visit the source for the error message at point.
Use this command in a compilation log buffer."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (compilation--ensure-parse (point))
  (if (get-text-property (point) 'compilation-directory)
      (dired-other-window
       (car (get-text-property (point) 'compilation-directory)))
    (setq compilation-current-error (point))
    (next-error-internal)))



(defun my-compile-goto-error (&optional event)
  "Visit the source for the error message at point.
Use this command in a compilation log buffer."
  (interactive (list last-input-event))
  (read-from-minibuffer "event end will go something like this")
  (narrate-event-end event)
  (read-from-minibuffer "got all tHat???")
  (read-from-minibuffer "doesn't matter anyway 💕☺💕😊💕")
  (cge-helper event)

  )



(nth 1 '(1 2 3))

(defun cge-helper (event)
 (if event (posn-set-point (event-end event)))
 (or (compilation-buffer-p (current-buffer))
     (error "Not in a compilation buffer"))
 (compilation--ensure-parse (point))
 (if (get-text-property (point) 'compilation-directory)
     (dired-other-window
      (car (get-text-property (point) 'compilation-directory)))
   (setq compilation-current-error (point))
   (next-error-internal)))
