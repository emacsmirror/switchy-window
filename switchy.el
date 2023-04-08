;; -*- lexical-binding: t; -*-

(defgroup switchy nil
  "switchy is a last-recently-used window-switcher."
  :group 'windows)

(defvar switchy--tick-counter 0
  "TODO")

(defvar switchy--tick-alist nil
  "TODO")

(defcustom switchy-delay 1.5
  "TODO"
  :type 'number)

(defvar switchy--timer nil
  "TODO")
(defvar switchy--visited-windows nil
  "TODO")

(defun switchy--on-window-selection-change (&optional frame)
  (when frame
    (when switchy--timer
      (cancel-timer switchy--timer))
    (setq switchy--timer (run-at-time
                          switchy-delay nil
                          (lambda ()
                            (setf (alist-get (selected-window) switchy--tick-alist)
                                  (cl-incf switchy--tick-counter))
                            (setq switchy--visited-windows nil))))))

;; TODO: Add prefix arg to switch counter-LRU-wise.
(defun switchy-window ()
  (interactive)
  ;; Remove dead windows.
  (setq switchy--tick-alist (seq-filter (lambda (e)
                                          (window-live-p (car e)))
                                        switchy--tick-alist))
  ;; Add windows never selected.
  (dolist (win (window-list (selected-frame)))
    (unless (assq win switchy--tick-alist)
      (setf (alist-get win switchy--tick-alist) 0)))

  ;; Ensure the current window is marked as visited.
  (setq switchy--visited-windows (cons (selected-window)
                                       switchy--visited-windows))

  (let ((win-entries (seq-filter
                      (lambda (e)
                        (let ((win (car e)))
                          (and (eq (window-frame win) (selected-frame))
                               (or (minibuffer-window-active-p win)
                                   (not (eq win (minibuffer-window
                                                 (selected-frame)))))
                               (not (memq win switchy--visited-windows)))))
                      switchy--tick-alist)))
    (if win-entries
        (when-let ((win (car (seq-reduce (lambda (x e)
                                           (if (and x (> (cdr x) (cdr e)))
                                               x
                                             e))
                                         win-entries nil))))

          (progn
            (setq switchy--visited-windows (cons win switchy--visited-windows))
            (select-window win)))
      (when switchy--visited-windows
        (setq switchy--visited-windows nil)
        (switchy-window)))))

(defvar switchy-minor-mode-map (make-sparse-keymap))

(define-minor-mode switchy-minor-mode
  "TODO"
  :global t
  :keymap switchy-minor-mode-map
  (if switchy-minor-mode
      (add-hook 'window-selection-change-functions
                #'switchy--on-window-selection-change)
    (remove-hook 'window-selection-change-functions
                 #'switchy--on-window-selection-change)))

(provide 'switchy)
