;;; switchy.el --- A last-recently-used window switcher  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Tassilo Horn
;;
;; Author: Tassilo Horn <tsdh@gnu.org>
;; Version: 1.0
;; Keywords: windows
;; Homepage: https://sr.ht/~tsdh/switchy/
;; Repository: https://git.sr.ht/~tsdh/switchy
;; Package-Requires: ((emacs "25.1") (compat "29.1.3.4"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Switchy is a last-recently-used window switcher.  It suits my personal Emacs
;; layout and workflow where I usually have at most two editing windows but up
;; to three side-windows which I have to select only seldomly.
;;
;; The idea of switchy is simple: when you invoke `switchy-window' in quick
;; succession, it will switch to one window after the other in
;; last-recently-used order.  Once you stop switching for long enough time
;; (`switchy-delay', 1.5 seconds by default), the selected window gets locked
;; in, i.e., its LRU timestamp is updated and this switching sequence is ended.
;; Thusly, you can toggle between two windows simply by invoking
;; `switchy-window', waiting at least `switchy-delay', and then invoking
;; `switchy-window' again to switch back to the original window.
;;
;; Activate `switchy-minor-mode' which tracks window changes and bind
;; `switchy-window' to a key of your liking in `switchy-minor-mode-map' (or
;; globally, see the variable's docstring for examples).
;;
;; Hint: Since the order of window switching is not as obvious as it is with
;; `other-window', adding a bit visual feedback to window selection changes can
;; be helpful.  That can be done easily with the stock Emacs pulse.el, e.g.:
;;
;;  (add-hook 'window-selection-change-functions
;;            (lambda (frame)
;;              (when (eq frame (selected-frame))
;;                (pulse-momentary-highlight-one-line))))


;;; Code:

(defgroup switchy nil
  "switchy is a last-recently-used window-switcher."
  :group 'windows)

(defvar switchy--tick-counter 0
  "Values of this counter represent the last-recently-used order of windows.
Only for internal use.")

(defvar switchy--tick-alist nil
  "An alist with entries (WINDOW . TICK).
A higher TICK value means a window has more recently been visited.
Only for internal use.")

(defcustom switchy-delay 1.5
  "Number of seconds before the current window gets locked in after
switching and the switching sequence ends."
  :type 'number)

(defvar switchy--timer nil
  "The timer locking in the current window after `switchy-delay' seconds.
Only for internal use.")

(defvar switchy--visited-windows nil
  "The windows having already been visited in the current switching cycle.")

(defun switchy--on-window-selection-change (&optional frame)
  "Record the next `switchy--tick-counter' value for the selected window.
Meant to be used in `window-selection-change-functions' which is
arranged by `switchy-minor-mode'."
  (when (eq frame (selected-frame))
    (when switchy--timer
      (cancel-timer switchy--timer))
    (setq switchy--timer (run-at-time
                          switchy-delay nil
                          (lambda ()
                            (setf (alist-get (selected-window)
                                             switchy--tick-alist)
                                  (cl-incf switchy--tick-counter))
                            (setq switchy--visited-windows nil))))))

(defun switchy-window (&optional arg)
  "Switch to other windows in last-recently-used order.
If prefix ARG is given, use least-recently-used order.

If the time between consecutive invocations is smaller than
`switchy-delay' seconds, selects one after the other window in
LRU order and cycles when all windows have been visited.  If
`switchy-delay' has passed, the current switching cycle ends and
the now selected window gets its tick updated (a kind of
timestamp)."
  (interactive)

  (unless switchy-minor-mode
    (user-error "switchy-window requires `switchy-minor-mode' being active"))

  ;; Remove dead windows.
  (setq switchy--tick-alist (seq-filter
                             (lambda (e)
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
                                           (if (and x (funcall (if arg #'< #'>)
                                                               (cdr x) (cdr e)))
                                               x
                                             e))
                                         win-entries nil))))

          (progn
            (setq switchy--visited-windows (cons win switchy--visited-windows))
            (select-window win)))
      ;; Start a new cycle if we're not at the start already, i.e., we visited
      ;; just one (the current) window.
      (when (> (length switchy--visited-windows) 1)
        (setq switchy--visited-windows nil)
        (switchy-window)))))

(defvar switchy-minor-mode-map (make-sparse-keymap)
  "The mode map of `switchy-minor-mode'.
No keys are bound by default.  Bind the main command
`switchy-window' to a key of your liking, e.g.,

  ;; That\\='s what I use.
  (keymap-set switchy-minor-mode-map \"C-<\" #\\='switchy-window)

  ;; Or as a substitute for `other-window'.
  (add-hook \\='switchy-minor-mode-hook
            (lambda ()
              (if switchy-minor-mode
                  (keymap-global-set \"<remap> <other-window>\"
                                     #\\='switchy-window)
                (keymap-global-unset \"<remap> <other-window>\"))))")

(define-minor-mode switchy-minor-mode
  "Activates recording of window selection ticks, i.e., timestamps
for figuring out the last-recently-used order of windows.

It uses the keymap `switchy-minor-mode-map', which see."
  :global t
  :keymap switchy-minor-mode-map
  (if switchy-minor-mode
      (add-hook 'window-selection-change-functions
                #'switchy--on-window-selection-change)
    (remove-hook 'window-selection-change-functions
                 #'switchy--on-window-selection-change)))

(provide 'switchy)
