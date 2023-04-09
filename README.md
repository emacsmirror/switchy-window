# switchy.el: A last-recently-used window switcher for Emacs

Switchy is a last-recently-used window switcher.  It suits my personal Emacs
layout and workflow where I usually have at most two editing windows but up
to three side-windows which I have to select only seldomly.

The idea of switchy is simple: when you invoke `switchy-window` in quick
succession, it will switch to one window after the other in
last-recently-used order.  Once you stop switching for long enough time
(`switchy-delay`, 1.5 seconds by default), the selected window gets locked
in, i.e., its LRU timestamp is updated and this switching sequence is ended.
Thusly, you can toggle between two windows simply by invoking
`switchy-window`, waiting at least `switchy-delay`, and then invoking
`switchy-window` again to switch back to the original window.


## Usage

Activate `switchy-minor-mode` which tracks window changes and bind
`switchy-window` to a key of your liking in `switchy-minor-mode-map` or
globally.  Here are is a sample configuration:

```elisp
(switchy-minor-mode)

;; That's what I use.
(keymap-set switchy-minor-mode-map "C-<" #'switchy-window)

;; Or as a substitute for `other-window'.
(add-hook 'switchy-minor-mode-hook
          (lambda ()
            (if switchy-minor-mode
                (keymap-global-set "<remap> <other-window>"
                                   #'switchy-window)
              (keymap-global-unset "<remap> <other-window>"))))
```

**Hint**: Since the order of window switching is not as obvious as it is with
`other-window`, adding a bit visual feedback to window selection changes can be
helpful.  That can be done easily with the stock Emacs `pulse.el`, e.g.:

```elisp
(add-hook 'window-selection-change-functions
          (lambda (frame)
            (when (eq frame (selected-frame))
              (pulse-momentary-highlight-one-line))))
```

## Installation

TODO: Will probably be available from GNU ELPA soon.

## Questions & Patches

For asking questions, sending feedback, or patches, refer to [my public inbox
(mailinglist)](https://lists.sr.ht/~tsdh/public-inbox).  Please mention the
project you are referring to in the subject.

## Bugs

Bugs and requests can be reported [here](https://todo.sr.ht/~tsdh/switchy).

## License

`switchy.el` is licensed under the
[GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) (or later).
