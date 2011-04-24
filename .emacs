(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")

; Some theme I'm testing
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-billw)

; Slightly smaller than regular
(set-face-attribute 'default (selected-frame) :height 90)

; Turn off annoying things
(menu-bar-mode 0)
(tool-bar-mode 0)

(blink-cursor-mode 0)
(column-number-mode 1)
(scroll-bar-mode 0)

