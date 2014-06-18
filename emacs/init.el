;; Using Cask for Package Management
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq ring-bell-function 'ignore)
(setq default-directory "~/")

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(set-frame-font "Source Code Pro for Powerline-14")

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(global-visual-line-mode t)
(global-linum-mode t)
(global-hl-line-mode 1)

(delete-selection-mode t)
(blink-cursor-mode t)
(show-paren-mode t)

(setq make-backup-file nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)

(when (window-system)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
