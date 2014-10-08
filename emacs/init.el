;; Using Cask for Package Management
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ==================================================
;;                 Basic Settings
;; ==================================================

(set-frame-font "Source Code Pro for Powerline-14")
(setq mac-command-modifier 'meta)
(setq ring-bell-function 'ignore)
(setq default-directory "~/")

(global-auto-revert-mode 1)
(global-visual-line-mode t)
(delete-selection-mode t)
(blink-cursor-mode t)
(show-paren-mode 1)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq make-backup-file nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(electric-indent-mode t)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(when (window-system)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
  (tooltip-mode -1)
  (set-fringe-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; ==================================================
;;               AUTO MODES
;; ==================================================

;; Ruby
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru\\'" . ruby-mode))

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))

;; Emmet
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; ORG mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; ==================================================
;;             PEDRO MODE MAPPINGS
;; ==================================================

;; Define my own keymap
(defvar pedro-mode-map (make-keymap) "my keys")

;; Change C-x with C-n and C-c with C-i on Colemak layout
(keyboard-translate ?\C-j ?\C-x)
(keyboard-translate ?\C-x ?\C-j)
(keyboard-translate ?\C-i ?\C-c)
(keyboard-translate ?\C-c ?\C-i)

;; CUSTOM FUNCTIONS
(define-key pedro-mode-map (kbd "C-c y") 'duplicate-current-line-or-region)
(define-key pedro-mode-map (kbd "C-c r") 'rename-this-buffer-and-file)
(define-key pedro-mode-map (kbd "C-l") 'comment-or-uncomment-line-or-region)
(define-key pedro-mode-map (kbd "M-n") 'delete-indentation)
(define-key pedro-mode-map (kbd "M-s") 'search-selection)


;; ==================================================
;;             GLOBAL MAPPINGS
;; ==================================================

;; CUSTOM FUNCTIONS
(global-set-key [remap kill-region] 'cut-line-or-region)
(global-set-key [remap kill-ring-save] 'copy-line-or-region)
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-set-key [(ctrl tab)] 'other-window)

;; ==================================================
;;              PLUGINS and PACKAGES
;; ==================================================

;; EXPAND REGION
(define-key pedro-mode-map (kbd "C-o") 'er/expand-region)

;; ACE JUMP MODE
(define-key pedro-mode-map (kbd "C-;") 'ace-jump-mode)

;; FOLD DWIM
(require 'fold-dwim)
(define-key pedro-mode-map (kbd "C-c f") 'fold-dwim-toggle-selective-display)

;; DIRED SETTINGS
(require 'dired)
(setq dired-recursive-deletes (quote top))
(define-key dired-mode-map (kbd "f") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^") (lambda ()
                                       (interactive)
                                       (find-alternate-file "..")))
(put 'dired-find-alternate-file 'disabled nil)

;; IDO MODE
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(ido-vertical-mode 1)

;; SAVEPLACE
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "saveplace.el"))
(setq-default save-place t)

;; AUTO-COMPLETE
(require 'auto-complete-config)
(ac-config-default)

;; YASNIPPET
(yas-global-mode t)

;; JS MODE
(setq js-indent-level 2)

;; PROJECTILE
(projectile-global-mode)

;; PROJECTILE and HELM
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "C-c b") 'helm-buffers-list)

;; MAGIT status
(define-key pedro-mode-map (kbd "C-c s")  'magit-status)
(define-key pedro-mode-map (kbd "C-c l")  'magit-log)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; MULTIPLE CURSORS
(define-key pedro-mode-map (kbd "C-,") 'mc/mark-previous-like-this)
(define-key pedro-mode-map (kbd "C-.") 'mc/mark-next-like-this)

;; Ag - the silver searcher
(setq ag-highlight-search 't)

;; ==================================================
;;              CUSTOM FUNCTIONS
;; ==================================================

(defun cut-line-or-region()
  "Kill current line if no region is active, otherwise kills region."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun copy-line-or-region()
  "Copy current line if no region is active, otherwise copies region."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        ;;(beginning-of-visual-line)
        (insert region)
        (indent-according-to-mode)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name
                        (file-name-nondirectory new-name))))))))

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )

(defun search-selection (beg end)
  "search for selected text"
  (interactive "r")
  (let (
        (selection (buffer-substring-no-properties beg end))
        )
    (deactivate-mark)
    (isearch-mode t nil nil nil)
    (isearch-yank-string selection)
    )
  )

;; ==================================================

;; Define my own minor mode and activate it
(define-minor-mode pedro-mode
  "A minor mode for my custom keys and functions"
  t " pedro" 'pedro-mode-map)
(pedro-mode t)

;; ==================================================
;;               APPEARENCE
;; ==================================================

(custom-set-faces
 '(show-paren-match ((t (:background "red"))))
 '(mode-line-buffer-id ((t (:inherit bold))))
 )

(load-theme 'solarized-dark t)

(powerline-default-theme)

(set-face-attribute 'mode-line nil
                    :background "DarkSlateGray"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)

