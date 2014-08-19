;; Using Cask for Package Management
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; ==================================================
;;                 Basic Settings
;; ==================================================

(set-frame-font "Source Code Pro for Powerline-14")
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq ring-bell-function 'ignore)
(setq default-directory "~/")

(global-auto-revert-mode 1)
(global-linum-mode -1)
(global-hl-line-mode -1)
(global-visual-line-mode t)
(delete-selection-mode t)
(blink-cursor-mode t)

;; show parenthesis match
(show-paren-mode 1)
;; (setq show-paren-style 'expression)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq make-backup-file nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent nil)
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
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)

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

;; WINDMOVE
(define-key pedro-mode-map (kbd "M-[")  'other-window)
(define-key pedro-mode-map (kbd "M-p")  'other-window)

;; EXPAND REGION
(define-key pedro-mode-map (kbd "C-o") 'er/expand-region)

;; ACE JUMP MODE
(define-key pedro-mode-map (kbd "M-o") 'ace-jump-mode)
(define-key pedro-mode-map (kbd "C-u SPC") 'ace-jump-char-mode)

;; CUSTOM FUNCTIONS
(define-key pedro-mode-map (kbd "<C-return>") 'open-line-above)
(define-key pedro-mode-map (kbd "M-RET") 'open-line-below)
(define-key pedro-mode-map (kbd "C-c y") 'duplicate-current-line-or-region)
(define-key pedro-mode-map (kbd "C-c r") 'rename-this-buffer-and-file)
(define-key pedro-mode-map (kbd "C-l") 'comment-or-uncomment-line-or-region)
(define-key pedro-mode-map (kbd "C-;") 'select-current-line)
(define-key pedro-mode-map (kbd "C-u k") 'dired-kill-subdir)
(define-key pedro-mode-map (kbd "M-n") 'delete-indentation)
(define-key pedro-mode-map (kbd "M-s") 'search-selection)
(define-key pedro-mode-map (kbd "<C-M-down>") 'move-line-down)
(define-key pedro-mode-map (kbd "<C-M-up>") 'move-line-up)

;; ==================================================
;;             GLOBAL MAPPINGS
;; ==================================================

;; CUSTOM FUNCTIONS
(global-set-key [remap kill-region] 'cut-line-or-region)
(global-set-key [remap kill-ring-save] 'copy-line-or-region)
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

;; ==================================================
;;              PLUGINS and PACKAGES
;; ==================================================

;; Fill Column Indicator
(setq-default fci-rule-column 100)
  (setq fci-handle-truncate-lines nil)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda ()
                                                         (fci-mode 1)))
  (global-fci-mode 1)
  (defun auto-fci-mode (&optional unused)
    (if (> (window-width) fci-rule-column)
        (fci-mode 1)
      (fci-mode 0))
    )
  (add-hook 'after-change-major-mode-hook 'auto-fci-mode)
(add-hook 'window-configuration-change-hook 'auto-fci-mode)

;; FOLD DWIM
(require 'fold-dwim)
(define-key pedro-mode-map (kbd "C-M-y") 'fold-dwim-toggle-selective-display)

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

;; MAGIT status
(define-key pedro-mode-map (kbd "C-c g")  'magit-status)

;; MULTIPLE CURSORS
(define-key pedro-mode-map (kbd "C->") 'mc/mark-next-like-this)
(define-key pedro-mode-map (kbd "C-<") 'mc/mark-previous-like-this)

;; ==================================================
;;              CUSTOM FUNCTIONS
;; ==================================================

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; GOTO LINE M-g g
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun select-current-line ()
  "Selects the current line"
  (interactive)
  (end-of-line)
  (push-mark (line-beginning-position) nil t))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

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

;; Shift the selected region right if distance is postive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; ==================================================

;; Define my own minor mode and activate it
(define-minor-mode pedro-mode
  "A minor mode for my custom keys and functions"
  t " pedro" 'pedro-mode-map)
(pedro-mode t)

;; ==================================================
;;               APPEARENCE
;; ==================================================
(powerline-default-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526"
                            "#ff4b4b"
                            "#b4fa70"
                            "#fce94f"
                            "#729fcf"
                            "#ad7fa8"
                            "#8cc4ff"
                            "#eeeeec"])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes (quote
                       ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)
                       )))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
