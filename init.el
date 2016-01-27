;;;; MELPA config
(require 'package)
(setq auto-install (expand-file-name "auto-install.el" user-emacs-directory))
(load auto-install)

;;;; Evil config
(require 'evil)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-nerd-commenter)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(evil-mode 1)

(global-evil-matchit-mode 1) ;; % to jump

;; set cursor colour on mode
(setq evil-emacs-state-cursor '("#CC6666" box))
(setq evil-normal-state-cursor '("#66CC66" box))
(setq evil-visual-state-cursor '("#CC6600" box))
(setq evil-insert-state-cursor '("#CC6666" bar))
(setq evil-replace-state-cursor '("#CC6666" bar))
(setq evil-operator-state-cursor '("#CC6666" hollow))

;;;; Project files
(require 'projectile)
(require 'helm-config)
(require 'helm-misc)
(require 'helm-projectile)
(require 'helm-locate)

(projectile-global-mode)

(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-autoresize-mode t)

(global-set-key (kbd "M-x") 'helm-M-x)

(defun helm-my-buffers ()
  (interactive)
  (let (helm-ff-transformer-show-only-basename nil))
  (helm-other-buffer '(helm-c-source-buffers-list
		       helm-c-source-elscreen
		       helm-c-source-projectile-files-list
		       helm-c-source-ctags
		       helm-c-source-recentf
		       helm-c-source-locate)
		     "*helm-my-buffers*"))


;;;; Colours
(load-theme 'flatland t)
(rainbow-delimiters-mode)


;;;; UI & Behaviour config

(require 'powerline)
(require 'paren)

(global-linum-mode t)
(column-number-mode t)

(tool-bar-mode -1)      ;; disable tool bar
(scroll-bar-mode -1)
(set-fringe-mode 1)

(setq visible-bell t)
(setq-default left-fringe-width 10)
(setq-default right-fringe-width 0)

(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(powerline-center-evil-theme)

(setq linum-format "%4d ")

(defun hide-fringe ()
  (interactive)
	(setq fringe-colour (face-background 'default))
	(custom-set-faces
	 `(fringe ((t (:background ,fringe-colour)))))
	)

(add-hook 'after-init-hook #'hide-fringe)

(defalias 'yes-or-no-p 'y-or-n-p)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit))
	)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)


(setq-default tab-width 2)
(setq-default tabs-mode nil)

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-hl-line-mode)

(show-paren-mode 1)
(set-face-background 'show-paren-match "#CCCC66")
(set-face-foreground 'show-paren-match "#000000")
(set-face-attribute  'show-paren-match nil :weight 'ultra-bold)

;;;; completeion

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flx-matching 1)
(setq ido-use-faces nil)

;;;; emacs-set config

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

;;;; git-gutter-fringe
(require 'git-gutter-fringe)
(set-face-foreground 'git-gutter-fr:added "#66CC66")
(set-face-foreground 'git-gutter-fr:deleted "#CC6666")
(set-face-foreground 'git-gutter-fr:modified "#6666CC")
(git-gutter)

;;;; smartparens
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)
(smartparens-mode)

;;;; rainbow-mode
(rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

;;;; multiterm
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(defun open-project-terms ()
	(interactive)
  (evil-window-vsplit)
	(evil-window-right 1)
	(multi-term)
	(evil-window-split)
	(evil-window-down 1)
	(multi-term)
	(evil-window-split)
	(evil-window-down 1)
	(multi-term)
	(evil-window-split)
	(evil-window-down 1)
	(multi-term)
	(balance-windows)
  (evil-window-left 1)
	)

;;;; ruby mode
(require 'ruby-block)

(ruby-block-mode t)

;;;; dimensions
(when window-system (set-frame-size (selected-frame) 275 120))

;;; emacs/lisp things

(defun eval-break-print ()
  (interactive)
	(forward-char)
	(eval-print-last-sexp)
	)

(defun eval-last-sexp-shunt ()
  (interactive)
	(forward-char)
	(eval-last-sexp)
	)

(defun open-scratch()
	(interactive)
	(evil-window-vsplit)
	(evil-window-right 1)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(lisp-interaction-mode)
	)


;;;; Leader bindings
(evil-leader/set-key
  ;; emacs/lisp
	"[ ]" 'eval-buffer
	"[ j" 'eval-break-print
	"[ '" 'eval-last-sexp
	"[ s" 'open-scratch
	
	;; projectile
	"p f" 'helm-projectile
	"p s" 'helm-projectile-grep
	"p t" 'open-project-terms
  "g g" 'git-gutter:toggle

	;; toggles
	"t n" 'rainbow-delimiters-mode
	"t s" 'smartparens-mode
	"t c" 'column-highlight-mode
	"t r" 'global-hl-line-mode
	"t l" 'linum-relative-toggle
	"t q" 'rainbow-mode

	;; features
	"c i" 'evilnc-comment-or-uncomment-lines
	"<RET>" 'multi-term

	)

(evil-leader/set-key "e e" (lambda() (interactive)(find-file "~/.emacs.d/init.el")))
