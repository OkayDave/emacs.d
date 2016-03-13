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
(require 'indent-guide)

(column-number-mode t)

(add-hook 'prog-mode-hook 'linum-mode)

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

(indent-guide-global-mode)
(setq indent-guide-recursive t)

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
(setq-default indent-tabs-mode nil)

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
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js.jsx\\'" . web-mode))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization nil)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-markup-indent-offset 0)
(setq web-mode-css-indent-offset 0)
(setq web-mode-code-indent-offset 0)

(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))


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
(sp-local-pair 'web-mode "<" ">" :actions nil)

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
	(balance-windows)
  (evil-window-left 1)
	)

(add-hook 'term-mode (lambda ()
                       (interactive)
                       (message "term mode term mode term mode")
                       (global-linum-mode 0)
                       ))

;;;; ruby 
(require 'ruby-test-mode)
(require 'rvm)

(rvm-use-default)

(add-hook 'ruby-mode 'rvm-activate-corresponding-ruby)
; (add-hook 'ruby-mode-hook 'robe-mode)

;(eval-after-load 'company
;  '(push 'company-robe company-backends))

 (add-hook 'inf-ruby-mode-hook (lambda () (require 'inf-ruby-company)))

;;;; elixir

;;;; yaml
(require 'yaml-mode)

(add-hook 'yaml-mode-hook
  (lambda ()
    (evil-local-mode 1)
    (indent-guide-mode 1)
    ))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  
;;;; dimensions
(when window-system (set-frame-size (selected-frame) 275 108))

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

;;;; org-mode
(require 'org)
(global-font-lock-mode 1)


(defun open-org()
	(interactive)
	(evil-window-vsplit)
  (evil-window-right 1)
	(shell-command "touch ~/agenda.org")
	(find-file "~/agenda.org")
	)

;;;; flycheck
(require 'flycheck)

(setq flycheck-indication-mode 'left-fringe)
(global-flycheck-mode)

;;;; windows

(defun my-window-open-right ()
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1))

(defun my-window-open-down ()
  (interactive)
  (evil-window-split)
  (evil-window-down 1))


(defun my-window-left ()
  (interactive)
  (evil-window-left 1)
  )

(defun my-window-right ()
  (interactive)
   (evil-window-right 1)
   )

(defun my-window-up ()
  (interactive)
   (evil-window-up 1)
   )

(defun my-window-down ()
  (interactive)
  (evil-window-down 1)
  )


;;;; company-mode

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)

;;;; neotree
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)

;;;; diminish
(diminish 'projectile-mode)
(diminish 'rainbow-mode)
(diminish 'smartparens-mode)
(diminish 'undo-tree-mode)
(diminish 'flycheck-mode)
(diminish 'indent-guide-mode)
(diminish 'git-gutter-mode)


;;;; Leader bindings
(evil-leader/set-key
  ;; emacs/lisp
	"[ ]" 'eval-buffer
	"[ j" 'eval-break-print
	"[ '" 'eval-last-sexp
	"[ s" 'open-scratch
	
	;; project
	"p f" 'helm-projectile
	"p s" 'helm-projectile-grep
	"p t" 'open-project-terms
  "p q" 'helm-projectile-switch-project
  "p o" 'neotree-toggle

	;; toggles
	"t n" 'rainbow-delimiters-mode
	"t s" 'smartparens-mode
	"t c" 'column-highlight-mode
	"t r" 'global-hl-line-mode
	"t l" 'linum-relative-toggle
	"t q" 'rainbow-mode
  "t g" 'git-gutter:toggle

	;; features
	"c i" 'evilnc-comment-or-uncomment-lines
	"<RET>" 'multi-term

	;; org-mode
	"o a" 'org-agenda
	"o l" 'org-store-link
	"o o" 'open-org

  ;; windows
  "w j" 'my-window-open-down
  "w l" 'my-window-open-right
  "w k" 'evil-window-delete
  "w >" 'evil-window-rotate-upwards
  "w <" 'evil-window-rotate-downwards
  "<left>" 'my-window-left
  "<right>" 'my-window-right
  "<up>"    'my-window-up
  "<down>"  'my-window-down

	;; ruby
	"r t f" 'ruby-test-run
	"r t p" 'ruby-test-run-at-point
	"r t t" 'ruby-test-toggle-implementation-and-specification

  ;; elixir

  "e x"   'alchemist-mix
  "e c" 'alchemist-mix-compile
  "e r" 'alchemist-mix-run
  "e t" 'alchemist-mix-test
  "e f" 'alchemist-mix-test-file
  "e s" 'alchemist-test-toggle-test-report-display

	)

(evil-leader/set-key "e e" (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

(provide 'init)
;;; init.el ends here
