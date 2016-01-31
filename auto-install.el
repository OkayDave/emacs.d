;;; From: http://stackoverflow.com/a/10093312
(setq package-list '(
	async
	col-highlight
	dash
	dtrt-indent
	epl
	evil
	evil-args
	evil-leader
	evil-matchit
	evil-nerd-commenter
	evil-smartparens
	evil-surround
	exec-path-from-shell
	flatland-theme
	flx
	flx-ido
	fringe-helper
	git-commit
	git-gutter
	git-gutter-fringe
	goto-chg
	helm
	helm-core
	helm-projectile
	linum-relative
	multi-term
	org
	pkg-info
	powerline
	powerline-evil
	projectile
	rainbow-delimiters
	rainbow-mode
	relative-line-numbers
	ruby-block
	ruby-tools
	ruby-test-mode
	smartparens
	sublimity
	undo-tree
	vline
	web-mode
	with-editor))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
												 ("melpa" . "https://melpa.org/packages/")
			                   ("org" . "http://orgmode.org/elpa/")))


; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


