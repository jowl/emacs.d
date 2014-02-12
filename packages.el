(use-package ansi-color)

(use-package ido
  :init (ido-mode 1)
  :config (setq ido-enable-flex-matching 1)
)

(use-package ack-and-a-half)

(use-package magit
  :init (use-package evm)
  :config
  (progn
    (setq magit-emacsclient-executable (evm-emacsclient))
    (setq magit-completing-read-function (quote magit-ido-completing-read))
    (setq magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
    (setq magit-highlight-indentation nil)
    (setq magit-highlight-trailing-whitespace 1)
    (setq magit-process-popup-time -1)
    (setq magit-repo-dirs (quote ("~/burtcorp" "~/development")))
    (setq magit-repo-dirs-depth 1)
    (setq magit-set-upstream-on-push 1)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-status-buffer-switch-function (quote switch-to-buffer))
    (add-hook 'magit-mode-hook (lambda () (bind-key "C-c , a" 'rspec-verify-all))))
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame-mode)))

(use-package projectile
  :init (projectile-global-mode)
  :config (progn 
	    (setq projectile-require-project-root nil)
	    (defun projectile-completion-fn (prompt choises)
	      "Projectile completion function that only shows file name.

               If two files have same name, new completion appears to select between
               them. These include the path relative to the project root."
	      (interactive)
	      (let* ((stripped-choises
		      (-uniq (--map (file-name-nondirectory it) choises)))
		     (choise
		      (ido-completing-read prompt stripped-choises))
		     (matching-files
		      (-filter
		       (lambda (file)
			 (equal (file-name-nondirectory file) choise))
		       choises)))
		(if (> (length matching-files) 1)
		    (ido-completing-read prompt matching-files)
		  (car matching-files))))

	    (setq projectile-completion-system 'projectile-completion-fn)))

(use-package powerline
  :init (powerline-default-theme)
  :config
  (progn
    (setq powerline-default-separator nil)
    (setq powerline-default-separator-dir (quote (left . right)))))

(use-package auto-complete
  :init 
  (progn
    (use-package auto-complete-config)
    (ac-config-default))
  :config
  (progn
    (setq ac-use-fuzzy 1)
    (setq ac-ignore-case nil)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-?" . mc/edit-beginnings-of-lines)
  ("C-c C-<" . mc/mark-all-like-this)))

(use-package yasnippet
  :init (yas-global-mode)
  :config (setq yas-snippet-dirs (list (f-expand "snippets" user-emacs-directory))))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (smartparens-global-mode 1)
    (setq sp-autoescape-string-quote nil))
  :bind
  (("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)))

(use-package discover)

(use-package prodigy)

(use-package web-mode)

(use-package rvm
  :init (rvm-use-default))

(use-package ruby-mode
  :init
  (progn
    (use-package rspec-mode
                 :config
                 (progn
                   (setq rspec-use-rvm 1)
                   (setq rspec-use-rake-flag nil)
                   (setq rspec-spec-command "JRUBY_OPTS=\"--1.9 --debug\" rspec")
                   (setq rspec-use-bundler-when-possible nil)
		   (defadvice rspec-compile (around rspec-compile-around activate)
		     "Use BASH shell for running the specs because of ZSH issues."
		     (let ((shell-file-name "/bin/bash"))
		       ad-do-it))
                   (add-hook 'compilation-mode-hook
                             (lambda ()
                               (local-set-key (kbd "g") (lambda ()
                                                          (interactive)
                                                          (when (eq major-mode 'rspec-compilation-mode)
                                                            (rspec-rerun)))))))))
  :config (progn
            (setq ruby-deep-indent-paren nil)
            (add-hook 'ruby-mode-hook 'rspec-mode)
            (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))
