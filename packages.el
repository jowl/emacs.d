(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package ansi-color)

(use-package ido
  :init (ido-mode 1)
  :config (progn
	    (setq ido-enable-flex-matching 1)
	    (ido-everywhere 1)))

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config (setq ido-use-faces nil))

(use-package ack-and-a-half)

(use-package magit
  :init (progn
          (use-package magit-blame)
          (use-package evm))
  :config
  (progn
    (setq magit-emacsclient-executable (evm-emacsclient))
    (setq magit-completing-read-function (quote magit-ido-completing-read))
    (setq magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
    (setq magit-highlight-indentation nil)
    (setq magit-highlight-trailing-whitespace 1)
    (setq magit-process-popup-time -1)
    (setq magit-repo-dirs (quote ("~/burtcorp" "~/development" "~/development/go/src/github.com/jowl")))
    (setq magit-repo-dirs-depth 1)
    (setq magit-set-upstream-on-push t)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t)
    (setq magit-status-buffer-switch-function (quote switch-to-buffer))
    (setq magit-diff-options (quote ("--histogram")))
    (add-hook 'magit-mode-hook (lambda () (if (f-file? (f-expand "Gemfile" default-directory)) (rspec-mode)))))
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame-mode)))

(use-package projectile
  :init (projectile-global-mode)
  :config (progn
            (add-hook 'projectile-mode-hook (lambda () (bind-key "C-c p a" 'ack-wrap projectile-mode-map)))))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (progn
            (setq company-idle-delay 0.3)
            (setq company-show-numbers 1)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-?" . mc/mark-all-dwim)))

(use-package yasnippet
  :init (yas-global-mode)
  :config (setq yas-snippet-dirs (list (f-expand "snippets" user-emacs-directory))))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (smartparens-global-mode t)
    (setq sp-autoescape-string-quote nil)
    (show-smartparens-global-mode t))
  :bind
  (("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("C-)"   . sp-forward-slurp-sexp)
   ("C-("   . sp-backward-slurp-sexp)
   ("C-}"   . sp-forward-barf-sexp)
   ("C-{"   . sp-backward-barf-sexp)
   ;; navigation
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("M-f"   . sp-forward-symbol)
   ("M-b"   . sp-backward-symbol)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)))

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
                   (defun -rspec-compile (orig target &optional opts)
                     (interactive "P")
                     (let ((opts (if current-prefix-arg
                                     (cons "--fail-fast" opts)
                                   opts))
                           (shell-command-switch "-lc"))
                       (apply orig (list target opts))))
                   (advice-add 'rspec-compile :around '-rspec-compile)
                   (setq rspec-spec-command "ng-rspec")
                   (setq rspec-use-bundler-when-possible nil)
                   (setq rspec-use-zeus-when-possible nil)
                   (setq rspec-use-rake-when-possible nil)
                   (setq rspec-use-spring-when-possible nil)
                   (add-hook 'compilation-mode-hook
                             (lambda ()
                               (when (eq major-mode 'rspec-compilation-mode)
                                 (local-set-key (kbd "g") (lambda ()
                                                            (interactive)
                                                            (rspec-rerun)))))))))
  :config (progn
            (setq ruby-deep-indent-paren nil)
            (add-hook 'ruby-mode-hook 'rspec-mode)
            (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
            (bind-key "C-c , e" '-rspec-verify-single-example rspec-mode-map)
            (bind-key "C-c , E" '-rspec-verify-example rspec-mode-map)
            (setq ruby-align-to-stmt-keywords '(begin if while unless until case for)))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package gist)

(use-package multi-term
  :init (setq multi-term-program "/bin/zsh"))

(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)))

(use-package sticky-windows
  :bind (
         ("C-x 0" . sticky-window-delete-window)
         ("C-x 1" . sticky-window-delete-other-windows)
         ("C-x 9" . sticky-window-keep-window-visible)))

(use-package ibuffer-vc
  :init (ibuffer-vc-set-filter-groups-by-vc-root)
  :bind ("C-x C-b" . ibuffer))

(use-package haskell-mode
  :init (progn (add-hook 'haskell-mode-hook 'haskell-indent-mode)
               (add-hook 'haskell-mode-hook 'interactive-haskell-mode)))

(use-package smart-mode-line
  :init (progn
          (custom-set-variables '(custom-safe-themes '("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
          (sml/setup)))
