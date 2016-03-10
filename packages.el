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
    (setq magit-repository-directories (quote ("~/burtcorp" "~/development" "~/development/go/src/github.com/jowl")))
    (setq magit-repository-directiries-depth 1)
    (setq magit-set-upstream-on-push t)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t)
    (setq magit-status-buffer-switch-function (quote switch-to-buffer))
    (setq magit-diff-options '("--histogram"))
    (setq magit-log-arguments '("--graph" "--color" "--decorate"))
    (setq magit-branch-arguments nil)
    (setq magit-push-always-verify nil)
    (setq magit-revert-buffers t)
    (add-hook 'magit-mode-hook (lambda () (if (f-file? (f-expand "Gemfile" default-directory)) (rspec-mode)))))
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame)))

(use-package projectile
  :init (projectile-global-mode))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (progn
            (setq company-idle-delay 0.3)
            (setq company-show-numbers 1)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-?" . mc/mark-all-dwim)
   ("M-/" . mc/edit-lines)))

(use-package yasnippet
  :init (yas-global-mode)
  :config (setq yas-snippet-dirs (list (f-expand "snippets" user-emacs-directory))))

(use-package smartparens-config
  :init
  (progn
    (use-package smartparens-html)
    (use-package smartparens-ruby)
    (use-package smartparens-scala)
    (use-package smartparens-latex)
    (use-package smartparens-haskell)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
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
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)))

(use-package html-mode)
(use-package discover)

(use-package prodigy
  :init (load-local "prodigy-services")
  :bind (("C-x p" . prodigy)))

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
                   (defun -rspec-compilation-buffer ()
                     (interactive)
                     (set-window-buffer (selected-window) (get-buffer "*rspec-compilation*")))
                   (advice-add 'rspec-compile :around '-rspec-compile)
                   (setq rspec-spec-command "rbenv exec bundle exec rspec")
                   (setq rspec-use-bundler-when-possible nil)
                   (setq rspec-use-zeus-when-possible nil)
                   (setq rspec-use-rake-when-possible nil)
                   (setq rspec-use-spring-when-possible nil)
                   (add-hook 'compilation-mode-hook
                             (lambda ()
                               (when (eq major-mode 'rspec-compilation-mode)
                                 (setq compilation-scroll-output t)
                                 (local-set-key (kbd "g") (lambda ()
                                                            (interactive)
                                                            (rspec-rerun)))))))
                 :bind (("C-c , g" . -rspec-compilation-buffer))))
  :config (progn
            (setq ruby-deep-indent-paren nil)
            (add-hook 'dired-mode-hook 'rspec-dired-mode)
            (add-hook 'ruby-mode-hook (lambda ()
                                        (bind-key ", e" '-rspec-verify-single-example rspec-verifiable-mode-keymap)
                                        (bind-key ", E" '-rspec-verify-example rspec-verifiable-mode-keymap)))
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

(use-package ibuffer-vc
  :init (ibuffer-vc-set-filter-groups-by-vc-root)
  :bind ("C-x C-b" . ibuffer))

(use-package haskell-mode
  :init (progn (add-hook 'haskell-mode-hook 'haskell-indent-mode)
               (add-hook 'haskell-mode-hook 'interactive-haskell-mode)))

(use-package smart-mode-line
  :init (progn
          (custom-set-variables '(custom-safe-themes '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
          (sml/setup)))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package rainbow-mode
  :config  (setq rainbow-x-colors nil))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config (setq-default js2-basic-offset 2))

(use-package js-mode
  :config (setq js-indent-level 2))

(use-package sh-script
  :config (progn
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)))

(use-package coffee-mode
  :config
  (progn
    (add-hook 'coffee-mode-hook
              (lambda ()
                (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
                (bind-key "C-M-h" 'backward-kill-word coffee-mode-map)
                (setq coffee-tab-width 2)))))

(use-package css-mode
  :mode (("\\.s?css$" . css-mode))
  :config (setq css-indent-offset 2))

(use-package web-mode
  :config (progn
            (setq web-mode-enable-auto-pairing nil)
            (setq web-mode-enable-css-colorization t)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (defun sp-web-mode-is-code-context (id action context)
              (and (eq action 'insert)
                   (not (or (get-text-property (point) 'part-side)
                            (get-text-property (point) 'block-side)))))
            (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))
