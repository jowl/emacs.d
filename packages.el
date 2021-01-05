(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package ansi-color
  :config (progn
            (defun -colorize-compilation-buffer ()
              (interactive)
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region (point-min) (point-max))))))

(use-package magit
  :init (use-package magit-blame)
  :config
  (progn
    (setq magit-completing-read-function 'ivy-completing-read)
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-highlight-indentation nil)
    (setq magit-highlight-trailing-whitespace 1)
    (setq magit-process-popup-time -1)
    (setq magit-repository-directories '(("~/burtcorp" . 1) ("~/development" . 1) ("~/development/go/src/github.com/jowl" . 1) ("~/inovia" . 4)))
    (setq magit-set-upstream-on-push t)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t)
    (setq magit-push-always-verify nil)
    (setq magit-revert-buffers t)
    (setq magit-diff-refine-hunk t)
    (add-hook 'magit-mode-hook (lambda () (if (f-file? (f-expand "Gemfile" default-directory)) (rspec-mode)))))
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame)))

(use-package projectile
  :init (progn
          (projectile-mode +1)
          (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
  :config (setq projectile-completion-system 'ivy))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (progn
            (setq company-idle-delay 0.3)
            (setq company-show-numbers 1)
            (setq company-dabbrev-downcase nil))
  :bind
  (("C-<tab>" . company-complete)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-?" . mc/mark-all-dwim)
   ("M-/" . mc/edit-lines)))

(use-package yasnippet
  :init (progn
    (add-hook 'after-save-hook
              (lambda ()
                (when (eql major-mode 'snippet-mode)
                  (yas-reload-all))))
    (yas-global-mode))
  :config (setq yas-snippet-dirs (list (f-expand "snippets" user-emacs-directory)))
  :mode ("\\.yasnippet" . snippet-mode))

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
            (setq ruby-insert-encoding-magic-comment nil)
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
          (custom-set-variables
           '(custom-safe-themes '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "62f68a0b49cf383478041c688cc1b82f084f76b84a2ab2819a4ed9ceb59aefd8" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
          (sml/setup)))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package rainbow-mode
  :config  (setq rainbow-x-colors nil))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config (progn
            (setq-default js2-basic-offset 2)))

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
  :mode ("\\.html?$" . web-mode)
  :config (progn
            (setq web-mode-enable-auto-pairing t)
            (setq web-mode-enable-css-colorization t)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-enable-current-element-highlight t)
            (defun sp-web-mode-is-code-context (id action context)
              (and (eq action 'insert)
                   (not (or (get-text-property (point) 'part-side)
                            (get-text-property (point) 'block-side)))))
            (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))

(use-package git-gutter-fringe
  :if window-system
  :demand
  :init (global-git-gutter-mode t)
  :config (progn
            (fringe-helper-define 'git-gutter-fr:modified nil
              "........"
              "...XX..."
              "..XXXX.."
              ".XXXXXX."
              ".XXXXXX."
              "..XXXX.."
              "...XX..."
              "........"))
  :bind (("C-x g g" . git-gutter)
         ("C-x g n" . git-gutter:next-hunk)
         ("C-x g p" . git-gutter:previous-hunk)
         ("C-x g s" . git-gutter:stage-hunk)
         ("C-x g v" . git-gutter:revert-hunk)
         ("C-x g TAB" . git-gutter:popup-hunk)
         ("C-x g SPC" . git-gutter:mark-hunk)))

(use-package swiper
  :config (progn
            (ivy-mode 1)
            (setq ivy-initial-inputs-alist nil)
            (bind-key "C-r" 'ivy-previous-line ivy-minibuffer-map))
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c C-r" . ivy-resume)))

(use-package yafolding
  :config (progn
            (mapc
             (lambda (hook)
               (add-hook hook (lambda ()
                                (yafolding-mode t))))
             '(json-mode-hook yaml-mode-hook)))
  :bind (("C-c y e" . yafolding-toggle-element)
         ("C-c y a" . yafolding-toggle-all)))

(use-package json-mode
  :config (progn
            (use-package jq-mode
              :mode ("\\.jq$" . jq-mode))
            (setq json-reformat:indent-width 2)
            (setq js-indent-level 2))
  :mode ("\\.json$" . json-mode)
  :bind (("C-c j q" . jq-interactively)))

(use-package dash-at-point
  :bind (("C-c i"   . dash-at-point)
         ("C-c C-i" . dash-at-point-with-docset)))


(use-package ag
  :config (progn
            (setq ag-highlight-search nil)
            (setq ag-group-matches nil)
            (setq ag-reuse-window t)
            (setq ag-ignore-list (list "node_modules" "vendor.js" "public/assets"))))

(use-package flymd
  :config (progn
            (defun my-flymd-browser-function (url)
              (let ((process-environment (browse-url-process-environment)))
                (apply 'start-process
                       (concat "firefox " url)
                       nil
                       "/usr/bin/open"
                       (list "-a" "firefox" url))))
            (setq flymd-browser-open-function 'my-flymd-browser-function)))

(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind (("M-n" . drag-stuff-down)
         ("M-p" . drag-stuff-up)))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure nil
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

(use-package typescript-mode
  :config (setq typescript-indent-level 2))

(use-package tide
  :ensure t
  :config (progn
            (setq company-tooltip-align-annotations t)
            ;; (add-hook 'before-save-hook 'tide-format-before-save)
            (add-hook 'typescript-mode-hook (lambda ()
                                              (interactive)
                                              (tide-setup)
                                              (flycheck-mode +1)
                                              (eldoc-mode +1)
                                              (tide-hl-identifier-mode +1)
                                              (company-mode +1)))))

(use-package unicode-fonts
   :ensure t
   :config
   (unicode-fonts-setup))
