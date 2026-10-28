;; run (nerd-icons-install-fonts) on fresh install
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package vertico
  :init (vertico-mode 1)
  :bind (("C-c C-r" . vertico-repeat)
         ("<f6>" . vertico-repeat))
  :hook (minibuffer-setup . vertico-repeat-save)
  :config (require 'vertico-repeat))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-c k" . consult-ripgrep)
         ("C-x b" . consult-buffer)
         ("M-i" . consult-imenu-multi)
         ("C-c a" . project-find-file)
         ("C-c j" . consult-git-grep)
         ("C-x l" . consult-locate)
         ("<f1> l" . load-library)
         ("<f2> i" . info-lookup-symbol)
         ("<f2> u" . insert-char)
         :map read-expression-map
         ("C-r" . consult-history))
  :config
  (setq consult-ripgrep-args
        (concat consult-ripgrep-args
                " --glob=!node_modules --glob=!vendor.js --glob=!public/assets")))

(use-package embark
  :bind (("C-." . embark-act))
  :config
  (defun -embark-magit-dir (file)
    "Open magit-status for the repository containing FILE."
    (magit-status-setup-buffer (magit-toplevel (if (file-directory-p file) file (file-name-directory file)))))
  (define-key embark-file-map (kbd "g") #'-embark-magit-dir))

(use-package embark-consult
  :after (embark consult))

(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("g" . consult-ripgrep)
              ("b" . consult-project-buffer))
  :custom
  (project-vc-ignores '(".DS_Store" "node_modules/" ".yarn/" "*.min.js*" "*.bundle.js" "dist/")))
(use-package dockerfile-mode
  :mode (("^Dockerfile" . dockerfile-mode)))

(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (ruby-flymake-use-rubocop-if-available nil))

;; --------------------------------------------------------------------------------

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package ansi-color
  :config (defun -colorize-compilation-buffer ()
            (interactive)
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package magit
					;  :init (use-package magit-blame)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
  ;; (setq magit-highlight-indentation nil)
  ;; (setq magit-highlight-trailing-whitespace 1)
  (setq magit-process-popup-time -1)
  (setq magit-repository-directories '(("~/development" . 1) ("~/development/go/src/github.com/jowl" . 1) ("~/volvo" . 4)))
  ;; (setq magit-set-upstream-on-push t)
  ;; (setq magit-stage-all-confirm nil)
  ;; (setq magit-unstage-all-confirm nil)
  ;; (setq magit-restore-window-configuration t)
  ;; (setq magit-push-always-verify nil)
  ;; (setq magit-revert-buffers t)
  (setq magit-diff-refine-hunk t)
  (add-hook 'magit-mode-hook (lambda () (if (f-file? (f-expand "Gemfile" default-directory)) (rspec-mode))))
  :bind
  (("C-c g" . magit-status)
   ("C-c b" . magit-blame)))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.3)
  (setq company-show-numbers 1)
  :bind
  (("C-<tab>" . company-complete)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-?" . mc/mark-all-dwim)
   ("M-/" . mc/edit-lines)))

(use-package yasnippet
  :init (setq yas-snippet-dirs (list (f-expand "snippets" user-emacs-directory)))
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  :mode ("\\.yasnippet" . snippet-mode))

;; (use-package smartparens-config
;;   :init
;;   (progn
;;     (use-package smartparens-html)
;;     (use-package smartparens-ruby)
;;     (use-package smartparens-scala)
;;     (use-package smartparens-latex)
;;     (use-package smartparens-haskell)
;;     (smartparens-global-mode 1)
;;     (show-smartparens-global-mode 1))
;;   :bind
;;   (("C-M-k" . sp-kill-sexp)
;;    ("C-M-w" . sp-copy-sexp)
;;    ("C-)"   . sp-forward-slurp-sexp)
;;    ("C-("   . sp-backward-slurp-sexp)
;;    ("C-}"   . sp-forward-barf-sexp)
;;    ("C-{"   . sp-backward-barf-sexp)
;;    ;; navigation
;;    ("C-M-a" . sp-beginning-of-sexp)
;;    ("C-M-e" . sp-end-of-sexp)
;;    ("C-M-n" . sp-next-sexp)
;;    ("C-M-p" . sp-previous-sexp)
;;    ("C-M-f" . sp-forward-sexp)
;;    ("C-M-b" . sp-backward-sexp)))

;; (use-package discover)

(use-package ruby-mode
  :init (use-package rspec-mode
          :config (defun -rspec-compile (orig target &optional opts)
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
                                                   (rspec-rerun))))))
          :bind (("C-c , g" . -rspec-compilation-buffer)))
  :config
  (setq ruby-deep-indent-paren nil)
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'dired-mode-hook 'rspec-dired-mode)
  ;; (add-hook 'ruby-mode-hook (lambda ()
  ;;                             (bind-key ", e" '-rspec-verify-single-example rspec-verifiable-mode-keymap)
  ;;                             (bind-key ", E" '-rspec-verify-example rspec-verifiable-mode-keymap)))
  (setq ruby-align-to-stmt-keywords '(begin if while unless until case for))
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)))

;; (use-package smex
;;   :init (smex-initialize)
;;   :bind ("M-x" . smex))

(use-package multi-term
  :init (setq multi-term-program "/bin/zsh"))

(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)))

(use-package ibuffer-vc
  :init (ibuffer-vc-set-filter-groups-by-vc-root)
  :bind ("C-x C-b" . ibuffer))

(use-package rainbow-mode
  :config  (setq rainbow-x-colors nil))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config (setq-default js2-basic-offset 2))

(use-package sh-script
  :config (setq sh-basic-offset 2))

(use-package css-mode
  :mode (("\\.s?css$" . css-mode))
  :config (setq css-indent-offset 2))

(use-package web-mode
  :mode ("\\.html?$" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t))

(use-package git-gutter-fringe
  :if window-system
  :demand
  :init (global-git-gutter-mode t)
  :config (fringe-helper-define 'git-gutter-fr:modified nil
				"........"
				"...XX..."
				"..XXXX.."
				".XXXXXX."
				".XXXXXX."
				"..XXXX.."
				"...XX..."
				"........")
  :bind (("C-x g g" . git-gutter)
         ("C-x g n" . git-gutter:next-hunk)
         ("C-x g p" . git-gutter:previous-hunk)
         ("C-x g s" . git-gutter:stage-hunk)
         ("C-x g v" . git-gutter:revert-hunk)
         ("C-x g TAB" . git-gutter:popup-hunk)
         ("C-x g SPC" . git-gutter:mark-hunk)))

;; (use-package swiper
;;   :config (bind-key "C-r" 'ivy-previous-line ivy-minibuffer-map)
;;   :bind (("C-s" . swiper)
;;          ("C-r" . swiper)))

(use-package json-mode
  :config (use-package jq-mode
            :mode ("\\.jq$" . jq-mode))
  (setq json-reformat:indent-width 2)
  (setq js-indent-level 2)
  :mode ("\\.json$" . json-mode)
  :bind (("C-c C-j q" . jq-interactively)))

(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind (("M-n" . drag-stuff-down)
         ("M-p" . drag-stuff-up)))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest)
  :bind (("C-s-<return>" . copilot-accept-completion) ("C-s-<tab>" . copilot-next-completion) ("C-s-n" . copilot-accept-completion-by-line) ("C-s-f" . copilot-accept-completion-by-word)))

(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.23.2")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")))
  (setq treesit-font-lock-level 4)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  :config
  (defun -treesit-install-missing-grammars ()
    "Build the tree-sitter grammars listed in `treesit-language-source-alist'. Needs git and a C compiler."
    (interactive)
    (dolist (source treesit-language-source-alist)
      (unless (treesit-language-available-p (car source))
        (treesit-install-language-grammar (car source)))))
  (setq typescript-ts-mode-indent-offset 2))

(use-package eglot
  :ensure nil
  :hook ((rust-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("M-j" . consult-imenu)
              ("C-c C-c l" . flymake-show-buffer-diagnostics)
              ("C-c C-c a" . eglot-code-actions)
              ("C-c C-c r" . eglot-rename)
              ("C-c C-c q" . eglot-reconnect)
              ("C-c C-c Q" . eglot-shutdown))
  :config
  (defun -eglot-format-on-save ()
    (add-hook 'before-save-hook
              (lambda () (when (eglot-managed-p) (eglot-format-buffer)))
              nil t))
  (add-hook 'rust-ts-mode-hook #'-eglot-format-on-save))

(use-package graphql-mode
  :mode ("\\.graphql\\'" "\\.gql\\'"))

(provide 'packages)

