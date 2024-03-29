(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package f)
(use-package s)
;(require 'dash)
;(dash-enable-font-lock)

(require 'server)
(unless (server-running-p)
  (server-start))

;; (setq exec-path-from-shell-check-startup-files nil)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq default-directory (f-full (getenv "HOME")))
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq standard-indent 2)
;; (setq c-basic-offset 2)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq ring-bell-function 'ignore)
;; (setq mac-option-key-is-meta t)
;; (setq mac-command-key-is-meta nil)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(unbind-key "s-q")
(menu-bar-mode 1)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(setq delete-by-moving-to-trash t)
(setq insert-directory-program "gls")
(bind-key "s-," (lambda () (interactive) (magit-status (f-dirname user-init-file))))
(bind-key "C-c h" 'query-replace-regexp)
(bind-key "C-c a" 'align-regexp)
(setq require-final-newline 'visit)
(setq mode-require-final-newline nil)
(windmove-default-keybindings 'super)
(column-number-mode t)
(scroll-bar-mode -1)
(cua-selection-mode t)
(show-paren-mode 1)
(global-hl-line-mode 1)
(bind-key "C-x C-c" (lambda ()
		      (interactive)
		      (when (y-or-n-p "Quit Emacs? ")
			(save-buffers-kill-emacs))))
(add-hook 'find-file-hook
          (lambda ()
            (setq show-trailing-whitespace t)))
(fset 'yes-or-no-p 'y-or-n-p)
(bind-key "C-v" (lambda () (interactive) (next-line 10) (recenter)))
(bind-key "M-v" (lambda () (interactive) (previous-line 10) (recenter)))
(bind-key "s-<mouse-1>" 'browse-url-at-mouse)
(bind-key "s-}" 'other-window)
(bind-key "s-{" (lambda () (interactive) (other-window -1)))
(setq ns-right-alternate-modifier 'none)
;(setq package-selected-packages '(ag all-the-icons all-the-icons-dired ansi-color company counsel counsel-projectile css-mode dash-at-point discover dockerfile-mode doom-modeline doom-themes drag-stuff exec-path-from-shell flycheck flymd gist git-gutter-fringe haskell-mode ibuffer-vc ivy ivy-prescient ivy-rich jq-mode js2-mode json-mode magit magit-blame misc multi-term multiple-cursors projectile rainbow-mode rspec-mode ruby-mode sh-script smart-mode-line smartparens-config smartparens-haskell smartparens-html smartparens-latex smartparens-ruby smartparens-scala smex swiper tide typescript-mode unicode-fonts web-mode yaml-mode yasnippet))

;; (defun load-local (file)
;;   (load (f-expand file user-emacs-directory)))

(require 'packages)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (let ((themes-path (f-expand "themes" user-emacs-directory)))
;;   (add-to-list 'load-path themes-path)
;;   (add-to-list 'custom-theme-load-path themes-path))
;; (defun toggle-slick-theme ()
;;   (interactive)
;;   (let ((slick-next-theme (if (and (boundp 'slick-current-theme) (eq slick-current-theme 'slick)) 'slick-light 'slick)))
;;     (let ((sml-next-theme (if (eq slick-next-theme 'slick) 'automatic 'respectful)))
;;       (setq slick-current-theme slick-next-theme)
;;       (load-theme slick-next-theme t)
;;       (sml/apply-theme sml-next-theme))))
;; (toggle-slick-theme)

(defun reset-buffers ()
  (interactive)
  (-each
      (--reject
       (eq (current-buffer) it)
       (buffer-list))
    'kill-buffer)
  (delete-other-windows))

(defun -rspec-verify-single-example (example)
  "Runs the specified example of the current buffer."
  (interactive "sExample (current buffer): ")
  (rspec-run-single-file
   (rspec-spec-file-for (buffer-file-name))
   (rspec-core-options)
   (concat " --example " (shell-quote-argument example))))

(defun -rspec-verify-example (example)
  "Runs specified example for the project of the current file."
  (interactive "sExample (current project): ")
  (rspec-run (concat (rspec-core-options) " --example " (shell-quote-argument example))))

(defvar closed-files (list))

(defun track-closed-file ()
  (and buffer-file-name
       (message buffer-file-name)
       (or (delete buffer-file-name closed-files) t)
       (add-to-list 'closed-files buffer-file-name)))

(defun recently-closed-files ()
  (interactive)
  (find-file (ido-completing-read "Recently closed: " closed-files)))

(add-hook 'kill-buffer-hook 'track-closed-file)
(bind-key "s-T" 'recently-closed-files)

(defadvice sticky-window-keep-window-visible (after sticky-window-mark activate)
  (let ((mark " ★"))
    (if (window-dedicated-p (selected-window))
        (unless (s-ends-with? mark (buffer-name)) (rename-buffer (s-append mark (buffer-name))))
      (rename-buffer (s-chop-suffix mark (buffer-name))))))

(defun -enlarge-window-horizontally ()
  (interactive)
  (call-interactively 'enlarge-window-horizontally)
  (-*-window-horizontally-transient-map))

(defun -shrink-window-horizontally ()
  (interactive)
  (call-interactively 'shrink-window-horizontally)
  (-*-window-horizontally-transient-map))

(defun -*-window-horizontally-transient-map ()
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "}") '-enlarge-window-horizontally)
     (define-key map (kbd "{") '-shrink-window-horizontally)
     map)))

(bind-key "C-x }" '-enlarge-window-horizontally)
(bind-key "C-x {" '-shrink-window-horizontally)

(bind-key "M-g" 'goto-line)

(add-hook 'find-file-hook
          (lambda ()
            (linum-mode t)))

;; (setq frame-title-format '(:eval (frame-title-function)))

;; (defun frame-title-function ()
;;   (let ((title (if (buffer-file-name) '("%f") '("%b"))))
;;     (if (projectile-project-p)
;;         (list title " - " (magit-get-current-branch))
;;       title)))

(defun -open-next-line ()
  (interactive)
  (end-of-line)
  (align-newline-and-indent))

(defun -open-line ()
  (interactive)
  (save-excursion (align-newline-and-indent)))

(bind-key "C-o" '-open-line)
(bind-key "M-o" '-open-next-line)

(defun copy-buffer-file-name ()
  (interactive)
  (kill-new buffer-file-name))

(defun rubify-fqcn (beg end)
  (interactive "r")
  (let ((case-fold-search nil))
    (save-excursion
      (goto-char beg)
      (re-search-forward "[A-Z]" end)
      (search-backward "." beg)
      (capitalize-region beg (point))
      (replace-string "." "::" nil (point) end)
      (replace-string "." "" nil beg end)
      (goto-char beg)
      (insert "Java::"))))

(put 'narrow-to-region 'disabled nil)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg))))
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "d") 'duplicate-current-line-or-region)
     map)))


(bind-key "C-c d" 'duplicate-current-line-or-region)

(defun -other-file-with-same-prefix (select-fn)
  "Cycle between files between the same prefix, e.g. index.css, index.html, index.js"
  (interactive)
  (let* ((file-name-base (replace-regexp-in-string "\.[^.]*$" "" (buffer-file-name)))
         (matching-files (f-entries (f-dirname file-name-base) (lambda (fn) (s-contains-p file-name-base fn)) nil))
         (index (-elem-index buffer-file-name matching-files))
         (count (length matching-files))
         (next-counterpart (nth (mod (funcall select-fn index) count) matching-files)))
    (find-file next-counterpart)))

(bind-key "M-[" (lambda () (interactive) (-other-file-with-same-prefix '1-)))
(bind-key "M-]" (lambda () (interactive) (-other-file-with-same-prefix '1+)))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(electric-pair-mode 1)

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "62f68a0b49cf383478041c688cc1b82f084f76b84a2ab2819a4ed9ceb59aefd8" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
;;  '(package-selected-packages
;;    '(unicode-fonts yaml-mode yafolding web-mode vue-mode use-package tide swoop smex smartparens smart-mode-line sass-mode rvm rspec-mode rbenv rainbow-mode projectile prodigy pallet multiple-cursors multi-term magit-gh-pulls kurecolor json-mode js2-mode jq-mode jade-mode ido-completing-read+ ibuffer-vc haskell-mode groovy-mode gradle-mode go-mode git-gutter-fringe gist flymd flx-ido exec-path-from-shell ess ensime drag-stuff discover dired-sidebar dash-at-point csharp-mode counsel coffee-mode browse-kill-ring auto-complete apib-mode all-the-icons-dired ag ace-jump-mode 0blayout)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "62f68a0b49cf383478041c688cc1b82f084f76b84a2ab2819a4ed9ceb59aefd8" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages
   '(graphql-mode rjsx-mode go-mode csharp-mode dired kotlin-mode markdown-mode jq-mode f use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
