(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'use-package)
(require 'f)
(require 's)
(require 'dash)
(dash-enable-font-lock)

(require 'server)
(unless (server-running-p)
  (server-start))

(setq exec-path-from-shell-check-startup-files nil)
(setq default-directory (f-full (getenv "HOME")))
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq standard-indent 2)
(setq c-basic-offset 2)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq ring-bell-function 'ignore)
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta nil)
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
(bind-key "C-x C-c" (lambda ()
		      (interactive)
		      (when (y-or-n-p "Quit Emacs?")
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

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-local "packages")

(let ((themes-path (f-expand "themes" user-emacs-directory)))
  (add-to-list 'load-path themes-path)
  (add-to-list 'custom-theme-load-path themes-path))
(defun toggle-slick-theme ()
  (interactive)
  (let ((slick-next-theme (if (and (boundp 'slick-current-theme) (eq slick-current-theme 'slick)) 'slick-light 'slick)))
    (let ((sml-next-theme (if (eq slick-next-theme 'slick) 'automatic 'respectful)))
      (setq slick-current-theme slick-next-theme)
      (load-theme slick-next-theme t)
      (sml/apply-theme sml-next-theme))))
(toggle-slick-theme)

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

(setq frame-title-format '(:eval (frame-title-function)))

(defun frame-title-function ()
  (let ((title (if (buffer-file-name) '("%f") '("%b"))))
    (if (projectile-project-p)
        (list title " - " (magit-get-current-branch))
      title)))

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

(defun agency-lib-test-toggle ()
  (interactive)
  (when (buffer-file-name)
    (let ((lib-matches
           (s-match
            "agency\\/lib\/\\(.+\\)\\/\\(.+\\)\\.coffee$"
            (buffer-file-name)))
          (spec-matches
           (s-match
            "agency\\/test\\/unit\/\\(.+\\)\\/\\(.+\\)\\.spec\\.coffee$"
            (buffer-file-name))))
      (cond (lib-matches
             (find-file
              (format
               "~/burtcorp/agency/test/unit/%s/%s.spec.coffee"
               (nth 1 lib-matches)
               (nth 2 lib-matches))))
            (spec-matches
             (find-file
              (format
               "~/burtcorp/agency/lib/%s/%s.coffee"
               (nth 1 spec-matches)
               (nth 2 spec-matches))))
            (:else (call-interactively 'projectile-toggle-between-implementation-and-test))))))

(bind-key "C-c p t" 'agency-lib-test-toggle projectile-mode-map)

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
