(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

;; Load Customize output early: custom-safe-themes is needed before the theme loads.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(require 'use-package)
(setq use-package-always-ensure t)
;; Inspect load times with M-x use-package-report.
(setq use-package-compute-statistics t)

(use-package quelpa)
(use-package quelpa-use-package)

(use-package f)
(use-package s)

(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(setq default-directory (f-full (getenv "HOME")))
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq standard-indent 2)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq ring-bell-function 'ignore)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(unbind-key "s-q")
(menu-bar-mode 1)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(setq delete-by-moving-to-trash t)
(bind-key "s-," (lambda () (interactive) (magit-status (f-dirname user-init-file))))
(bind-key "C-c h" 'query-replace-regexp)
(bind-key "C-c A" 'align-regexp)
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
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda ()
                   (setq show-trailing-whitespace t)
                   (display-line-numbers-mode 1))))
(setq use-short-answers t)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-saved-items 200)
(global-auto-revert-mode 1)
(auto-save-visited-mode 1)
(bind-key "C-v" (lambda () (interactive) (next-line 10) (recenter)))
(bind-key "M-v" (lambda () (interactive) (previous-line 10) (recenter)))
(bind-key "s-<mouse-1>" 'browse-url-at-mouse)
(bind-key "s-}" 'other-window)
(bind-key "s-{" (lambda () (interactive) (other-window -1)))
(setq ns-right-alternate-modifier 'none)
(require 'packages)

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

(defun recently-closed-files ()
  (interactive)
  (find-file (completing-read "Recently closed: " recentf-list nil t)))

(bind-key "s-T" 'recently-closed-files)

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

(setq frame-title-format '(:eval (frame-title-function)))

(defun frame-title-function ()
  (let ((title (if (buffer-file-name) '("%f") '("%b"))))
    (if (project-current)
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

(defun rubify-fqcn (beg end)
  (interactive "r")
  (let ((case-fold-search nil))
    (save-excursion
      (goto-char beg)
      (re-search-forward "[A-Z]" end)
      (search-backward "." beg)
      (capitalize-region beg (point))
      (let ((end-marker (copy-marker end)))
        (while (re-search-forward "\\." end-marker t)
          (replace-match "::"))
        (goto-char beg)
        (while (re-search-forward "\\." end-marker t)
          (replace-match "")))
      (goto-char beg)
      (insert "Java::"))))

(put 'narrow-to-region 'disabled nil)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
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

