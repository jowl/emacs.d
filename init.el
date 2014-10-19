(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'use-package)
(require 'f)
(require 's)
(require 'dash)
(dash-enable-font-lock)

(setq default-directory (f-full (getenv "HOME")))
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
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
(menu-bar-mode 1)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(setq delete-by-moving-to-trash t)
(setq insert-directory-program "gls")
(bind-key "s-," (lambda () (interactive) (find-file-other-window user-init-file)))
(bind-key "C-c h" 'query-replace-regexp)
(bind-key "C-c a" 'align-regexp)
(setq require-final-newline nil)
(add-to-list 'custom-theme-load-path (f-expand "themes" user-emacs-directory))
(load-theme 'slick t)
(windmove-default-keybindings 'super)
(show-paren-mode t)
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

(defun load-local (file)
  (load (f-expand file user-emacs-directory)))

(load-local "packages")
(require 'ace-jump-mode)
(bind-key "C-c SPC" 'ace-jump-mode)

(defun reset-buffers ()
  (interactive)
  (-each
      (--reject
       (eq (current-buffer) it)
       (buffer-list))
    'kill-buffer)
  (delete-other-windows))

(defun ack-wrap (arg)
  (interactive "P")
  (print arg)
  (let ((current-prefix-arg nil)
        (ack-and-a-half-ignore-case (if arg nil 'smart)))
    (call-interactively 'projectile-ack)))

(defun -rspec-verify-single-example (example)
  "Runs the specified example of the current buffer."
  (interactive "sExample (current buffer): ")
  (rspec-run-single-file
   (rspec-spec-file-for (buffer-file-name))
   (rspec-core-options)
   (concat " --example \"" example "\"")))

(defun -rspec-verify-example (example)
  "Runs specified example for the project of the current file."
  (interactive "sExample (current project): ")
  (rspec-run (concat (rspec-core-options) " --example \"" example "\"")))

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
