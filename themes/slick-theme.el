(require 'slick-colors)

(deftheme slick
  "Created 2013-10-15.")

(with-slick-colors
 (lambda ()
   (custom-theme-set-faces
    'slick

    ;; general
    `(header-line ((t (:box nil :foreground ,slick-orange+1 :background ,slick-bw-3 :inherit (mode-line)))))
    `(escape-glyph ((t (:weight bold :foreground ,slick-pink-1))))
    `(region ((t (:inverse-video nil :background ,slick-blue-2))))
    `(highlight ((t (:foreground ,slick-orange+2 :background ,slick-blue-2))))
    `(cursor ((t (:foreground ,slick-bw-3 :background ,slick-bw+3))))
    `(minibuffer-prompt ((t (:foreground ,slick-blue+1))))
    `(secondary-selection ((t (:background ,slick-blue-1))))
    `(trailing-whitespace ((t (:background ,slick-pink-2))))
    '(fixed-pitch ((t (:family "Monospace"))))
    '(variable-pitch ((t (:family "Sans Serif"))))
    `(hl-line ((t (:background ,slick-blue-2 :inherit nil))))
    '(button ((t (:underline (:color foreground-color :style line) :inherit (link)))))
    `(link ((t (:weight bold :underline (:color foreground-color :style line) :foreground ,slick-blue-0))))
    `(link-visited ((t (:weight normal :underline (:color foreground-color :style line) :foreground ,slick-blue-1 :inherit (link)))))
    `(fringe ((t (:foreground ,slick-bw-0 :background ,slick-bw-2))))
    `(tooltip ((((class color)) (:inherit (variable-pitch) :foreground ,slick-bw-3 :background "lightyellow")) (t (:inherit (variable-pitch)))))
    `(linum ((t (:foreground ,slick-bw-1 :height 120))))
    `(success ((t (:foreground ,slick-success))))
    `(warning ((t (:foreground ,slick-warning))))
    `(error ((t (:foreground ,slick-error))))

    ;; syntax highlighting
    `(font-lock-builtin-face ((t (:foreground ,slick-green-0))))
    `(font-lock-regexp-grouping-backslash ((t (:weight bold :foreground ,slick-green-0 :inherit (bold)))))
    `(font-lock-function-name-face ((t (:foreground ,slick-green-0))))
    `(font-lock-comment-face ((t (:foreground ,slick-bw-0))))
    `(font-lock-comment-delimiter-face ((t (:foreground ,slick-bw-1 :inherit (font-lock-comment-face)))))
    `(font-lock-doc-face ((t (:foreground ,slick-pink-1 :inherit (font-lock-string-face)))))
    `(font-lock-negation-char-face ((t (:foreground ,slick-pink-1))))
    `(font-lock-constant-face ((t (:foreground ,slick-blue+1))))
    `(font-lock-keyword-face ((t (:weight bold :foreground ,slick-pink-0))))
    `(font-lock-preprocessor-face ((t (:foreground ,slick-pink-0 :inherit (font-lock-builtin-face)))))
    `(font-lock-regexp-grouping-construct ((t (:weight bold :foreground ,slick-orange-0 :inherit (bold)))))
    `(font-lock-variable-name-face ((t (:foreground ,slick-orange-0))))
    `(font-lock-string-face ((t (:foreground ,slick-green+1))))
    `(font-lock-type-face ((t (:foreground ,slick-blue-0))))
    `(font-lock-warning-face ((t (:weight bold :inherit (warning)))))

    ;; mode line
    `(mode-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground ,slick-blue-1 :background ,slick-bw-3))))
    `(mode-line-buffer-id ((t (:weight bold :foreground ,slick-blue-0))))
    '(mode-line-emphasis ((t (:weight bold))))
    `(mode-line-highlight ((t (:box (:line-width 2 :color ,slick-blue-2 :style released-button))) (t (:inherit (highlight)))))
    `(mode-line-inactive ((t (:weight light :box (:line-width -1 :color nil :style released-button) :foreground ,slick-bw-1 :background ,slick-bw-2 :inherit (mode-line)))))

    ;; magit
    `(magit-branch ((t (:inherit magit-header :foreground ,slick-blue-0 :weight bold))))
    `(magit-diff-del ((t (:inherit diff-removed :foreground ,slick-pink-0 :background ,slick-bw-2))))
    `(magit-diff-add ((t (:inherit diff-added :foreground ,slick-green-0 :background ,slick-bw-2))))
    `(magit-item-highlight ((t (:background ,slick-blue-1 :inherit nil))))
    `(magit-log-sha1 ((t (:foreground ,slick-green-1))))
    `(magit-section-heading ((t (:foreground ,slick-orange+1))))
    `(magit-cherry-equivalent ((t (:foreground ,slick-blue+1))))

    ;; git-gutter
    `(git-gutter-fr:modified ((t (:foreground ,slick-orange-2))))
    `(git-gutter-fr:added ((t (:foreground ,slick-green-2))))
    `(git-gutter-fr:deleted ((t (:foreground ,slick-pink-2))))

    ;; company
    `(company-echo-common ((t (:foreground ,slick-pink-0))))
    `(company-preview ((t (:foreground ,slick-orange+1 :background ,slick-blue-2))))
    `(company-preview-common ((t (:inherit company-preview :foreground ,slick-pink-0))))
    `(company-preview-search ((t (:inherit company-preview :background ,slick-blue-1))))
    `(company-tooltip ((t (:background ,slick-bw-3))))
    `(company-scrollbar-bg ((t (:inherit company-tooltip :background ,slick-bw-1))))
    `(company-scrollbar-fg ((t (:inherit company-tooltip :background ,slick-pink-1))))
    `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground ,slick-orange+1))))
    `(company-tooltip-common ((t (:inherit company-tooltip :foreground ,slick-orange-0))))
    `(company-tooltip-selection ((t (:inherit company-tooltip :background ,slick-pink-2))))
    `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground ,slick-pink-0))))

    ;; search
    `(isearch ((t (:foreground ,slick-orange-0 :background ,slick-bw-3))))
    `(isearch-fail ((t (:foreground ,slick-orange+2 :background ,slick-pink-1))))
    `(lazy-highlight ((t (:foreground ,slick-orange+1 :background ,slick-bw-3))))
    `(match ((t (:weight bold :foreground ,slick-green-0 :background ,slick-bw-3))))
    '(query-replace ((t (:inherit ( isearch)))))

    ;;smart-parens
    `(sp-show-pair-match-face ((t (:background ,slick-blue-1))))
    `(sp-show-pair-mismatch-face ((t (:background ,slick-pink-1))))

    ;; ivy
    `(ivy-confirm-face ((t (:foreground ,slick-green-1))))
    `(ivy-current-match-face ((t (:foreground ,slick-bw-3 :background ,slick-blue+1))))
    `(ivy-match-required-face ((t (:foreground ,slick-pink+2))))
    `(ivy-minibuffer-match-face-1 ((t (:background ,slick-bw-1))))
    `(ivy-minibuffer-match-face-2 ((t (:background ,slick-blue-1))))
    `(ivy-minibuffer-match-face-3 ((t (:background ,slick-blue-1))))
    `(ivy-minibuffer-match-face-4 ((t (:background ,slick-pink-1))))
    `(ivy-remote-face ((t (:foreground ,slick-blue+2))))
    `(ivy-remote-face ((t (:foreground ,slick-blue+2))))

    ;; web-mode
    `(web-mode-html-tag-face ((t (:weight bold :foreground ,slick-pink-0))))
    `(web-mode-html-tag-custom-face ((t (:weight bold :foreground ,slick-pink+1))))
    `(web-mode-html-tag-bracket-face ((t (:foreground ,slick-bw+2))))
    `(web-mode-variable-name-face ((t (:foreground ,slick-orange+1))))
    `(web-mode-html-attr-name-face ((t (:foreground ,slick-blue+1))))
    `(web-mode-block-delimiter-face ((t (:foreground ,slick-green-0))))
    `(web-mode-current-element-highlight-face ((t (:background ,slick-blue-1))))

    ;; default
    `(default ((t (:family "Source Code Pro" :foundry "apple" :height 120 :foreground ,slick-bw+2 :background ,slick-bw-2)))))))

(provide-theme 'slick)
