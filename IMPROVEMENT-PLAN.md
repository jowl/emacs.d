# Emacs config improvement plan

## Already applied
- `early-init.el`: `package-enable-at-startup` disabled (init.el calls `package-initialize`).
- Package archives: removed dead `orgmode.org/elpa`, added `nongnu`; removed TLS 1.3 workaround.
- Customize output moved to `custom.el` (`custom-file`), loaded before packages so `custom-safe-themes` applies to the theme.
- Removed duplicate `custom-safe-themes` and `smart-mode-line` (conflicted with `doom-modeline`).
- `C-c a` clash resolved: `align-regexp` is now `C-c A`; `counsel-git` keeps `C-c a`.
- Added missing `lsp-mode` / `lsp-ui` (needed by `rustic` bindings).
- Removed `web-mode` smartparens code (`sp-local-pair` was undefined).
- `magit-get-current-name` -> `magit-get-current-branch`.
- Removed ~100 lines of commented-out code, duplicate `insert-directory-program`, debug `message` in `track-closed-file`.
- `load-path` uses `user-emacs-directory`.
- Completion stack: `ivy`/`counsel`/`swiper`/`prescient` replaced by `vertico` + `orderless` + `marginalia` + `consult` + `embark`; `counsel-*` keys remapped (`C-c k` still uses `ag` until the search migration).
- Search: `ag` replaced by `consult-ripgrep` (`C-c k`, `C-c p g`); needs `rg` (`brew install ripgrep`).
- Project: `projectile` replaced by built-in `project.el` (`C-c p` prefix kept; `g` -> `consult-ripgrep`, `b` -> `consult-project-buffer`).
- `quelpa` removed; `copilot` installs via `use-package :vc`. Setup needs Node and `M-x copilot-install-server`. On an existing install, delete the old `elpa/copilot-*` (quelpa-built) directory and restart to switch to the VC checkout.

## Planned (not yet applied; each is a behaviour change, do separately)

### 1. Cleanup and correctness
- Convert `defadvice sticky-window-keep-window-visible` to `advice-add`; that package is never required/installed, so either add it or delete the advice.
- `recently-closed-files` uses `ido-completing-read`; replace with `recentf` + `completing-read` (or `consult-recent-file`) and drop manual `closed-files` tracking.
- `rubify-fqcn`: replace interactive-only `replace-string` with `re-search-forward` + `replace-match`.
- `M-g` -> `goto-line` shadows the `M-g` goto prefix; use `M-g g` or leave default.
- Decide between `electric-pair-mode` and `web-mode-enable-auto-pairing`.
- Prune `package-selected-packages` of unused packages (`ess`, `haskell-mode`, `dash-at-point`, `flymd`, `gist`, `kubernetes-helm`, `prettier`, `svelte-mode` if unused).
- `prodigy-services.el`: not loaded by init and contains hardcoded `/usr/local/Cellar/...` paths (Apple Silicon uses `/opt/homebrew`); review for secrets, update or delete.

### 2. Hooks and defaults
- Replace per-file `find-file-hook` for line numbers/trailing whitespace with `prog-mode-hook`/`text-mode-hook` (or `global-display-line-numbers-mode`).
- `(setq use-short-answers t)` instead of `fset 'yes-or-no-p`.
- Add `savehist-mode`, `save-place-mode`, `recentf-mode`, `global-auto-revert-mode`, `auto-save-visited-mode`.

### 3. Startup performance
- Drop `(progn ...)` wrappers inside `:config`; use `:hook`, `:bind`, `:commands` so packages lazy-load; remove unnecessary `:demand`/`:ensure t` (already `use-package-always-ensure`).
- Avoid `package-refresh-contents` at every cold start; only refresh when installing.
- Measure first with `M-x use-package-report` (set `use-package-compute-statistics`).

### 4. Larger modernisation (one PR each)
2. **LSP and syntax:** `tree-sitter`/`tide`/`rustic`/`lsp-mode` -> built-in `treesit` modes + `eglot` (Emacs 29+). Use `rust-ts-mode` and `typescript-ts-mode`/`tsx-ts-mode`.
3. **Diagnostics:** `flycheck` -> `flymake` (integrates with eglot).

### 5. Documentation
- Add a README with fresh-install steps: `nerd-icons-install-fonts`, `gls` (coreutils), Node for copilot, rbenv.
