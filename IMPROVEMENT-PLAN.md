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
- LSP and syntax: `tree-sitter`/`tide`/`rustic`/`lsp-mode`/`typescript-mode` replaced by built-in `treesit` (`rust-ts-mode`, `typescript-ts-mode`, `tsx-ts-mode`) + `eglot`. Run `M-x -treesit-install-missing-grammars` once (needs git + a C compiler) and install `rust-analyzer` and `typescript-language-server`. Rustic cargo commands are gone (use `compile`).
- Diagnostics: `flycheck` replaced by `flymake` (enabled in `prog-mode`; ruby uses `ruby -wc`, rubocop stays off).
- Small improvements: dead-code cleanup, `recentf`-based recent files, hook/default modernisation, `progn` unwrapping and conditional `package-refresh-contents`.

## Planned

### Documentation
- Add a README with fresh-install steps: `nerd-icons-install-fonts`, `gls` (coreutils), Node for copilot, rbenv.
