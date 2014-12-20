# evil-snipe

> This is a new (and potentially buggy) plugin and I am an elisp newb -- any
> advice or contributions would be appreciated!

Snipe is a marriage of [vim-sneak](https://github.com/justinmk/vim-sneak) and
[vim-seek](https://github.com/goldfeld/vim-seek) but for
[evil-mode](https://gitorious.org/evil/pages/Home) on Emacs.

Put simply, evil-snipe is f/F/t/T on steroids. It can be configured to accept N
characters, but by default will accept 2.

## Installation

Evil-snipe can be installed from MELPA, via `M-x package-install RET evil-snipe`

Then, to enable globally, add the following to `~/.emacs`:

```elisp
(require 'evil-snipe)
(global-evil-snipe-mode 1)

;; Optional!
(evil-snipe-replace-evil) ;; replaces evil-mode's f/F/t/T/;/, with snipe
(evil-snipe-enable-nN)    ;; enable repeating searches with n/N

;; not necessary if using (evil-snipe-replace-evil)
(evil-snipe-enable-sS)    ;; enable repeating searches with s/S
```

## Preview

<-- animated screenshot here soon -->

## Features

  * `s/S`
    * `|the abacus is` => `sab`  => `the |abacus is`
    * `the abacus| is` => `Sab`  => `the |abacus is`
  * `z/Z` = inclusive, `x/X` = exclusive
    * `|the abacus is` => `dzab` => `|acus is`
    * `the abacus| is` => `dZab` => `the | is`
    * `|the abacus is` => `dxab` => `|abacus is`
    * `the abacus| is` => `dXab` => `the ab| is`
  * Snipe is always literal: `s\*` jumps to a literal `\*`
  * `s<Enter>`, `S<Enter>` (reverse), `;` and `,` (reverse) jumps to the next
    match. Note: `;` and `,` should still work directly after sniping even if
    you have rebound them.
  * Repeating with `s/S` and/or `n/N` can be enabled, see
    `(evil-snipe-enable-sS)` and `(evil-snipe-enable-sS)`.
  * Backspace to undo characters
  * `TAB` in the snipe prompt increments N on the fly. e.g. `s<tab><tab>goal`
  * Highlight matches if `evil-snipe-enable-highlight`
  * Incrementally highlight (as you type) if
    `evil-snipe-enable-incremental-highlight`
  * Change scope of searches with `evil-snipe-scope`
  * Change scope of searches while repeating with `evil-snipe-repeat-scope`
    (separate from `evil-snipe-scope`)
  * Change what the count prefix means to snipe with `evil-snipe-count-scope`
    * if nil, treat COUNT as default in vim: times-to-repeat
    * if 'letters, accept COUNT characters
    * if 'vertical, scope is column bound (vertical scoping)
  * Use `(evil-snipe-replace-evil)` to replace evil-mode's f/F/t/T/;/, with
    snipe. Snipe implements [clever-f](https://github.com/rhysd/clever-f.vim)
    functionality as well.

### Planned

  * Vertical-scoping: `5shi` will jump to the next occurance of 'hi' that is
    within 5 columns of the cursor on any following line.
  * `r/R` operators for targeting remote objects (e.g. `driwhi` = delete remote inner word
    'hi'), then return to starting point
  * `p/P` operators that do what `r/R` does, but stays in the modified location.
  * `r/R/p/P` text-objects, so: `dirwo` will delete the next inner word containing `wo`.
    dorwo will target the next OUTER word.

## Configuration

* Sniping in visual mode:

  ```elisp
  (evil-define-key 'visual evil-snipe-mode-map "z" 'evil-snipe-f)
  (evil-define-key 'visual evil-snipe-mode-map "Z" 'evil-snipe-F)
  ```
* Snipe disables evil-mode's substitute commands (s/S). To prevent this,
  set `evil-snipe-auto-disable-substitute` to nil (before evil-snipe is loaded).

* To change the highlight colors, configure: `evil-snipe-first-match-face` and
  `evil-snipe-matches-face`

* Disable `N>` prompt when sniping by setting `evil-snipe-show-prompt` to nil

### Configure like vim-seek

```elisp
(setq evil-snipe-scope 'line)
(setq evil-snipe-repeat-scope 'whole-line)
(setq evil-snipe-count-scope nil)
(setq evil-snipe-search-highlight nil)
(setq evil-snipe-search-incremental-highlight nil)
(setq evil-snipe-enable-half-cursor nil)
```

### Configure like vim-sneak

```elisp
(evil-snipe-enable-sS)

;; or 'buffer, 'whole-visible or 'whole-buffer
(setq evil-snipe-scope 'visible)
(setq evil-snipe-repeat-scope 'whole-visible)

(setq evil-snipe-count-scope 'vertical)  ;; not implemented yet
(setq evil-snipe-enable-highlight t)
(setq evil-snipe-enable-incremental-highlight t)
(setq evil-snipe-enable-half-cursor nil)
```

### Compatibility

* [evil-surround](https://github.com/timcharper/evil-surround) does not conflict
  with evil-snipe. Surround uses `s/S` in visual mode and `s` in operator mode.
  Snipe uses `s/S` in normal mode and `z/Z/x/X` in operator mode.
* [evil-space](https://github.com/linktohack/evil-space) needs more investigating.

## Credits

Evil-snipe was inspired by:

* [vim-seek](https://github.com/goldfeld/vim-seek)
* [vim-sneak](https://github.com/justinmk/vim-sneak)
* [evil-sneak](https://github.com/AshleyMoni/evil-sneak)
