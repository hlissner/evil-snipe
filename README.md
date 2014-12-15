# evil-snipe

> This is a new (and potentially buggy) plugin and I am an elisp newb -- any
> advice or contributions would be appreciated!

Snipe is a marriage of [vim-sneak](https://github.com/justinmk/vim-sneak) and
[vim-seek](https://github.com/goldfeld/vim-seek), but for
[evil-mode](https://gitorious.org/evil/pages/Home) on Emacs.

Put simply, evil-snipe is f/F/t/T on steroids. It can be configured to accept N
characters, but by default will accept 2.

## Installation

Evil-snipe can be installed from MELPA, via `M-x package-install RET evil-snipe`

Then, to enable globally, add the following to `~/.emacs`:

```elisp
(require 'evil-snipe)
(global-evil-snipe-mode 1)
```

## Preview

<-- animated screenshot here soon -->

## Features

  * Press `sab` to move the cursor immediately onto the 'a' of the next
    occurrence of `ab`.
  * Use `S` to search backwards.
  * Evil-snipe is always literal: `s\*` will jump to a literal `\*`
  * In operator mode, evil-snipe is bound to `z/Z` (inclusive) and `x/X`
    (exclusive). For instance, `dzab` will delete up to and including the 'ab'.
    `x/X` will stop short of the 'ab'.
  * Highlight matches if `evil-snipe-search-highlight` is non-nil.
  * Incrementally highlight matches as you type if
    `evil-snipe-search-incremental-highlight` is non-nil.
  * `evil-snipe-scope` controls the scope of searches. Use `'line` to mimic
    vim-seek and `'visible` or `'buffer` to mimic vim-sneak. See variable for
    other options and better explanations.
  * Vertical scoping when `evil-snipe-count-scope` is set to 'vertical
  * While typing your search characters, press `TAB` to increment the character
    count on the fly. e.g. `s<tab><tab>goal` will search for the next instance
    of "goal".
  * Backspace works in the snipe prompt.
  * Press `s<Enter>` to repeat the last snipe. `S<enter>` does the inverse.
  * `;` and `,` repeat support (as well as `s/S` and `n/N` support; see below)
  * `evil-snipe-repeat-scope`, separate from `evil-snipe-scope`, controls the
    scope of searches and highlighting when repeating searches. The default is
    `'whole-line`.

### Planned

  * Vertical-scoping: `5shi` will jump to the next occurance of 'hi' that is
    within 5 columns of the cursor on any following line.
  * `r/R` operators for targeting remote objects (e.g. `driwhi` = delete remote inner word
    'hi'), then return to starting point
  * `p/P` operators that do what `r/R` does, but stays in the modified location.
  * `r/R/p/P` text-objects, so: `dirwo` will delete the next inner word containing `wo`.
    dorwo will target the next OUTER word.

## Configuration

**Any variable assignments should be done _before_ loading evil-snipe.**

* By default sniping is scoped to the current line (relative to your cursor).
  This is consistent with vim-seek. If you prefer vim-sneak's
  rest-of-buffer-scoped approach, do:

  ```elisp
  (setq evil-snipe-scope 'visible)  ;; or 'buffer, 'whole-visible or 'whole-buffer
  ```
* Disable highlighting (and/or incremental highlighting) with:

  ```elisp
  (setq evil-snipe-enable-highlight nil)
  (setq evil-snipe-enable-incremental-highlight nil)
  ```
* To get sniping is visual mode:

  ```elisp
  (evil-define-key 'visual evil-snipe-mode-map "z" 'evil-snipe-f)
  (evil-define-key 'visual evil-snipe-mode-map "Z" 'evil-snipe-F)
  ```
* evil-snipe disable's evil-mode's substitute commands (s/S), set
  `evil-snipe-auto-disable-substitute` to nil.

* evil-snipe can override evil-mode's f/F/t/T and ;/, and replace them. To do
  so, set `evil-snipe-override` to t. Note that ;/, will work even if you've
  rebound them, but only *just* after you've initiated a search.

* Change what the number prefix means to evil-snipe with
  `evil-snipe-count-scope`.
  * If set to nil, repeat search N times.
  * If set to 'letters, search for N characters. e.g. `5shello` will jump to the
    next 'hello'.
  * If set to 'vertical **(NOT IMPLEMENTED YET)**: limit search to within N
    columns of the point, on any following or preceding line. E.g. `5shi` will
    search for the next 'hi' within 5 columns on any following line.

* You can repeat search using `;` and `,`. To add `s/S` support, see
  `evil-snipe-repeat-sS`, and `evil-snipe-repeat-nN` for `n/N` support.

### Compatibility

* [evil-surround](https://github.com/timcharper/evil-surround)'s s/S mappings
  override snipe in visual mode. It **does not** affect evil-surround's `s`
  operator though. Snipe uses `z/Z/x/X` instead. Perhaps we can use that in visual
  mode as well.
* [evil-space](https://github.com/linktohack/evil-space) needs more investigating.


## Credits

Evil-snipe was inspired by:

* [vim-seek](https://github.com/goldfeld/vim-seek)
* [vim-sneak](https://github.com/justinmk/vim-sneak)
* [evil-sneak](https://github.com/AshleyMoni/evil-sneak)
