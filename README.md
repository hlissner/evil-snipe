# evil-snipe

Evil-snipe enables you to search quicker and more precisely in the buffer. It
improves on [evil-mode](https://gitorious.org/evil/pages/Home)'s built-in single
character motions (`f`/`F`/`t`/`T`) by adding another key pair for 2-char
searches: `s` and `S` -- as well as `z`/`Z` in operator mode (e.g. following a
`d` or `c` motion).

2-char searchs are 50x more accurate. You _could_ use `/` or `?`, but it saves
you the extra keystroke of pressing enter and is harder to reach than `s` and
`S`.

## Main Features

### 2-char search with `s`/`S`

A 2-char search can be performed with `s{char}{char}`, `S` will do the same backwards.

### Enhanced 1-char search (`f`/`F`/`t`/`T`)

If `evil-snipe-override-evil` is non-nil, evil-snipe replaces evil-mode's single
character motions with 1-char sniping; this comes with the added benefit of
incremental highlighting and a customizable search scope.

### Customizable search scope

Searches are scoped depending on `evil-snipe-scope`'s value, which can be any of:

Value            | Description
-----------------|------------------------------------------------------------
buffer           | search the rest of the buffer after the cursor (`vim-sneak` behavior)
line             | **(default)** search the current line after the cursor (`vim-seek` behavior)
visible          | search the rest of the _visible_ buffer only
whole-line       | same as `line`, but highlight matches on either side of cursor
whole-buffer     | same as `buffer`, but highlight *all* matches in buffer
whole-visible    | same as 'visible, but highlight *all* visible matches in buffer

Search scope while _repeating_ a previous search is independently adjustable by
changing `evil-snipe-repeat-scope`, which accepts the same values
`evil-snipe-scope` does.

### Symbol groups

With symbol groups you can map characters to regex patterns. To add a pattern,
add a `'(CHAR REGEX)` list to `evil-snipe-symbol-groups`.

Here are some examples:

* Mapping `[` and `]` to all brackets allows typing `sa[` to match `a[`, `a(` or
`a{`, **or** `s].` will match '].', ').' or '}.'
```elisp
(add-to-list 'evil-snipe-symbol-groups '(?\\[ \"[[{(]\"))
(add-to-list 'evil-snipe-symbol-groups '(?\\] \"[)}]]\"))
```

* Mapping `:` to match a python function allows you to quickly cycle through all
function definitions in a buffer with `f:fff`
```elisp
(add-to-list 'evil-snipe-symbol-groups '(?\\: \"def .+:\"\))
```

### Overview

  * `s/S`
    * `|the abacus is` => `sab`  => `the |abacus is`
    * `the abacus| is` => `Sab`  => `the |abacus is`
  * `z/Z` = inclusive, `x/X` = exclusive
    * `|the abacus is` => `dzab` => `|acus is`
    * `the abacus| is` => `dZab` => `the | is`
    * `|the abacus is` => `dxab` => `|abacus is`
    * `the abacus| is` => `dXab` => `the ab| is`
  * Snipe is literal: `s\*` jumps to a literal `\*`
  * `s<Enter>` repeats, `S<Enter>` repeats in the opposite direction. `;` and
    `,` are aliases.
  * Set `evil-snipe-override-evil` to non-nil to replace evil-mode's 1-char
    motions with evil-snipe's 1-char motions.
  * Repeat searches with `s`, `S` (reverse) if `evil-snipe-repeat-keys` is
    non-nil. Also applies to `f/F/t/T` if `evil-snipe-override-evil` is set.
  * Backspace to undo characters
  * `TAB` in the prompt increments N on the fly, and lets you type more
    characters. e.g. `s<tab><tab>goal`
  * Highlight matches if `evil-snipe-enable-highlight`
  * Incrementally highlight (as you type) if
    `evil-snipe-enable-incremental-highlight`
  * Change scope of searches with `evil-snipe-scope`
  * Change scope of searches while repeating with `evil-snipe-repeat-scope`
    (separate from `evil-snipe-scope`)
  * Change what the count prefix means to snipe with `evil-snipe-count-scope`
    * if nil, treat COUNT as default in vim: times-to-repeat
    * if 'letters, accept COUNT characters
    * ~~if 'vertical, scope is column bound (vertical scoping)~~ (not implemented)
  * Supports **smart case**. If `evil-snipe-smart-case` is non-nil, searches
    will be case-insensitive unless they include capital letters.
  * Regex symbol groups. See `evil-snipe-symbol-groups`. You can map single
    characters to entire regex expressions. For instance, `]` => `[])}]`
  * Set `evil-snipe-auto-scroll` to non-nil to have window scroll with your
    searches (keeps your selection centered).
  * `evil-snipe-show-prompt`: whether or not to show `N>` prompt in minibuffer
    while sniping.

## Installation

Evil-snipe is on MELPA: `M-x package-install RET evil-snipe`

Enable it with:
```elisp
(require 'evil-snipe)
(evil-snipe-mode 1)
```

Or for specific modes:
```elisp
(add-hook 'prog-mode-hook 'evil-snipe-mode)
```

## Configuration

* If you want sniping in visual mode:
  ```elisp
  (evil-define-key 'visual evil-snipe-mode-map "z" 'evil-snipe-f)
  (evil-define-key 'visual evil-snipe-mode-map "Z" 'evil-snipe-F)
  ```
* Snipe disables evil-mode's substitute commands (s/S). To prevent this,
  set `evil-snipe-auto-disable-substitute` to nil (before evil-snipe is loaded).
* To change the highlight colors, configure: `evil-snipe-first-match-face` and
  `evil-snipe-matches-face`

### Configure like vim-seek
  ```elisp
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-repeat-scope 'whole-line)
  (setq evil-snipe-count-scope nil)
  (setq evil-snipe-search-highlight nil)
  (setq evil-snipe-search-incremental-highlight nil)
  ```
### Configure like vim-sneak
  ```elisp
  (setq evil-snipe-repeat-keys t)

  ;; or 'buffer, 'whole-visible or 'whole-buffer
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'whole-visible)
  (setq evil-snipe-enable-highlight t)
  (setq evil-snipe-enable-incremental-highlight t)

  ;; Note: vertical scoping isn't implemented yet
  ```

## Credits

Evil-snipe was inspired by:
* [vim-seek](https://github.com/goldfeld/vim-seek)
* [vim-sneak](https://github.com/justinmk/vim-sneak)
* [evil-sneak](https://github.com/AshleyMoni/evil-sneak)
