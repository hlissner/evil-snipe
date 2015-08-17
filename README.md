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

`(evil-snipe-override-mode 1)` causes evil-snipe to replace evil-mode's single
character motions with 1-char sniping; this comes with the added benefit of
incremental highlighting, customizable search scope and character aliasing (see
`evil-snipe-symbol-groups`).

### Customizable search scope

`evil-snipe-scope` determines the scope of a snipe and the incremental
highlighter. These are the possible settings:

Value            | Description
-----------------|------------------------------------------------------------
'buffer          | the rest of the buffer after cursor (`vim-sneak` behavior)
'line            | **(default)** the rest of the current line after cursor (`vim-seek` behavior)
'visible         | the rest of the _visible_ buffer after cursor
'whole-line      | same as `'line`, but match on either side of cursor
'whole-buffer    | same as `'buffer`, but match *all* matches in buffer
'whole-visible   | same as `'visible`, but match *all* visible matches in buffer

Search scope while _repeating_ a previous search is independently adjustable by
changing `evil-snipe-repeat-scope`, which accepts the same values
`evil-snipe-scope` does.

### Symbol groups

With symbol groups you can alias specific characters to regex patterns. To add a
pattern, add a `'(CHAR REGEX)` list to `evil-snipe-symbol-groups`.

Here are some examples:

```elisp
(evil-snipe-add-alias ?[ "[[{(]")
```
* Map `[` to all opening parenthesis and brackets. Therefore: `sa[` matches `a[`,
`a(` or `a{`.

```elisp
(evil-snipe-add-alias ?: "def .+:")
```
* For python users, this maps `:` to python function defs so you can cycle through
function definitions with `f:fff`.

## Installation

Evil-snipe is on MELPA: `M-x package-install RET evil-snipe`

Enable it globally with:

```elisp
(require 'evil-snipe)
(evil-snipe-mode 1)

;; OPTIONAL: Replaces evil-mode's f/F/t/T motions with evil-snipe
(evil-snipe-override-mode 1)
```

## Configuration

* If you want sniping in visual mode:
  ```elisp
  (evil-define-key 'visual evil-snipe-mode-map "z" 'evil-snipe-f)
  (evil-define-key 'visual evil-snipe-mode-map "Z" 'evil-snipe-F)
  ```
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
  ```

## Credits

Evil-snipe was inspired by:
* [vim-seek](https://github.com/goldfeld/vim-seek)
* [vim-sneak](https://github.com/justinmk/vim-sneak)
* [evil-sneak](https://github.com/AshleyMoni/evil-sneak)
