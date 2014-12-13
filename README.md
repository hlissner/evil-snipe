# evil-snipe

> This is a new (and potentially buggy) plugin, much functionality has yet to be
> implemented. I'm also an elisp noob, so help or contributions are appreciated!

Snipe is a marriage of [vim-sneak](https://github.com/justinmk/vim-sneak) and
[vim-seek](https://github.com/goldfeld/vim-seek), but for
[evil-mode](https://gitorious.org/evil/pages/Home) on Emacs.

Put simply, evil-snipe is f/F/t/T on steroids. It can be configured to accept N
characters, but by default will accept 2; `shi` will jump to the next occurrence
of 'hi'.

## Installation

Download evil-snipe.el, place it on your loadpath and insert this into your
emacs configuration:

```elisp
(add-to-list 'load-path "/directory/containing/evil-snipe/")
(require 'evil-snipe)
(global-evil-snipe-mode)
```

## Configuration

By default sniping is scoped to the current line (relative to your cursor). This
is consistent with vim-seek. If you prefer vim-sneak's rest-of-buffer-scoped
approach, do:

```elisp
(setq evil-snipe-scope 'visible)  ;; or 'buffer, 'whole-visible or 'whole-buffer
```

If you *don't* want incremental or highlighting at all (without which it becomes
more vim-seek-like):

```elisp
(setq evil-snipe-enable-highlight nil)
(setq evil-snipe-enable-incremental-highlight nil)
```

To get sniping in visual mode:

```elisp
(define-key evil-visual-state-map "z" 'evil-snipe-f)
(define-key evil-visual-state-map "Z" 'evil-snipe-F)
```

**Note:** snipe hijacks the s/S bindings in normal mode (e.g. `s{char]{char}}`,
which belong to 'evil-substitute'. If you miss it, `s` can be accomplished with
`cl` and `S` with `cc`. If that isn't enough, see
`evil-snipe-auto-disable-substitute`.

For more information, look up any of the following functions:

```elisp
'evil-snipe-f
'evil-snipe-F
'evil-snipe-t
'evil-snipe-T
```

### Compatibility

* [evil-surround](https://github.com/timcharper/evil-surround)'s s/S mappings
  override snipe in visual mode. It **does not** affect evil-surround's `s`
  operator though. Snipe uses `z/Z/x/X` instead. Perhaps we can use that in visual
  mode as well.
* [evil-space](https://github.com/linktohack/evil-space) needs more investigating.

## Features

  * Press `sab` to move the cursor immediately onto the 'a' of the next
    occurrence of `ab`.
  * Use `S` to search backwards.
  * Evil-snipe is always literal: `s\*` will jump to a literal `\*`
  * In operator mode, evil-snipe is bound to `z/Z` (inclusive) and `x/X`
    (exclusive). For instance, `dzab` will delete up to and including the 'ab'.
    `x/X` will stop short of the 'ab'.
  * Press `s<Enter>` to repeat the last snipe. `S<enter>` does the inverse.
  * Highlight matches if `evil-snipe-search-highlight` is non-nil.
  * Incrementally highlight matches as you type if
    `evil-snipe-search-incremental-highlight` is non-nil.
  * `evil-snipe-scope` controls the scope of searches. `'line` mimics vim-seek
    while `'visible` or `'buffer` mimics vim-sneak. See variable for other
    options.
  * `evil-snipe-count-scope`
    * If nil, `count` will specify how many times to repeat the command (e.g.
      `3shi` will find the 3rd 'hi').
    * If `'vertical`, snipe uses vim-sneak's vertical scoping. `3shi` will
      find the first 'hi' within 3 columns on any line after the cursor.
    * If `'letters`, the count will set how many characters to accept. `5shello`
      will jump to the first 'hello'.
  * While typing your search characters, press `TAB` to increment the character
    count. e.g. `s<tab><tab>goal` will search for the next instance of "goal".
  * You can use backspace in the character prompt.

### Planned

  * `;` and `,` repeat support
  * `n` and `N` repeat support
  * Vertical-scoping: `5shi` will jump to the next occurance of 'hi' that is
    within 5 columns of the cursor on any following line.
  * Use `r` as an operator or text-object to target remove objects.
  * As an operator, it defaults to word mode. So `drwo` will delete the
    next word containg `wo`.
  * As a text-object, you can specify inner or outer: `dirwo` will
    delete the next INNER word containing `wo`. dorwo will target the next OUTER
    word.
  * Use `R` for reverse remote snipes.
  * Use `p` for the same functionality as `r` except that it stays at the
    destination.

## Credits

Evil-snipe was inspired by:

* [vim-seek](https://github.com/goldfeld/vim-seek)
* [vim-sneak](https://github.com/justinmk/vim-sneak)
* [evil-sneak](https://github.com/AshleyMoni/evil-sneak)
