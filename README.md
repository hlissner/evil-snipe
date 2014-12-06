# evil-snipe

> This is a new (and potentially buggy) plugin, much functionaly hasn't be
> implemented. I'm also an elisp newbie, so any help or contributions would be
> appreciated!

Snipe is an attempt to bring a marriage of
[vim-sneak](https://github.com/justinmk/vim-sneak) and
[vim-seek](https://github.com/goldfeld/vim-seek) to evil-mode.

Everythign it can do can be summaried into two actions: skulking and sniping.
Skulking pertains to finding and jumping to two-character matches. Sniping
pertains to performing actions (yank, delete, change, etc.) on remote words.

**Sniping** (which hasn't been implemented yet) is like vim-seek's
[remote and presential leaps](https://github.com/goldfeld/vim-seek#leaping-motions).
For instance, you can delete a nearby inner word that contains "ev" with
`direv`. That's d for delete, `ir` for inner-remote and `ev` for 'word that
contains `ev`'.

## Installation

Download evil-snipe.el, place it on your loadpath and insert this into your
emacs configuration:

```elisp
   (add-to-list 'load-path "/directory/containing/evil-snipe/")
   (require 'evil-snipe)
   (global-evil-snipe-mode)

   ;; for compatibility with evil-surround and evil-space
   (evil-snipe-surround-compatibility)
```

## Configuration

Skulking is synonymous with f/F and t/T. By default its scope is limited to
the current line (relative to your cursor), and no further. This is consistent
with vim-seek.

If you prefer vim-sneak and its rest-of-buffer-scoped search:

    (setq evil-snipe-scope 'visible)  ;; or 'buffer
    ;; Note: highlighting hasn't been implemented yet.
    (setq evil-snipe-enable-highlight t)
    (setq evil-snipe-enable-incremental-highlight t)

#### vim-sneak's vertical scoping

~I'm not sure if I will try to implement vertical scoping~ I will try to
implement vertical scoping, but horizontal scoping is first on the roadmap. This
means, when enabled, the count will determine the horizontal extent to search.
e.g. `5sev` will search the next 5 lines for ev.

Thsi will be controlled with the variable `evil-snipe-count-scope`.

By default, count is interpreted as is standard in vim: as a repeat count.

#### Original s/S bindings (substitute)

Snipe hijacks the s/S bindings in normal/visual mode (e.g. `s{char]{char}}`,
which belong to 'evil-substitute'. If you miss it, `s` can be accomplished with
`cl` and `S` with `cc`. If that isn't enough, see
`evil-snipe-auto-disable-substitute`.

## Features

* **Skulking**
  * Press sab to move the cursor immediately to the next instance of
    the text `ab`.
  * Press `S` to search backwards.
  * Evil-snipe is always literal! `s\*` will jump to a literal \*
  * Use `z` to use snipe in operator mode. For instance, `dzab` will delete up
    to just before ab (inclusive, by default. Exclusive motion is bound to x/X
    instead of z/Z).
  * Press `ctrl-o` or \`\` to go back to the starting point.
  * [PLANNED] Press `s<Enter>` at any time to repeat the last snipe.
  * [PLANNED] If `evil-snipe-search-highlight` is non-nil, matches are
    highlighted.
  * [PLANNED] If `evil-snipe-search-live-highlight` is non-nil, matches for your
    first key are highlighted as well (like incremental search).

* **Sniping**
  * [PLANNED] Use `r` as an operator or text-object to target remove objects.
  * [PLANNED] As an operator, it defaults to word mode. So `drwo` will delete the
    next word containg `wo`.
  * [PLANNED] As a text-object, you can specify inner or outer: `dirwo` will
    delete the next INNER word containing `wo`. dorwo will target the next OUTER
    word.
  * [PLANNED] Use `R` for reverse remote snipes.
  * [PLANNED] Use `p` for the same functionality as `r` except that it stays at the
    destination.

* **Both**
  * [PLANNED] If `evil-snipe-repeat` is t, jump to next match with ; and ,
    * If it's `'next`, then use s and S.
    * If it's `'search`, then use n and N.
    * If nil, do not pass go. Do not collect $200.

### Compatibility with other evil plugins

* [evil-space](https://github.com/linktohack/evil-space) needs more investigating.
* [evil-surround](https://github.com/timcharper/evil-surround)'s s/S mappings
  override snipe in visual mode. It **does not** affect evil-surround's `s`
  operator though. Snipe uses `z` instead. Perhaps we can use that in visual
  mode as well.

## TODO

* `s<enter>` repeat support
* Highlighting, either ala isearch or ace-jump
* *Incremental* highlighting for first character too
  (`evil-snipe-search-incremental-highlight`)
* `;` and `,` repeating
* Other kinds of repeat support (`evil-snipe-repeat`)
  * `'next` for s/S (like vim-sneak)
  * `'search'` for n/N (like evil-search)
* Horizontal scoping
* Vertical scoping

## Credits

Evil-snipe was inspired by:

* [vim-seek](https://github.com/goldfeld/vim-seek)
* [vim-sneak](https://github.com/justinmk/vim-sneak)
* [evil-sneak](https://github.com/AshleyMoni/evil-sneak)
