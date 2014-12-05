# evil-snipe

> This is a new (and potentially buggy) plugin, much functionaly hasn't be
> implemented. I'm also an elisp newbie, so any help or contributions would be
> appreciated!

Snipe is an attempt to bring a marriage of
[vim-sneak](https://github.com/justinmk/vim-sneak) and
[vim-seek](https://github.com/goldfeld/vim-seek) to evil-mode.

Specifically, it brings two functions to evil: skulking and sniping. Skulking
pertains to finding and jumping to two-character matches. Sniping pertains to
performing actions (yank, delete, change, etc.) on remote words, far from the
cursor.

**Skulking** is synonymous with f/F and t/T. By default (like vim-seek and f/F/t/T),
it only search for matches on the same line relative to (point). If you
prefer buffer-wide search, see `evil-snipe-bounds`.

**Sniping**, however, is like vim-seek's
[remote and presential leaps](https://github.com/goldfeld/vim-seek#leaping-motions).
For instance, you can delete a nearby word that contains "ev" with `direv`.
That's d for delete, `ir` for inner-remote and `ev` for 'word that contains
`ev`.

<!-- You can turn either off by setting either `evil-snipe-enable-skulking` and -->
<!-- `evil-snipe-enable-sniping` to nil. -->

## Installation

```elisp
   (add-to-list 'load-path "/directory/containing/evil-snipe")
   (require 'evil-snipe)
   (global-evil-snipe-mode)

   ;; for compatibility with evil-surround and evil-space
   (evil-snipe-surround-compatibility)
```

## Configuration

### vim-seek

If you preferred vim-seek, or would simply like a line-bound search, the default
settings should suite you. For posterity, this is how you configure evil-snipe
to act like vim-seek:

    (setq evil-snipe-bounds 'line)
    (setq evil-snipe-live-search nil)
    (setq evil-snipe-search-incremental-highlight nil)


### vim-sneak

    (setq evil-snipe-bounds 'visible)  ;; or use 'buffer
    ;; Note: highlighting hasn't been implemented yet.
    (setq evil-snipe-search-highlight t)
    (setq evil-snipe-search-incremental-highlight t)

#### Vertical Scoping

If you're interested in vim-sneak's vertical scoping, I'm not sure if I will try
to implement it. However, I *will* implement horizontal scoping. Meaning the
count will determine the horizontal extent to search. e.g. `5sev` will search
the next 5 lines for ev.

By default, count is interpreted as is standard in vim: as a repeat count.

#### Original s/S bindings (substitute)

Snipe hijacks the s/S bindings in normal/visual mode (e.g. `s{char]{char}}`. If
you don't like this, see `evil-snipe-auto-disable-substitute`.

## Features

* **Skulking**
  * Press sab to move the cursor immediately to the next instance of
    the text `ab`.
  * Press dzab to delete up to just before ab (inclusive, by default. Exclusive
    motion is bound to x/X instead of z/Z)
  * Press `ctrl-o` or \`\` to go back to the starting point.
  * [PLANNED] Press `s<Enter>` at any time to repeat the last snipe.
  * Press `S` to search backwards.
  * evil-snipe is always literal! `s\*` will jump to a literal \*
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

### Inconsistencies with vim-seek/sneak

* Vertically scoped searches (e.g. 5sxy) are not implemented. Instead,
  the number is treated as a count as per normal vim conventions. E.g.
  5sxy will jump to the 5th match of "xy". Unless `evil-snipe-bounds' is
  set to 'count, in which case it's horizontally scoped, not vertically.

### Compatibility with other evil plugins

* [evil-space](https://github.com/linktohack/evil-space) needs more investigating.
* [evil-surround](https://github.com/timcharper/evil-surround)'s s/S mappings
  override snipe in visual mode. Up to you how you want to deal with that.

## TODO

* s<enter> repeat support
* Evil-snipe-t/T motions (phase out evil-snipe-lazy setting)
* Evil-snipe-enable-* options to disable skulking or sniping
* Highlighting, either ala isearch or ace-jump
* *Incremental* highlighting for first character too
  (`evil-snipe-search-incremental-highlight`)
* `;` and `,` repeating
* Other kinds of repeat support (`evil-snipe-repeat`)
  * `'next` for s/S (like vim-sneak)
  * `'search'` for n/N (like evil-search)

## Credits

Evil-snipe was inspired by:

* [vim-seek](https://github.com/goldfeld/vim-seek)
* [vim-sneak](https://github.com/justinmk/vim-sneak)
* [evil-sneak](https://github.com/AshleyMoni/evil-sneak)
