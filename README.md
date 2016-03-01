# evil-snipe
[![MIT](https://img.shields.io/badge/license-MIT-green.svg)](./LICENSE)
[![MELPA](http://melpa.org/packages/evil-snipe-badge.svg)](http://melpa.org/#/evil-snipe)

Evil-snipe emulates [vim-seek](https://github.com/goldfeld/vim-seek) and/or
[vim-sneak](https://github.com/justinmk/vim-sneak) in
[evil-mode](https://gitorious.org/evil/pages/Home).

It provides 2-character motions for quickly (and more accurately) jumping around
text, compared to evil's built-in f/F/t/T motions, incrementally highlighting
candidate targets as you type.

## Install

Evil-snipe is available on MELPA.

`M-x package-install evil-snipe`

Then enable it globally with:

```elisp
(require 'evil-snipe)
(evil-snipe-mode 1)
```

Or in specific modes:

```elisp
(add-hook 'python-mode 'evil-snipe-local-mode)
```

## Usage

By default, snipe only binds <kbd>s</kbd> (forward)/<kbd>S</kbd> (backward) to
`evil-snipe-s` and `evil-snipe-S`, respectively. In operator mode, snipe is
bound to <kbd>z</kbd>/<kbd>Z</kbd> and <kbd>x</kbd>/<kbd>X</kbd> (exclusive).

The last snipe can be repeated with <kbd>s</kbd>/<kbd>S</kbd> after a successful snipe
(or with <kbd>s</kbd>+<kbd>RET</kbd>).

Evil-snipe can override evil-mode's native motions with 1-char sniping:

```elisp
;; Globally
(evil-snipe-override-mode 1)

;; Or locally
(add-hook 'ruby-mode 'evil-snipe-override-local-mode)
```

The benefit of this is:

* Incremental highlighting
* You can repeat searches with <kbd>f</kbd>, <kbd>F</kbd>, <kbd>t</kbd> and
<kbd>T</kbd> (ala [Clever-F](https://github.com/rhysd/clever-f.vim))
* <kbd>;</kbd> and <kbd>,</kbd> are available for repeating searches (and won't
  interfere with the original maps; they take effect only after a successful snipe)
* A more streamlined experience in general

## Customization

### Search scope

These three variables determine the scope of snipes (and the incremental
highlighter):

* `evil-snipe-scope` (default: `line`)
* `evil-snipe-repeat-scope` (default: `whole-line`) Scope while _repeating_
  searches with `evil-snipe-repeat` or `evil-snipe-repeat-reverse`.
* `evil-snipe-spillover-scope` (default: `nil`) Scope to expand to when a snipe
  fails. Only useful if set to a broader scope than `evil-snipe-scope`.

These are the possible settings:

Value            | Description
-----------------|------------------------------------------------------------
'line            | rest of the current line after cursor (`vim-seek` behavior)
'buffer          | rest of the buffer after cursor (`vim-sneak` behavior)
'visible         | the rest of the _visible_ buffer after cursor
'whole-line      | same as `'line`, but highlights on either side of cursor
'whole-buffer    | same as `'buffer`, but highlights *all* matches in buffer
'whole-visible   | same as `'visible`, but highlights *all* visible matches in buffer

### Character aliases

Specific characters can be aliased to regex patterns using
`(evil-snipe-add-alias CHAR PATTERN)`.

#### Examples

* To map <kbd>[</kbd> to any opening parentheses or bracket:

  ```elisp
  (evil-snipe-add-alias ?[ "[[{(]")
  ```

  Therefore, <kbd>s</kbd><kbd>a</kbd><kbd>[</kbd> will match `a[`, `a{` or `a(`

* Or <kbd>:</kbd> to a python function:

  ```elisp
  (evil-snipe-add-alias ?: "def .+:")
  ```

### N-Char search

`evil-snipe-count-scope` (default: `nil`) will control how evil-snipe treats
a COUNT prefix. If set to `'letter'`, then COUNT determines how many characters
snipe will accept. e.g.
<kbd>4</kbd><kbd>s</kbd><kbd>a</kbd><kbd>b</kbd><kbd>c</kbd><kbd>d</kbd>.

If nil, COUNT determines how many matches to jump forward (or backward).

### Faces

* `evil-snipe-first-match-face`: The first highlighted match.
* `evil-snipe-matches-face`: The rest of the highlighted matches.

### Sniping in visual mode

To avoid binding conflicts, evil-snipe has no visual mode bindings. You can add
them with:

```elisp
(evil-define-key 'visual evil-snipe-mode-map "z" 'evil-snipe-s)
(evil-define-key 'visual evil-snipe-mode-map "Z" 'evil-snipe-S)
```

## Conflicts with other plugins

It seems `evil-snipe-override-mode` causes problems in Magit buffers, to fix this:

`(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)`

## Appendix

### Other settings

* `evil-snipe-enable-highlight` (default: `t`) Highlight search
* `evil-snipe-enable-incremental-highlight` (default: `t`) Highlight as-you-type
* `evil-snipe-override-evil-repeat-keys` (default: `t`) Whether or not evil-snipe will
  override evil's default <kbd>;</kbd> and <kbd>,</kbd> mappings with snipe's (when
  `evil-snipe-override-mode` is on).
* `evil-snipe-repeat-keys` (default `t`) If non-nil, pressing <kbd>s</kbd>/<kbd>S</kbd>
  after a search will repeat it. If `evil-snipe-override-evil` is non-nil, this applies
  to f/F/t/T as well.
* `evil-snipe-show-prompt` (default `t`) Whether or not to show the "N>" prompt, where
  N is the number of characters left that snipe is waiting for.
* `evil-snipe-smart-case` (default `t`) If non-nil, searches will be case-insenstive
  unless any of the keys are capitalized.
* `evil-snipe-auto-scroll` (default `nil`) If non-nil, the window will scroll to follow
  the cursor.
* `evil-snipe-auto-disable-substitute` (default: `t`) Whether or not evil's default
  substitute mappings (s/S) are unset. They can sometimes interfere with snipe.
* `evil-snipe-skip-leading-whitespace` (default `t`) If non-nil, single char sniping
  (f/F/t/T) will skip over leading whitespaces in a line (when you snipe for
  whitespace).
* `evil-snipe-tab-increment` (default `nil`) If non-nil, pressing TAB in the snipe
  prompt will increment the maximum characters for your current search.

### Functions

* `evil-snipe-mode` / `evil-snipe-local-mode`
* `evil-snipe-override-mode` / `evil-snipe-override-local-mode`
* `evil-snipe-repeat` / `evil-snipe-repeat-reverse`
* `evil-snipe-s` / `evil-snipe-S`: inclusive 2-char sniping
* `evil-snipe-x` / `evil-snipe-X`: exclusive 2-char sniping
* `evil-snipe-f` / `evil-snipe-F`: inclusive 1-char sniping
* `evil-snipe-t` / `evil-snipe-T`: exclusive 1-char sniping
* `evil-snipe-add-alias`

### Default keybindings

```elisp
(evil-define-key 'motion evil-snipe-mode-map "s" 'evil-snipe-s)
(evil-define-key 'motion evil-snipe-mode-map "S" 'evil-snipe-S)
(evil-define-key 'operator evil-snipe-mode-map "z" 'evil-snipe-s)
(evil-define-key 'operator evil-snipe-mode-map "Z" 'evil-snipe-S)
(evil-define-key 'operator evil-snipe-mode-map "x" 'evil-snipe-x)
(evil-define-key 'operator evil-snipe-mode-map "X" 'evil-snipe-X)

(evil-define-key 'motion evil-snipe-override-mode-map "f" 'evil-snipe-f)
(evil-define-key 'motion evil-snipe-override-mode-map "F" 'evil-snipe-F)
(evil-define-key 'motion evil-snipe-override-mode-map "t" 'evil-snipe-t)
(evil-define-key 'motion evil-snipe-override-mode-map "T" 'evil-snipe-T)

(when evil-snipe-override-evil-repeat-keys
  (evil-define-key 'motion map ";" 'evil-snipe-repeat)
  (evil-define-key 'motion map "," 'evil-snipe-repeat-reverse))
```
