[![Made with Doom Emacs](https://img.shields.io/badge/Made_with-Doom_Emacs-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)](https://github.com/hlissner/doom-emacs)
![Release tag](https://img.shields.io/github/tag/hlissner/evil-snipe.svg?label=release&style=flat-square)
[![MELPA](http://melpa.org/packages/evil-snipe-badge.svg?style=flat-square)](http://melpa.org/#/evil-snipe)
![Build status](https://img.shields.io/github/workflow/status/hlissner/evil-snipe/CI/master?style=flat-square)

# evil-snipe

![Sniper!](../screenshots/cover.jpg)

Evil-snipe emulates [vim-seek](https://github.com/goldfeld/vim-seek) and/or
[vim-sneak](https://github.com/justinmk/vim-sneak) in
[evil-mode](https://gitorious.org/evil/pages/Home).

It provides 2-character motions for quickly (and more accurately) jumping around
text, compared to evil's built-in f/F/t/T motions, incrementally highlighting
candidate targets as you type.

## Install

Evil-snipe is available on MELPA.

`M-x package-install evil-snipe`

```emacs-lisp
(require 'evil-snipe)
```

`evil-snipe` comes with two global modes: `evil-snipe-mode` and
`evil-snipe-override-mode`, and two local modes: `evil-snipe-local-mode` and
`evil-snipe-override-local-mode`.

You can either a) enable one or both globally:

```elisp
(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

;; and disable in specific modes
(push 'python-mode evil-snipe-disabled-modes)

;; or disable it manually
(add-hook 'python-mode-hook #'turn-off-evil-snipe-mode)
(add-hook 'python-mode-hook #'turn-off-evil-snipe-override-mode)
```

Or b) enable one or both locally, where you need it:

```elisp
(add-hook 'python-mode-hook 'turn-on-evil-snipe-mode)
(add-hook 'python-mode-hook 'turn-on-evil-snipe-override-local-mode)
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
(add-hook 'ruby-mode-hook 'evil-snipe-override-local-mode)
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

Specific characters can be aliased to regex patterns by modifying `evil-snipe-aliases`.

#### Examples

* To map <kbd>[</kbd> to any opening parentheses or bracket **in all modes**:

  ```elisp
  (push '(?\[ "[[{(]") evil-snipe-aliases)
  ```

  Therefore, <kbd>s</kbd><kbd>a</kbd><kbd>[</kbd> will match `a[`, `a{` or `a(`

* To map <kbd>:</kbd> to a python function (but only in `python-mode`):

  ```elisp
  (add-hook 'python-mode-hook
    (lambda ()
      (make-variable-buffer-local 'evil-snipe-aliases)
      (push '(?: "def .+:") evil-snipe-aliases)))
  ```

### Faces

* `evil-snipe-first-match-face`: The first highlighted match.
* `evil-snipe-matches-face`: The rest of the highlighted matches.

### Sniping in visual mode

To avoid binding conflicts, evil-snipe has no visual mode bindings. You can add
them with:

```elisp
(evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
(evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S)
```

### Integration into avy/evil-easymotion

This will allow you to quickly hop into avy/evil-easymotion right after a snipe.

```elisp
(define-key evil-snipe-parent-transient-map (kbd "C-;")
  (evilem-create 'evil-snipe-repeat
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))
```

(Thanks to [PythonNut](https://github.com/PythonNut) for this.
[More info here](https://github.com/hlissner/evil-snipe/issues/25#issuecomment-208068419))

## Conflicts with other plugins

It seems `evil-snipe-override-mode` causes problems in Magit buffers, to fix this:

`(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)`

## Appendix

### Other settings

* `evil-snipe-enable-highlight` (default: `t`) Highlight first match.
* `evil-snipe-enable-incremental-highlight` (default: `t`) Incrementally highlight all
  matches in scope.
* `evil-snipe-override-evil-repeat-keys` (default: `t`) Whether or not evil-snipe will
  override evil's default <kbd>;</kbd> and <kbd>,</kbd> mappings with snipe's (when
  `evil-snipe-override-mode` is on).
* `evil-snipe-repeat-keys` (default `t`) If non-nil, pressing <kbd>s</kbd>/<kbd>S</kbd>
  after a search will repeat it. If `evil-snipe-override-evil` is non-nil, this applies
  to f/F/t/T as well.
* `evil-snipe-show-prompt` (default `t`) Whether or not to show the "N>" prompt.
* `evil-snipe-smart-case` (default `nil`) If non-nil, searches will be case-insenstive
  unless your search contains a capital letter.
* `evil-snipe-auto-scroll` (default `nil`) If non-nil, the window will scroll to follow
  the cursor.
* `evil-snipe-auto-disable-substitute` (default: `t`) Whether or not evil's
  default substitute mappings (s/S) are unset. They can sometimes interfere with
  snipe. Must be set _before_ evil-snipe is loaded.
* `evil-snipe-skip-leading-whitespace` (default `t`) If non-nil, sniping will skip over
  leading whitespace when you search for whitespace.
* `evil-snipe-tab-increment` (default `nil`) If non-nil, pressing TAB in the snipe
  prompt will increase the size of the snipe buffer.
* `evil-snipe-use-vim-sneak-bindings` (default `nil`) If non-nil, evil-snipe
  binds z/Z to exclusive sniping in operator state, but leaves the x/X bindings
  free. This mirrors the default bindings of vim-sneak, and frees up cx/cX to be
  used by [evil-exchange](https://github.com/Dewdrops/evil-exchange).

### Functions

* `evil-snipe-mode` / `evil-snipe-local-mode`
* `evil-snipe-override-mode` / `evil-snipe-override-local-mode`
* `evil-snipe-repeat` / `evil-snipe-repeat-reverse`
* `evil-snipe-s` / `evil-snipe-S`: inclusive 2-char sniping
* `evil-snipe-x` / `evil-snipe-X`: exclusive 2-char sniping
* `evil-snipe-f` / `evil-snipe-F`: inclusive 1-char sniping
* `evil-snipe-t` / `evil-snipe-T`: exclusive 1-char sniping

### Default keybindings

```elisp
(evil-define-key '(normal motion) evil-snipe-local-mode-map
  "s" 'evil-snipe-s
  "S" 'evil-snipe-S)

(evil-define-key 'operator evil-snipe-local-mode-map
  "z" 'evil-snipe-s
  "Z" 'evil-snipe-S
  "x" 'evil-snipe-x
  "X" 'evil-snipe-X)

(evil-define-key 'motion evil-snipe-override-local-mode-map
  "f" 'evil-snipe-f
  "F" 'evil-snipe-F
  "t" 'evil-snipe-t
  "T" 'evil-snipe-T)

(when evil-snipe-override-evil-repeat-keys
  (evil-define-key 'motion map
    ";" 'evil-snipe-repeat
    "," 'evil-snipe-repeat-reverse))
```
