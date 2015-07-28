;;; evil-snipe.el --- emulate vim-sneak & vim-seek
;;
;; Copyright (C) 2014-15 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: December 5, 2014
;; Modified: July 3, 2015
;; Version: 1.7.7
;; Keywords: emulation, vim, evil, sneak, seek
;; Homepage: https://github.com/hlissner/evil-snipe
;; Package-Requires: ((evil "1.1.3"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Snipe is f/F/t/T on steroids. It emulates vim-sneak and vim-seek for
;; evil-mode by default, bound to s/S in normal mode and z/Z/x/X in visual or
;; operator mode. With its N-character searching, it can also be adapted to
;; replace evil-mode's f/F/t/T funcitonality. See the readme for more
;; information.
;;
;; To enable globally, add the following lines to ~/.emacs:
;;
;;     (require 'evil-snipe)
;;     (evil-snipe-mode 1)
;;
;; To replace evil-mode's f/F/t/T functionality with (1-character) snipe, use:
;;
;;     (evil-snipe-override-mode 1)
;;
;;; Code:

(require 'evil)
(eval-when-compile (require 'cl-lib))

(defgroup evil-snipe nil
  "vim-seek/sneak emulation for Emacs"
  :prefix "evil-snipe-"
  :group 'evil)

(defcustom evil-snipe-enable-highlight t
  "If non-nil, all matches will be highlighted after the initial jump.
Highlights will disappear as soon as you do anything afterwards, like move the
cursor."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-enable-incremental-highlight t
  "If non-nil, each additional keypress will incrementally search and highlight
matches. Otherwise, only highlight after you've finished skulking."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-override-evil-repeat-keys t
  "If non-nil (while `evil-snipe-override-evil' is non-nil) evil-snipe will
override evil's ; and , repeat keys in favor of its own."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-scope 'line
  "Dictates the scope of searches, which can be one of:

    'line    ;; search line after the cursor (this is vim-seek behavior) (default)
    'buffer  ;; search rest of the buffer after the cursor (vim-sneak behavior)
    'visible ;; search rest of visible buffer (Is more performant than 'buffer, but
             ;; will not highlight/jump past the visible buffer)
    'whole-line     ;; same as 'line, but highlight matches on either side of cursor
    'whole-buffer   ;; same as 'buffer, but highlight *all* matches in buffer
    'whole-visible  ;; same as 'visible, but highlight *all* visible matches in buffer"
  :group 'evil-snipe
  :type 'symbol)

(defcustom evil-snipe-repeat-scope 'whole-line
  "Dictates the scope of repeat searches (see `evil-snipe-scope' for possible
settings)"
  :group 'evil-snipe
  :type 'symbol)

(defcustom evil-snipe-count-scope nil
  "Dictates the scope of searches, which can be one of:

    nil          ;; default; treat count as repeat count
    'letters     ;; count = how many characters to expect and search for
    'vertical    ;; find first match within N (visible) columns"
  :group 'evil-snipe
  :type 'symbol)

(defcustom evil-snipe-spillover-scope nil
  "Takes any value `evil-snipe-scope' accepts. If nil, a failed search will
simply fail. If non-nil, snipe will search for more matches within this scope.
It is useful only if set to a broader scope than `evil-snipe-scope'.

This also applies to N>1 COUNT prefixes. E.g. if 3sab fails, it will extend the
scope to `evil-snipe-spillover-scope''s to find a 3rd match."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-repeat-keys t
  "If non-nil, pressing s/S after a search will repeat it. If
`evil-snipe-override-evil' is non-nil, this applies to f/F/t/T as well."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-show-prompt t
  "If non-nil, show 'N>' prompt while sniping."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-smart-case nil
  "By default, searches are case sensitive. If `evil-snipe-smart-case' is
enabled, searches are case sensitive only if search contains capital
letters."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-auto-scroll nil
  "If non-nil, the window will scroll to follow the cursor."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-symbol-groups '()
  "You specify key aliases here, in the format '(KEY REGEX). Any instance of KEY
will be replaced with REGEX.

Here are some examples:

    ;; Alias [ and ] to all types of brackets
    (add-to-list 'evil-snipe-symbol-groups '(?\\] \"[]})]\"))
    (add-to-list 'evil-snipe-symbol-groups '(?\\[ \"[[{(]\"))
    ;; For python style functions
    (add-to-list 'evil-snipe-symbol-groups '(?\\: \"def .+:\"\))"
  :group 'evil-snipe
  :type 'list)

(defvar evil-snipe-auto-disable-substitute t
  "Disables evil's native s/S functionality (substitute) if non-nil. By default
this is t, since they are mostly redundant with other motions. s can be done
via cl and S with cc (or C).

MUST BE SET BEFORE EVIL-SNIPE IS LOADED.")

(defcustom evil-snipe-skip-leading-whitespace t
  "If non-nil, single char sniping (f/F/t/T) will skip over leading whitespaces
in a line (when you snipe for whitespace, e.g. f<space> or f<tab>)."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-tab-increment nil
  "If non-nil, pressing TAB while sniping will add another character to your
current search. For example, typing sab will search for 'ab'. In order to search
for 'abcd', you do sa<tab><tab>bcd.

However, this interferes with sniping for literal tab characters.

By default, this is set to (not `indent-tabs-mode')."
  :group 'evil-snipe
  :type 'boolean)

(defface evil-snipe-first-match-face
  '((t (:inherit isearch)))
  "Face for first match when sniping"
  :group 'evil-snipe)

(defface evil-snipe-matches-face
  '((t (:inherit region)))
  "Face for other matches when sniping"
  :group 'evil-snipe)

;; State vars ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-snipe--last nil
  "The last search performed.")

(defvar evil-snipe--last-repeat nil
  "Whether the last search was a repeat.")

(defvar evil-snipe--last-direction t
  "Direction of the last search.")

(defvar evil-snipe--consume-match t
  "Whether the search should consume the match or not.")

(defvar evil-snipe--match-count 2
  "Number of characters to match. Can be let-bound to create motions that search
  for N characters. Do not set directly, unless you want to change the default
  number of characters to search.")

(defvar evil-snipe--this-func nil)

(defvar evil-snipe--transient-map-func nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe--case-p (keystr)
  (let ((case-fold-search nil)
        (keystr (if (not (stringp keystr)) (evil-snipe--key-patterns keystr) keystr)))
    (if evil-snipe-smart-case
        (not (string-match-p "[A-Z]" keystr))
      nil)))

(defun evil-snipe--count ()
  (when current-prefix-arg (prefix-numeric-value current-prefix-arg)))

(defun evil-snipe--interactive (&optional how-many)
  (let ((count (evil-snipe--count))
        (evil-snipe--match-count (or how-many 2)))
    (list (evil-snipe--collect-keys count evil-snipe--last-direction))))

(defun evil-snipe--process-key (key)
  (let ((regex-p (assoc key evil-snipe-symbol-groups))
        (keystr (if (characterp key) (char-to-string key) key)))
    (cons keystr
          (if regex-p (elt regex-p 1) (regexp-quote keystr)))))

(defun evil-snipe--process-keys (keys)
  (mapcar 'evil-snipe--process-key keys))

(defun evil-snipe--keys (data)
  (mapconcat 'car data ""))

(defun evil-snipe--key-patterns (data)
  (if (symbolp data)
      ""
    (mapconcat 'cdr data "")))

(defun evil-snipe--collect-keys (&optional count forward-p)
  "The core of evil-snipe's N-character searching. Prompts for
`evil-snipe--match-count' characters, which can be incremented by pressing TAB.
Backspace works for correcting yourself too.

If `evil-snipe-count-scope' is 'letters, N = `count', so 5s will prompt you for
5 characters."
  (let* ((count (or count 1))
         (how-many (if (eq evil-snipe-count-scope 'letters)
                       (or (if count (abs count))
                           evil-snipe--match-count)
                     evil-snipe--match-count))
         (data '())
         (i how-many)
         ;; disable this to suppress keys messing with the prompt
         (echo-keystrokes 0)
         regex-p)
    (unless forward-p
      (setq count (* -1 count)))
    (unwind-protect
        (catch 'abort
          (while (> i 0)
            (let* ((keystr (evil-snipe--keys data))
                   (prompt (if evil-snipe-show-prompt (concat (number-to-string i) ">" keystr) ""))
                   (key (read-event prompt)))
              (cond ((and (eq key 'tab)          ; Tab = adds more characters to search
                          evil-snipe-tab-increment)
                     (setq i (1+ i)))
                    ((eq key 'return)            ; Enter = search with
                     (if (= i how-many)          ;         current characters
                         (throw 'abort 'repeat)
                       (throw 'abort data)))
                    ((eq key 'escape)            ; Escape/C-g = abort
                     (evil-snipe--pre-command)
                     (throw 'abort 'abort))
                    ;; Otherwise, process key
                    (t
                     (cond ((eq key 'backspace)  ; if backspace, delete a character
                            (cl-incf i)
                            (let ((data-len (length data)))
                              (if (<= (length data) 1)
                                  (progn (evil-snipe--pre-command)
                                         (throw 'abort 'abort))
                                (nbutlast data))))
                           (t ;; Otherwise add it
                            (when (eq key 'tab) ; if tab gets this far, add \t
                              (setq key ?\t))
                            (setq regex-p (assoc key evil-snipe-symbol-groups))
                            (setq data (append data (list (evil-snipe--process-key key))))
                            (cl-decf i)))
                     (when evil-snipe-enable-incremental-highlight
                       (save-excursion
                         (when evil-snipe-skip-leading-whitespace
                           (evil-first-non-blank))
                         (evil-snipe--pre-command)
                         (evil-snipe--highlight-all count (evil-snipe--key-patterns data))
                         (add-hook 'pre-command-hook 'evil-snipe--pre-command)))))))
          data))))

(defun evil-snipe--bounds (&optional forward-p count)
  "Returns a cons cell containing (beg . end), which represents the search scope
depending on what `evil-snipe-scope' is set to."
  (let* ((point+1 (1+ (point)))
         (evil-snipe-scope (or (if (and count (> (abs count) 1)) evil-snipe-spillover-scope) evil-snipe-scope))
         (bounds (cl-case evil-snipe-scope
                   ('line
                    (if forward-p
                        `(,point+1 . ,(line-end-position))
                      `(,(line-beginning-position) . ,(point))))
                   ('visible
                    (if forward-p
                        `(,point+1 . ,(1- (window-end)))
                      `(,(window-start) . ,(point))))
                   ('buffer
                    (if forward-p
                        `(,point+1 . ,(point-max))
                      `(,(point-min) . ,(point))))
                   ('whole-line
                    `(,(line-beginning-position) . ,(line-end-position)))
                   ('whole-visible
                    `(,(window-start) . ,(1- (window-end))))
                   ('whole-buffer
                    `(,(point-min) . ,(point-max)))
                   (t
                    (error "Invalid scope: %s" evil-snipe-scope))))
         (end (cdr bounds)))
    (when (> (car bounds) end)
      (setq bounds `(,end . ,end)))
    bounds))

(defun evil-snipe--highlight (beg end &optional first-p)
  "Highlights region between beg and end. If first-p is t, then use
`evil-snipe-first-p-match-face'"
  (if (and first-p (overlays-in beg end))
      (remove-overlays beg end 'category 'evil-snipe))
  (let ((overlay (make-overlay beg end nil nil nil)))
    (overlay-put overlay 'face (if first-p 'evil-snipe-first-match-face 'evil-snipe-matches-face))
    (overlay-put overlay 'category 'evil-snipe)))

(defun evil-snipe--highlight-all (count match)
  "Highlight all instances of `match' ahead of the cursor, or behind it if
`forward-p' is nil."
  (let* ((forward-p (> count 0))
         (bounds (evil-snipe--bounds forward-p))
         (beg (car bounds))
         (end (cdr bounds))
         (beg-offset (+ (point-min) beg -1))
         (string (buffer-substring-no-properties beg end))
         (case-fold-search (evil-snipe--case-p match))
         (i 0))
    (while (and (< i (length string))
                (string-match match string i))
      (when (= (% i count) 0)
        (evil-snipe--highlight (+ beg-offset (match-beginning 0))
                               (+ beg-offset (match-end 0))))
      (setq i (1+ (match-beginning 0))))))

(defun evil-snipe--pre-command ()
  "Disables overlays and cleans up after evil-snipe."
  (when evil-snipe-mode
    (remove-overlays nil nil 'category 'evil-snipe))
  (remove-hook 'pre-command-hook 'evil-snipe--pre-command))

(defun evil-snipe--disable-transient-map ()
  "Disable lingering transient map, if necessary."
  (when (and evil-snipe-mode (functionp evil-snipe--transient-map-func))
    (funcall evil-snipe--transient-map-func)
    (setq evil-snipe--transient-map-func nil)))

(defun evil-snipe--transient-map (forward-key backward-key)
  (let ((map (make-sparse-keymap)))
    ;; So ; and , are common to all sub keymaps
    (define-key map ";" 'evil-snipe-repeat)
    (define-key map "," 'evil-snipe-repeat-reverse)
    (when evil-snipe-repeat-keys
      (define-key map forward-key 'evil-snipe-repeat)
      (define-key map backward-key 'evil-snipe-repeat-reverse))
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-interactive-code "<+c>"
  "Regular count"
  (setq evil-snipe--last-direction t)
  (list (evil-snipe--count)))

(evil-define-interactive-code "<-c>"
  "Inverted count"
  (setq evil-snipe--last-direction nil)
  (let ((count (evil-snipe--count)))
    (list (if count (- count)))))

(evil-define-interactive-code "<1C>"
  (evil-snipe--interactive 1))

(evil-define-interactive-code "<2C>"
  (evil-snipe--interactive 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe-seek (count keys &optional keymap)
  "Perform a snipe. KEYS is a list of characters provided by <-c> and <+c>
interactive codes. KEYMAP is the transient map to activate afterwards."
  (let ((case-fold-search (evil-snipe--case-p keys)))
    (cl-case keys
      ('abort)
      ;; if <enter>, repeat last search
      ('repeat (if evil-snipe--last-direction
                   (evil-snipe-repeat count)
                 (evil-snipe-repeat-reverse count)))
      ;; If KEYS is empty
      ('() (user-error "No keys provided!"))
      ;; Otherwise, perform the search
      (t (let ((count (or count (if evil-snipe--last-direction 1 -1)))
               (keymap (if (keymapp keymap) keymap))
               (data (if (consp (nth 0 keys)) keys (evil-snipe--process-keys keys))))
           (unless evil-snipe--last-repeat
             (setq evil-snipe--last (list count data keymap
                                          evil-snipe--consume-match
                                          evil-snipe--match-count)))
           (cl-case evil-snipe-count-scope
             ;; ('vertical
             ;;  (evil-snipe--seek-vertical count data))
             ('letters
              (evil-snipe--seek (if (> count 0) 1 -1) data))
             (t
              (evil-snipe--seek count data))))))))

(defun evil-snipe--seek (count data)
  "(INTERNAL) Perform a snipe and adjust cursor position depending on mode."
  (when (> (length data) 0)
    (let* (new-orig-point
           (orig-point (point))
           (forward-p (> count 0))
           (string (evil-snipe--key-patterns data))
           (offset (length data))
           (scope (evil-snipe--bounds forward-p count))
           (evil-op-vs-state-p (or (evil-operator-state-p) (evil-visual-state-p))))
      ;; Adjust search starting point
      (if forward-p (forward-char))
      (unless evil-snipe--consume-match (if forward-p (forward-char) (backward-char)))
      (unwind-protect
          (when (and evil-snipe-skip-leading-whitespace
                     (= (length string) 1)
                     (<= orig-point (save-excursion (back-to-indentation) (point))))
            (if forward-p
                (evil-first-non-blank)
              (evil-beginning-of-line)))
          (if (re-search-forward string (if forward-p (cdr scope) (car scope)) t count) ;; hi |
              (let* ((beg (match-beginning 0))
                     (end (match-end 0))
                     (window-start (window-start))
                     (window-end (window-end)))
                ;; Set cursor position
                (if forward-p
                    (progn
                      (goto-char (if evil-op-vs-state-p (1- end) beg))
                      (unless evil-snipe--consume-match (backward-char offset)))
                  (goto-char (if evil-snipe--consume-match beg end)))
                ;; Highlight first result (except when in operator/visual mode)
                (when (and (not evil-op-vs-state-p) evil-snipe-enable-highlight)
                  (evil-snipe--highlight beg end t))
                ;; Follow the cursor
                (when evil-snipe-auto-scroll
                  (setq new-orig-point (point))
                  (if (or (> window-start new-orig-point)
                          (< window-end new-orig-point))
                      (evil-scroll-line-to-center (line-number-at-pos))
                    (evil-scroll-line-down (- (line-number-at-pos) (line-number-at-pos orig-point))))
                  (goto-char new-orig-point))
                ;; Activate the repeat keymap
                (when (and keymap (not (evil-operator-state-p)))
                  (setq evil-snipe--transient-map-func (set-transient-map keymap))))
            (goto-char orig-point)
            (user-error "Can't find %s" ;; show invisible keys
                        (replace-regexp-in-string "\t" "<TAB>"
                        (replace-regexp-in-string "\s" "<SPC>" (evil-snipe--keys data)))))
          (when evil-snipe-enable-highlight
            (evil-snipe--highlight-all count string))
          (add-hook 'pre-command-hook 'evil-snipe--pre-command)))))

;; TODO Implement evil-snipe--seek-vertical
(defun evil-snipe--seek-vertical (count keys)
  (error "Not implemented!"))

(evil-define-motion evil-snipe-repeat (count)
  "Repeat the last evil-snipe `count' times"
  :jump t
  :type inclusive
  (interactive "<+c>")
  (if (listp evil-snipe--last)
      (let ((evil-snipe--last-repeat t)
            (count (or count 1))
            (evil-snipe-scope (or evil-snipe-repeat-scope evil-snipe-scope))
            (evil-snipe--consume-match (nth 3 evil-snipe--last))
            (evil-snipe--match-count (nth 4 evil-snipe--last)))
        (evil-snipe-seek (* count (nth 0 evil-snipe--last))   ;;count
                        (nth 1 evil-snipe--last)
                        (nth 2 evil-snipe--last)))          ;;keys
    (user-error "Nothing to repeat")))

(evil-define-motion evil-snipe-repeat-reverse (count)
  :jump t
  :type inclusive
  "Repeat the inverse of the last evil-snipe `count' times"
  (interactive "<-c>")
  (evil-snipe-repeat (or count -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s/S

(evil-define-motion evil-snipe-s (count keys)
  "Jump to the next N-character match `count' times. N is specified by
`evil-snipe--match-count', which is 2 by default.

COUNT is how many times to repeat the snipe (behaves differently depending on
`evil-snipe-count-scope')
KEYS is a list of character codes or strings."
  :jump t
  :type inclusive
  (interactive "<+c><2C>")
  (evil-snipe-seek count keys evil-snipe-mode-s-map))

(evil-define-motion evil-snipe-S (count keys)
  "Performs a reverse `evil-snipe-s'"
  :jump t
  :type inclusive
  (interactive "<-c><2C>")
  (evil-snipe-s count keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x/X (exclusive s/S in operator mode)

(evil-define-motion evil-snipe-x (count keys)
  "Performs an exclusive `evil-snipe-s'"
  :jump t
  :type inclusive
  (interactive "<+c><2C>")
  (let ((evil-snipe--consume-match nil))
    (evil-snipe-seek count keys evil-snipe-mode-x-map)))

(evil-define-motion evil-snipe-X (count keys)
  "Performs an backwards, exclusive `evil-snipe-S'"
  :jump t
  :type inclusive
  (interactive "<-c><2C>")
  (evil-snipe-x count keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f/F

(evil-define-motion evil-snipe-f (count keys)
  "Jump forward to next match of {char}"
  :jump t
  :type inclusive
  (interactive "<+c><1C>")
  (let ((evil-snipe-count-scope nil))
    (evil-snipe-seek count keys evil-snipe-mode-f-map)))

(evil-define-motion evil-snipe-F (count keys)
  "Jump forward to next match of {char}"
  :jump t
  :type inclusive
  (interactive "<-c><1C>")
  (evil-snipe-f count keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; t/T

(evil-define-motion evil-snipe-t (count keys)
  "Jump forward to next match of {char} (exclusive)"
  :jump t
  :type inclusive
  (interactive "<+c><1C>")
  (let ((evil-snipe--consume-match nil))
    (evil-snipe-seek count keys evil-snipe-mode-t-map)))

(evil-define-motion evil-snipe-T (count keys)
  "Jump forward to next match of {char} (exclusive)"
  :jump t
  :type inclusive
  (interactive "<-c><1C>")
  (evil-snipe-t count keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe-add-alias (char pattern)
  "Set a character alias for sniping. See `evil-snipe-symbol-groups'."
  (add-to-list 'evil-snipe-symbol-groups `(,char ,pattern)))

(defvar evil-snipe-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion map "s" 'evil-snipe-s)
    (evil-define-key 'motion map "S" 'evil-snipe-S)
    (evil-define-key 'operator map "z" 'evil-snipe-s)
    (evil-define-key 'operator map "Z" 'evil-snipe-S)
    (evil-define-key 'operator map "x" 'evil-snipe-x)
    (evil-define-key 'operator map "X" 'evil-snipe-X)

    (when evil-snipe-auto-disable-substitute
      ;; Disable s/S (substitute)
      (define-key evil-normal-state-map "s" nil)
      (define-key evil-normal-state-map "S" nil))
    map))

(defvar evil-snipe-override-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion map "f" 'evil-snipe-f)
    (evil-define-key 'motion map "F" 'evil-snipe-F)
    (evil-define-key 'motion map "t" 'evil-snipe-t)
    (evil-define-key 'motion map "T" 'evil-snipe-T)

    (when evil-snipe-override-evil-repeat-keys
      (evil-define-key 'motion map ";" 'evil-snipe-repeat)
      (evil-define-key 'motion map "," 'evil-snipe-repeat-reverse))
  map))

(unless (fboundp 'set-transient-map)
  (defalias 'set-transient-map 'set-temporary-overlay-map))

(defvar evil-snipe-mode-s-map (evil-snipe--transient-map "s" "S"))
(defvar evil-snipe-mode-x-map (evil-snipe--transient-map "x" "X"))
(defvar evil-snipe-mode-f-map (evil-snipe--transient-map "f" "F"))
(defvar evil-snipe-mode-t-map (evil-snipe--transient-map "t" "T"))

;;;###autoload
(define-minor-mode evil-snipe-mode
  "evil-snipe minor mode."
  :global t
  :lighter " snipe"
  :keymap evil-snipe-mode-map
  :group 'evil-snipe
  (if evil-snipe-mode
      (turn-on-evil-snipe-mode t)
    (turn-off-evil-snipe-mode t)))

;;;###autoload
(define-minor-mode evil-snipe-override-mode
  "evil-snipe minor mode that overrides evil-mode f/F/t/T/;/, bindings."
  :global t
  :keymap evil-snipe-override-mode-map
  :group 'evil-snipe)

;;;###autoload
(defun turn-on-evil-snipe-mode (&optional internal)
  "Enable evil-snipe-mode in the current buffer."
  (unless internal (evil-snipe-mode 1))
  (when (fboundp 'advice-add)
    (advice-add 'evil-force-normal-state :before 'evil-snipe--pre-command))
  (add-hook 'evil-insert-state-entry-hook 'evil-snipe--disable-transient-map))

;;;###autoload
(defun turn-off-evil-snipe-mode (&optional internal)
  "Disable evil-snipe-mode in the current buffer."
  (when (fboundp 'advice-remove)
    (advice-remove 'evil-force-normal-state 'evil-snipe--pre-command))
  (remove-hook 'evil-insert-state-entry-hook 'evil-snipe--disable-transient-map)
  (unless internal (evil-snipe-mode -1))
  (evil-snipe-override-mode -1))


(provide 'evil-snipe)
;;; evil-snipe.el ends here
