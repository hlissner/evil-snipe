;;; evil-snipe.el --- emulate vim-sneak & vim-seek
;;
;; Copyright (C) 2014 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: December 5 2014
;; Modified: December 25, 2014
;; Version: 1.5.2
;; Keywords: emulation, vim, evil, sneak, seek
;; Homepage: https://github.com/hlissner/evil-snipe
;; Package-Requires: ((evil "1.0.9"))
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
;;     (global-evil-snipe-mode 1)
;;
;; To replace evil-mode's f/F/t/T functionality with (1-character) snipe, use:
;;
;;     (evil-snipe-replace-evil)
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

(defcustom evil-snipe-enable-half-cursor t
  "Whether or not to activate half-cursor mode on activation of evil-snipe (for
more visual feedback)."
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

(defcustom evil-snipe-show-prompt t
  "If non-nil, show 'N>' prompt while sniping."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-enable-sS nil
  "Allows s/S for repeating searches if non-nil. Automatically set to t if
`evil-snipe-replace-evil' is used."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-enable-nN nil
  "Whether to enable n/N for repeating searches"
  :group 'evil-snipe
  :type 'boolean)


(defvar evil-snipe-auto-disable-substitute t
  "Disables evil's native s/S functionality (substitute) if non-nil. By default
this is t, since they are mostly redundant with other motions. s can be done
via cl and S with cc.

MUST BE SET BEFORE EVIL-SNIPE IS LOADED.")

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
  "The last search performed.")

(defvar evil-snipe--last-direction t
  "Direction of the last search")

(defvar evil-snipe--consume-match t
  "The last search performed.")

(defvar evil-snipe--match-count 2
  "Number of characters to match. Can be let-bound to create motions that search
  for N characters. Do not set directly, unless you want to change the default
  number of characters to search.")

(defvar evil-snipe--this-func nil)

(defvar evil-snipe--transient-map-func nil)

(defvar evil-snipe--transient-common-map-func nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe--count ()
  (when current-prefix-arg (prefix-numeric-value current-prefix-arg)))

(defun evil-snipe--interactive (&optional how-many)
  (let ((count (evil-snipe--count))
        (evil-snipe--match-count (or how-many 2)))
    (list (evil-snipe--collect-keys count evil-snipe--last-direction))))

(defun evil-snipe--collect-keys (&optional count forward-p)
  "The core of evil-snipe's N-character searching. Prompts for
`evil-snipe--match-count' characters, which can be incremented by pressing TAB.
Backspace works for correcting yourself too.

If `evil-snipe-count-scope' is 'letters, N = `count', so 5s will prompt you for
5 characters."
  (let* ((how-many (if (eq evil-snipe-count-scope 'letters)
                       (or (if count (abs count))
                           evil-snipe--match-count)
                     evil-snipe--match-count))
         (keys '())
         (i how-many))
    (unless (or (evil-operator-state-p) (not evil-snipe-enable-half-cursor))
      (evil-half-cursor))
    (catch 'abort
      (while (> i 0)
        (let* ((prompt (if evil-snipe-show-prompt (concat (number-to-string i) ">" keys) ""))
               (key (evil-read-key prompt)))
          (cond ((char-equal key ?\t)         ; Tab = adds more characters to search
                 (setq i (1+ i)))
                ((or (char-equal key ?\n)
                     (char-equal key 13))
                 (if (= i how-many)
                     (throw 'abort 'repeat)
                   (throw 'abort keys)))
                ((or (char-equal key ?\C-\[)
                     (char-equal key ?\C-g))  ; Escape/C-g = abort
                 (throw 'abort 'abort))
                (t (if (char-equal key ?\^?)
                        (progn
                          (when (= i how-many) (throw 'abort 'abort))
                          (setq i (1+ i))
                          (when (= i how-many) (pop keys)))
                      (setq keys (append keys `(,key)))
                      (setq i (1- i)))
                    (when evil-snipe-enable-incremental-highlight
                      (evil-snipe--highlight-clear)
                      (evil-snipe--highlight-rest (concat keys) forward-p)
                      (add-hook 'pre-command-hook 'evil-snipe--highlight-clear))))))
      keys)))

(defun evil-snipe--bounds (&optional forward-p)
  "Returns a cons cell containing (beg . end), which represents the search scope
depending on what `evil-snipe-scope' is set to."
  (let ((point+1 (1+ (point))))
    (cl-case evil-snipe-scope
      ('line
       (if forward-p
           `(,point+1 . ,(line-end-position))
         `(,(line-beginning-position) . ,(point))))
      ('visible
       (if forward-p
           `(,point+1 . ,(window-end))
         `(,(window-start) . ,(point))))
      ('buffer
       (if forward-p
           `(,point+1 . ,(point-max))
         `(,(point-min) . ,(point))))
      ('whole-line
       `(,(line-beginning-position) . ,(line-end-position)))
      ('whole-visible
       `(,(window-start) . ,(window-end)))
      ('whole-buffer
       `(,(point-min) . ,(point-max)))
      (t
       (error "Invalid scope: %s" evil-snipe-scope)))))

(defun evil-snipe--highlight (beg end &optional first)
  (if (and first (overlays-in beg end))
      (remove-overlays beg end 'category 'evil-snipe))
  (unless (overlays-in beg end)
    (let ((x (make-overlay beg end)))
      (overlay-put x 'face (if first 'evil-snipe-first-match-face 'evil-snipe-matches-face))
      (overlay-put x 'category 'evil-snipe))))

(defun evil-snipe--highlight-rest (match forward-p)
  (let* ((bounds (evil-snipe--bounds forward-p))
         (beg (car bounds))
         (end (cdr bounds))
         (string (buffer-substring-no-properties beg end))
         (beg-offset (+ (point-min) beg -1))
         (i 0))
    (while (and (< i (length string))
                (string-match (regexp-quote match) string i))
      ;; TODO Apply column-bound highlighting
      (setq i (1+ (match-beginning 0)))
      (evil-snipe--highlight (+ beg-offset (match-beginning 0))
                             (+ beg-offset (match-end 0))))))

(defun evil-snipe--highlight-clear ()
  (remove-overlays nil nil 'category 'evil-snipe)
  (remove-hook 'pre-command-hook 'evil-snipe--highlight-clear))

(defun evil-snipe--disable-transient-map ()
  (when (functionp evil-snipe--transient-map-func)
    (funcall evil-snipe--transient-map-func)
    (setq evil-snipe--transient-map-func nil))
  (when (functionp evil-snipe--transient-common-map-func)
    (funcall evil-snipe--transient-common-map-func)
    (setq evil-snipe--transient-common-map-func nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe--seek (count string scope-beg scope-end)
  (when (> (length string) 0)
    (let ((fwdp (> count 0))
          (orig-point (point))
          (type (evil-visual-type))
          (skip-pad (length string))
          (evil-op-vs-state-p (or (evil-operator-state-p) (evil-visual-state-p))))
      ;; Adjust search starting point
      (if fwdp
          (forward-char (if evil-snipe--consume-match 1 2))
        (unless evil-snipe--consume-match (backward-char 1)))
      (when evil-snipe-enable-highlight
        (evil-snipe--highlight-rest string fwdp))
      (if (search-forward string (if fwdp scope-end scope-beg) t count) ;; hi |
          (progn
            (when fwdp (backward-char skip-pad))    ;; hi | => h|i
            (when (and (not evil-op-vs-state-p) evil-snipe-enable-highlight)
              (evil-snipe--highlight (point) (+ (point) skip-pad) t)
              (add-hook 'pre-command-hook 'evil-snipe--highlight-clear))
            ;; Adjustments for operator/visual mode
            (if evil-op-vs-state-p                ;; d{?}hi
              (if fwdp
                  (progn
                    (backward-char)               ;; h|i => |hi
                    (if evil-snipe--consume-match
                        (forward-char skip-pad))) ;; hi| (z)
                (unless evil-snipe--consume-match
                  (forward-char skip-pad)))       ;; hi| (X)
              (unless evil-snipe--consume-match
                (forward-char (if fwdp (- skip-pad) skip-pad)))))
        (goto-char orig-point)
        (user-error "Can't find %s" string)))))

;; TODO Implement evil-snipe--seek-vertical
(defun evil-snipe--seek-vertical (count string scope-beg scope-end)
  (error "Not implemented"))

(evil-define-command evil-snipe-repeat (count)
  "Repeat the last evil-snipe `count' times"
  (interactive "<c>")
  (if (listp evil-snipe--last)
      (let ((evil-snipe--last-repeat t)
            (count (or count 1))
            (evil-snipe-scope (or evil-snipe-repeat-scope evil-snipe-scope)))
        (funcall (first evil-snipe--last)            ;;func name
                 (* count (nth 1 evil-snipe--last))  ;;count
                 (nth 2 evil-snipe--last)))          ;;keys
    (user-error "No search to repeat")))

(evil-define-command evil-snipe-repeat-reverse (count)
  "Repeat the inverse of the last evil-snipe `count' times"
  (interactive "<c>")
  (evil-snipe-repeat (if count (- count) -1)))

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

(evil-define-interactive-code "<2C>"
  (evil-snipe--interactive 2))

(evil-define-interactive-code "<1C>"
  (evil-snipe--interactive 1))

(evil-define-motion evil-snipe-s (count keys)
  "Jump to the next N-character match `count' times. N is specified by
`evil-snipe--match-count', which is 2 by default.

COUNT is how many times to repeat the snipe (behaves differently depending on
`evil-snipe-count-scope')
KEYS is a list of character codes or strings."
  :jump t
  :type inclusive
  (interactive "<+c><2C>")
  (cl-case keys
    ('abort)
    ;; if <enter>, repeat last search
    ('repeat (evil-snipe-repeat count))
    ;; Otherwise, perform the search
    (t (let* ((count (or count (if evil-snipe--last-direction 1 -1)))
              (forward-p (> count 0))
              (scope (evil-snipe--bounds forward-p))
              (scope-beg (car scope))
              (scope-end (cdr scope))
              (evil-snipe--this-func (or evil-snipe--this-func 'evil-snipe-s))
              (charstr (concat keys)))
         (setq evil-snipe--transient-common-map-func
               (set-transient-map evil-snipe-mode-common-map))
         (setq evil-snipe--transient-map-func
               (set-transient-map
                (cl-case evil-snipe--this-func
                  ('evil-snipe-f evil-snipe-mode-f-map)
                  ('evil-snipe-t evil-snipe-mode-t-map)
                  ('evil-snipe-s (when evil-snipe-enable-sS evil-snipe-mode-s-map))
                  (t (error "Tried to activate non-existant keymap: %s" evil-snipe--this-func)))))
         (unless evil-snipe--last-repeat
           (setq evil-snipe--last (list evil-snipe--this-func count keys)))
         (cl-case evil-snipe-count-scope
           ('vertical
            (evil-snipe--seek-vertical count charstr scope-beg scope-end))
           ('letters
            (evil-snipe--seek (if forward-p 1 -1) charstr scope-beg scope-end))
           (t
            (evil-snipe--seek count charstr scope-beg scope-end)))))))

(evil-define-motion evil-snipe-S (count keys)
  "Performs a reverse `evil-snipe-s'"
  :jump t
  :type inclusive
  (interactive "<-c><2C>")
  (let ((evil-snipe--this-func 'evil-snipe-s))
    (evil-snipe-s count keys)))

(evil-define-motion evil-snipe-x (count keys)
  "Performs an exclusive `evil-snipe-s'"
  :jump t
  :type inclusive
  (interactive "<+c><2C>")
  (let ((evil-snipe--consume-match nil)
        (evil-snipe--this-func 'evil-snipe-x))
    (evil-snipe-s count keys)))

(evil-define-motion evil-snipe-X (count keys)
  "Performs an backwards, exclusive `evil-snipe-S'"
  :jump t
  :type inclusive
  (interactive "<-c><2C>")
  (evil-snipe-x count keys))

(evil-define-motion evil-snipe-f (count keys)
  "Jump forward to next match of {char}"
  :jump t
  :type inclusive
  (interactive "<+c><1C>")
  (let ((evil-snipe-count-scope nil)
        (evil-snipe--this-func (or evil-snipe--this-func 'evil-snipe-f)))
    (evil-snipe-s count keys)))

(evil-define-motion evil-snipe-F (count keys)
  "Jump forward to next match of {char}"
  :jump t
  :type inclusive
  (interactive "<-c><1C>")
  (evil-snipe-f count keys))

(evil-define-motion evil-snipe-t (count keys)
  "Jump forward to next match of {char} (exclusive)"
  :jump t
  :type inclusive
  (interactive "<+c><1C>")
  (let ((evil-snipe--this-func 'evil-snipe-t)
        (evil-snipe--consume-match nil))
    (evil-snipe-f count keys)))

(evil-define-motion evil-snipe-T (count keys)
  "Jump forward to next match of {char} (exclusive)"
  :jump t
  :type inclusive
  (interactive "<-c><1C>")
  (evil-snipe-t count keys))

;; TODO Write evil-snipe-p
;; (evil-define-operator evil-snipe-p (count keys))

;; TODO Write evil-snipe-P
;; (evil-define-operator evil-snipe-P (count keys))

;; TODO Write evil-snipe-r
;; (evil-define-operator evil-snipe-r (count keys))

;; TODO Write evil-snipe-R
;; (evil-define-operator evil-snipe-R (count keys))

;; TODO Write evil-snipe-p-inner
;; (evil-define-text-object evil-snipe-p-inner (count keys))

;; TODO Write evil-snipe-P-inner
;; (evil-define-text-object evil-snipe-P-inner (count keys))

;; TODO Write evil-snipe-r-outer
;; (evil-define-text-object evil-snipe-r-outer (count keys))

;; TODO Write evil-snipe-R-outer
;; (evil-define-text-object evil-snipe-R-outer (count keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-snipe-mode-common-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";" 'evil-snipe-repeat)
    (define-key map "," 'evil-snipe-repeat-reverse)
    map))

(defvar evil-snipe-mode-f-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'evil-snipe-repeat)
    (define-key map "F" 'evil-snipe-repeat-reverse)
    map))

(defvar evil-snipe-mode-t-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" 'evil-snipe-repeat)
    (define-key map "T" 'evil-snipe-repeat-reverse)
    map))

(defvar evil-snipe-mode-s-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'evil-snipe-repeat)
    (define-key map "S" 'evil-snipe-repeat-reverse)
    map))

(defvar evil-snipe-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion map "s" 'evil-snipe-s)
    (evil-define-key 'motion map "S" 'evil-snipe-S)
    (evil-define-key 'operator map "z" 'evil-snipe-s)
    (evil-define-key 'operator map "Z" 'evil-snipe-S)
    (evil-define-key 'operator map "x" 'evil-snipe-x)
    (evil-define-key 'operator map "X" 'evil-snipe-X)

    ;; TODO Enable evil-snipe-r/R/p/P mappings
    ;; (evil-define-key 'operator map "r" 'evil-snipe-r)
    ;; (evil-define-key 'operator map "R" 'evil-snipe-R)
    ;; (evil-define-key 'operator map "p" 'evil-snipe-p)
    ;; (evil-define-key 'operator map "P" 'evil-snipe-P)
    ;; (define-key evil-outer-text-objects-map "r" 'evil-snipe-r-outer)
    ;; (define-key evil-inner-text-objects-map "r" 'evil-snipe-r-inner)
    ;; (define-key evil-outer-text-objects-map "p" 'evil-snipe-p-outer)
    ;; (define-key evil-inner-text-objects-map "p" 'evil-snipe-p-inner)

    (when evil-snipe-auto-disable-substitute
      ;; Disable s/S (substitute)
      (define-key evil-normal-state-map "s" nil)
      (define-key evil-normal-state-map "S" nil))
    map))

;;;###autoload
(define-minor-mode evil-snipe-mode
  "evil-snipe minor mode."
  :keymap evil-snipe-mode-map
  :group evil-snipe
  (evil-normalize-keymaps))

;;;###autoload
(defun evil-snipe-replace-evil ()
  "Override evil-mode's f/F/t/T functionality and replace it with evil-snipe's
version. No need to do `evil-nipe-enable-sS' with this."
  (evil-snipe-enable-sS)
  (let ((map evil-snipe-mode-map))
    (evil-define-key 'motion map "f" 'evil-snipe-f)
    (evil-define-key 'motion map "F" 'evil-snipe-F)
    (evil-define-key 'motion map "t" 'evil-snipe-t)
    (evil-define-key 'motion map "T" 'evil-snipe-T)

    (evil-define-key 'motion map ";" 'evil-snipe-repeat)
    (evil-define-key 'motion map "," 'evil-snipe-repeat-reverse)))

;;;###autoload
(defun evil-snipe-enable-sS ()
  "Enables s/S for repeating searches. Not necessary if using `evil-snipe-replace-evil'. Kept for backwards compatibility."
  (setq evil-snipe-enable-sS t))

;;;###autoload
(defun evil-snipe-enable-nN ()
  "Enables n/N for repeating searches. Kept for backwards compatibility."
  (setq evil-snipe-enable-nN t))

;;;###autoload
(defun turn-on-evil-snipe-mode ()
  "Enable evil-snipe-mode in the current buffer."
  (advice-add 'evil-force-normal-state :before 'evil-snipe--highlight-clear)
  (add-hook 'evil-insert-state-entry-hook 'evil-snipe--disable-transient-map)
  (evil-snipe-mode 1))

;;;###autoload
(defun turn-off-evil-snipe-mode ()
  "Disable evil-snipe-mode in the current buffer."
  (advice-remove 'evil-force-normal-state :before 'evil-snipe--highlight-clear)
  (remove-hook 'evil-insert-state-entry-hook 'evil-snipe--disable-transient-map)
  (evil-snipe-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-snipe-mode
  evil-snipe-mode turn-on-evil-snipe-mode
  "Global minor mode to emulate surround.vim.")


(provide 'evil-snipe)
;;; evil-snipe.el ends here
