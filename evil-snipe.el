;;; evil-snipe.el --- emulate vim-sneak & vim-seek
;;
;; Copyright (C) 2014-15 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: December 5 2014
;; Modified: March 1, 2015
;; Version: 1.6.2
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

(defcustom evil-snipe-override-evil nil
  "If non-nil, replace evil's native f/F/t/T/;/, with evil-snipe."
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

(defcustom evil-snipe-auto-scroll t
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
        (keystr (if (not (stringp keystr)) (mapconcat 'cdr keystr "") keystr)))
    (if evil-snipe-smart-case
        (not (string-match-p "[A-Z]" keystr))
      nil)))

(defun evil-snipe--count ()
  (when current-prefix-arg (prefix-numeric-value current-prefix-arg)))

(defun evil-snipe--interactive (&optional how-many)
  (let ((count (evil-snipe--count))
        (evil-snipe--match-count (or how-many 2)))
    (list (evil-snipe--collect-keys count evil-snipe--last-direction))))

(defun evil-snipe--process-keys (keys)
  (mapcar (lambda (key)
            (let ((regex-p (assoc key evil-snipe-symbol-groups)))
              (list (if regex-p t nil)
                    (if regex-p (elt regex-p 1) (char-to-string key))
                    key)))
            keys))

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
         regex-p)
    (unless forward-p
      (setq count (* -1 count)))
    (unwind-protect
        (catch 'abort
          (while (> i 0)
            (let* ((prompt (if evil-snipe-show-prompt (concat (number-to-string i) ">" (mapconcat 'cdr data "")) ""))
                   (key (evil-read-key prompt)))
              (cond ((char-equal key ?\t)         ; Tab = adds more characters to search
                     (setq i (1+ i)))
                    ((or (char-equal key ?\n)     ; Premature search
                         (char-equal key 13))
                     (if (= i how-many)
                         (throw 'abort 'repeat)
                       (throw 'abort data)))
                    ((or (char-equal key ?\C-\[)
                         (char-equal key ?\C-g))  ; Escape/C-g = abort
                     (throw 'abort 'abort))
                    (t (if (char-equal key ?\^?)  ; Otherwise, process key
                           (progn
                             (when (= i how-many) (throw 'abort 'abort))
                             (cl-incf i)
                             (when (= i how-many) (pop data)))
                         (setq regex-p (assoc key evil-snipe-symbol-groups))
                         (setq data (append data (list (cons (if regex-p t nil)
                                                             (if regex-p (elt regex-p 1) (char-to-string key))))))
                         (cl-decf i))
                       (when evil-snipe-enable-incremental-highlight
                         (evil-snipe--pre-command)
                         (evil-snipe--highlight-all count (mapconcat 'cdr data "") (assoc t data))
                         (add-hook 'pre-command-hook 'evil-snipe--pre-command))))))
          data))))

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

(defun evil-snipe--highlight (beg end &optional first-p)
  "Highlights region between beg and end. If first-p is t, then use
`evil-snipe-first-p-match-face'"
  (if (and first-p (overlays-in beg end))
      (remove-overlays beg end 'category 'evil-snipe))
  (unless (overlays-in beg end)
    (let ((x (make-overlay beg end)))
      (overlay-put x 'face (if first-p 'evil-snipe-first-match-face 'evil-snipe-matches-face))
      (overlay-put x 'category 'evil-snipe)
      (overlay-put x 'priority 100)
      (overlay-put x 'window t))))

(defun evil-snipe--highlight-all (count match &optional regex-p)
  "Highlight all instances of `match' ahead of the cursor, or behind it if
`forward-p' is nil."
  (let* ((forward-p (> count 0))
         (bounds (evil-snipe--bounds forward-p))
         (beg (car bounds))
         (end (cdr bounds))
         (beg-offset (+ (point-min) beg -1))
         (case-fold-search (evil-snipe--case-p match))
         (string (buffer-substring-no-properties beg end))
         (i 0))
    (while (and (< i (length string))
                (string-match (if regex-p match (regexp-quote match)) string i))
      (when (= (% i count) 0)
        ;; TODO Apply column-bound highlighting
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

(defun evil-snipe-seek (count keys &optional keymap)
  "Perform a snipe. KEYS is a list of characters provided by <-c> and <+c>
interactive codes. KEYMAP is the transient map to activate afterwards."
  (let ((case-fold-search (evil-snipe--case-p keys)))
    (cl-case keys
      ('abort)
      ;; if <enter>, repeat last search
      ('repeat (evil-snipe-repeat count))
      ;; If KEYS is empty.
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
             ('vertical
              (evil-snipe--seek-vertical count data))
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
           (string (mapconcat 'cdr data ""))
           (offset (length data))
           (scope (evil-snipe--bounds forward-p))
           (regex-p (assoc t data))
           (search-func (if regex-p 're-search-forward 'search-forward))
           (evil-op-vs-state-p (or (evil-operator-state-p) (evil-visual-state-p))))
      ;; Adjust search starting point
      (if forward-p (forward-char))
      (unless evil-snipe--consume-match (if forward-p (forward-char) (backward-char)))
      (unwind-protect
          (if (funcall search-func string (if forward-p (cdr scope) (car scope)) t count) ;; hi |
              (let* ((beg (match-beginning 0))
                     (end (match-end 0)))
                (if forward-p
                    (progn
                      (goto-char (if evil-op-vs-state-p (1- end) beg))
                      (unless evil-snipe--consume-match (backward-char offset)))
                  (goto-char (if evil-snipe--consume-match beg end)))
                (when (and (not evil-op-vs-state-p) evil-snipe-enable-highlight)
                  (evil-snipe--highlight beg end t))
                (when evil-snipe-auto-scroll
                  (setq new-orig-point (point))
                  (evil-scroll-line-down (- (line-number-at-pos) (line-number-at-pos orig-point)))
                  (goto-char new-orig-point))
                (when keymap
                  (setq evil-snipe--transient-map-func (set-transient-map keymap))))
            (goto-char orig-point)
            (user-error "Can't find %s" string))
        (when evil-snipe-enable-highlight
          (evil-snipe--highlight-all count string))
        (add-hook 'pre-command-hook 'evil-snipe--pre-command)))))

;; TODO Implement evil-snipe--seek-vertical
(defun evil-snipe--seek-vertical (count keys)
  (error "Not implemented!"))

(evil-define-command evil-snipe-repeat (count)
  "Repeat the last evil-snipe `count' times"
  (interactive "<c>")
  (if (listp evil-snipe--last)
      (let ((evil-snipe--last-repeat t)
            (count (or count 1))
            (evil-snipe-scope (or evil-snipe-repeat-scope evil-snipe-scope))
            (evil-snipe--consume-match (nth 3 evil-snipe--last))
            (evil-snipe--match-count (nth 4 evil-snipe--last)))
        (evil-snipe-seek (* count (first evil-snipe--last))  ;;count
                        (nth 1 evil-snipe--last)
                        (nth 2 evil-snipe--last)))          ;;keys
    (user-error "Nothing to repeat")))

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

(evil-define-interactive-code "<1C>"
  (evil-snipe--interactive 1))

(evil-define-interactive-code "<2C>"
  (evil-snipe--interactive 2))

;; (evil-define-interactive-code "<NC>"
;;   (let ((count (evil-snipe--count))
;;         (evil-snipe--match-count (or evil-snipe--match-count 2)))
;;     (list (evil-snipe--collect-keys count evil-snipe--last-direction))))

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
  (evil-snipe-seek count keys (evil-snipe--transient-map "s" "S")))

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
    (evil-snipe-seek count keys (evil-snipe--transient-map "x" "X"))))

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
    (evil-snipe-seek count keys (evil-snipe--transient-map "f" "F"))))

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
    (evil-snipe-seek count keys (evil-snipe--transient-map "t" "T"))))

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

(defvar evil-snipe-override-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion map "f" 'evil-snipe-f)
    (evil-define-key 'motion map "F" 'evil-snipe-F)
    (evil-define-key 'motion map "t" 'evil-snipe-t)
    (evil-define-key 'motion map "T" 'evil-snipe-T)

    (evil-define-key 'motion map ";" 'evil-snipe-repeat)
    (evil-define-key 'motion map "," 'evil-snipe-repeat-reverse)
  map))

(unless (fboundp 'set-transient-map)
  (defalias 'set-transient-map 'set-temporary-overlay-map))

;;;###autoload
(define-minor-mode evil-snipe-mode
  "evil-snipe minor mode."
  :lighter " snipe"
  :keymap evil-snipe-mode-map
  :group evil-snipe
  (evil-normalize-keymaps)
  (when (fboundp 'advice-add)
    (advice-add 'evil-force-normal-state :before 'evil-snipe--pre-command))
  (add-hook 'evil-insert-state-entry-hook 'evil-snipe--disable-transient-map))

;;;###autoload
(define-minor-mode evil-snipe-override-mode
  "evil-snipe minor mode that overrides evil-mode f/F/t/T/;/, bindings."
  :keymap evil-snipe-override-mode-map
  :group evil-snipe)

;;;###autoload
(defun turn-on-evil-snipe-mode ()
  "Enable evil-snipe-mode in the current buffer."
  (evil-snipe-mode 1)
  (when evil-snipe-override-evil
    (evil-snipe-override-mode 1)))

;;;###autoload
(defun turn-off-evil-snipe-mode ()
  "Disable evil-snipe-mode in the current buffer."
  (when (fboundp 'advice-remove)
    (advice-remove 'evil-force-normal-state :before 'evil-snipe--pre-command))
  (remove-hook 'evil-insert-state-entry-hook 'evil-snipe--disable-transient-map)
  (evil-snipe-mode -1)
  (evil-snipe-override-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-snipe-mode
  evil-snipe-mode turn-on-evil-snipe-mode
  "Global minor mode to emulate surround.vim.")


(provide 'evil-snipe)
;;; evil-snipe.el ends here
