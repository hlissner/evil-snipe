;;; evil-snipe.el --- emulate vim-sneak & vim-seek
;;
;; Copyright (C) 2010-2014 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: December 5 2014
;; Version: 0.1
;; Keywords: emulation, vim, evil, sneak, seek
;; Homepage: https://github.com/hlissner/evil-snipe
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Snipe is an attempt to bring a marriage of vim-sneak and vim-seek to evil-mode.
;;
;; Specifically, it brings two functions to evil: skulking and sniping. Skulking
;; pertains to finding and jumping to two-character matches. Sniping pertains to
;; performing actions (yank, delete, change, etc.) on remote words, far from the
;; cursor.
;;
;; Skulking is synonymous with f/F and t/T. By default (like vim-seek and f/F/t/T),
;; it only search for matches on the same line relative to (point). If you prefer
;; buffer-wide search, see evil-snipe-scope.
;;
;; Sniping, however, is like vim-seek's remote and presential leaps. For instance,
;; you can delete a nearby word that contains "ev" with direv. That's d for delete,
;; ir for inner-remote and ev for 'word that contains ev.
;;
;; See the README.md for more information.
;;
;;; Code:

(require 'evil)

(defvar evil-snipe--debug-mode t)

(defvar evil-snipe-search-highlight t
  "If non-nil, all matches will be highlighted after the initial jump.
  Highlights will disappear as soon as you do anything afterwards, like move the
  cursor.")

(defvar evil-snipe-search-incremental-highlight t
  "If non-nil, each additional keypress will incrementally search and highlight
matches. Otherwise, only highlight after you've finished skulking.")

(defvar evil-snipe-scope 'line
  "Dictates the scope of searches, which can be one of:

    'line    ;; search only on the line (this is vim-seek behavior) (default)
    'buffer  ;; search rest of the buffer (vim-sneak behavior)
    'visible ;; search rest of visible buffer. Is more performant than 'buffer, but
             ;; will not highlight past the visible buffer
    'whole-buffer   ;; same as 'buffer, but highlight *all* matches in buffer
    'whole-visible  ;; same as 'visible, but highlight *all* visible matches in buffer")

(defvar evil-snipe-count-scope nil
  "Dictates the scope of searches, which can be one of:

    nil          ;; default; treat count as repeat count
    'letters     ;; count = how many characters to expect and search for
    'vertical    ;; find first match within N (visible) columns")

(defvar evil-snipe-auto-disable-substitute t
  "Disables evil's native s/S functionality (substitute) if non-nil. By default
  this is t, since they are mostly redundant with other motions. s can be done
  via cl and S with cc.")

;; State vars ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar evil-snipe--last nil
  "The last search performed.")

(defvar evil-snipe--was-repeat nil
  "The last search performed.")

(defvar evil-snipe--consume-match t
  "The last search performed.")

(defvar evil-snipe--match-count 2
  "Number of characters to match. Can be let-bound to create motions that search
  for N characters. Do not set directly, unless you want to change the default
  number of characters to search.")

(defvar evil-snipe--this-func nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe--collect-keys (&optional count forward-p)
  (evil-half-cursor)
  (let* ((count (or (if (and count (> count 1)) (abs count)) evil-snipe--match-count))
         (keystr "")
         (how-many (or count evil-snipe--match-count))
         (i how-many))
    (catch 'abort
      (while (> i 0)
        (let* ((prompt (concat (number-to-string i) ">" keystr))
               (key (evil-read-key prompt)))
          (cond ((char-equal key ?\t)     ; Tab = adds more characters to search
                 (setq i (1+ i)))
                ((and (= i how-many)
                      (or (char-equal key ?\n)
                          (char-equal key 13)))
                 (throw 'abort '(?\n)))
                ((char-equal key ?\C-\[)  ; Escape = abort
                 (throw 'abort '(abort)))
                ((char-equal key ?\^?)    ; Backspace = delete character
                 (when (= i how-many) (throw 'abort '(abort)))
                 (setq i (1+ i))
                 (when (= i how-many) (setq keystr (substring keystr 0 -1))))
                (t (setq keystr (concat keystr (char-to-string key)))
                   (when evil-snipe-search-incremental-highlight
                     (evil-snipe--highlight-clear)
                     (evil-snipe--highlight-rest keystr forward-p)
                     (add-hook 'pre-command-hook 'evil-snipe--highlight-clear))
                   (setq i (1- i))))))
      (split-string keystr nil t))))

(defun evil-snipe--bounds (&optional forward-p)
  (let ((point+1 (1+ (point))))
    (case evil-snipe-scope
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
      ('whole-visible
       `(,(window-start) . ,(window-end)))
      ('whole-buffer
       `(,(point-min) . ,(point-max))))))

(defun evil-snipe--highlight (beg end &optional first)
  (let ((x (make-overlay beg end)))
    (overlay-put x 'face (if first 'isearch 'region))
    (overlay-put x 'category 'evil-snipe)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evil-snipe--seek (count string scope-beg scope-end)
  (when (> (length string) 0)
    (let ((fwdp (> count 0))
          (orig-point (point))
          (type (evil-visual-type))
          (skip-pad (length string))
          (evil-op-vs-state-p (or (evil-operator-state-p) (evil-visual-state-p))))
      (when fwdp (forward-char 1))
      (if (search-forward string (if fwdp scope-end scope-beg) t count)
          (progn
            (case evil-snipe--this-func
              ('evil-snipe-f
               (setq skip-pad (if fwdp (if evil-op-vs-state-p 1 skip-pad) 0)))
              ('evil-snipe-t
               (setq skip-pad (if fwdp (+ skip-pad 1) (- skip-pad)))))

            (unless evil-op-vs-state-p
              (when (or evil-snipe-search-highlight evil-snipe-search-incremental-highlight)
                (evil-snipe--highlight-clear))
              (when evil-snipe-search-highlight
                (evil-snipe--highlight-rest string fwdp)
                (evil-snipe--highlight (point) (- (point) skip-pad) t)
                (add-hook 'pre-command-hook 'evil-snipe--highlight-clear)))
            (backward-char skip-pad))
        (goto-char orig-point)
        (user-error "Can't find %s" string)))))

;; TODO Implement evil-snipe--seek-vertical
(defun evil-snipe--seek-vertical (count string scope-beg scope-end)
  (error "Not implemented"))

;; TODO Update for evil-snipe new defun signatures
(defun evil-snipe--repeat (count)
  (if evil-snipe--last
      (let ((evil-snipe--was-repeat t)
            (-count (nth 1 evil-snipe--last)))
        (funcall (first evil-snipe--last) ;;func name
                 (if (> -count 0) count (* count -1)) ;;count
                 (nth 2 evil-snipe--last) ;;keys
                 ))
    (user-error "No search to repeat")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-interactive-code "<+c>"
  "Regular count"
  (let ((count (when current-prefix-arg
                 (prefix-numeric-value current-prefix-arg))))
    (list (or count 1))))

(evil-define-interactive-code "<-c>"
  "Inverted count"
  (let ((count (when current-prefix-arg
                 (prefix-numeric-value current-prefix-arg))))
    (list (if count (* count -1) -1))))

(evil-define-motion evil-snipe-f (count &optional keys)
  :jump t
  :type inclusive
  (interactive "<+c>")
  (unless keys
    (setq keys (evil-snipe--collect-keys count (> count 0)))
    (message ""))
  (case (first keys)
    ('abort)
    ;; if <enter>, repeat last search
    (?\n (evil-snipe--repeat count))
    ;; Otherwise, perform the search
    (t (let* ((--was-repeat-p evil-snipe--was-repeat)

              (count (or count 1))
              (forward-p (> count 0))
              (scope (evil-snipe--bounds forward-p))
              (scope-beg (car scope))
              (scope-end (cdr scope))
              (evil-snipe--this-func (or evil-snipe--this-func 'evil-snipe-f))

              (charstr (mapconcat 'identity keys "")))
         (unless --was-repeat-p
           (setq evil-snipe--last (list evil-snipe--this-func count keys)))

         (case evil-snipe-count-scope
           ('vertical
            (evil-snipe--seek-vertical count charstr scope-beg scope-end))
           ('letters
            (evil-snipe--seek (if forward-p 1 -1) charstr scope-beg scope-end))
           (t
            (evil-snipe--seek count charstr scope-beg scope-end)))))))

(evil-define-motion evil-snipe-F (count &optional keys)
  "Jump backwards to the position of a two-character string."
  :jump t
  :type inclusive
  (interactive "<-c>")
  (evil-snipe-f count keys))

(evil-define-motion evil-snipe-t (count &optional keys)
  :jump t
  :type inclusive
  (interactive "<+c>")
  (let ((evil-snipe--consume-match nil)
        (evil-snipe--this-func 'evil-snipe-t))
    (evil-snipe-f count keys)))

(evil-define-motion evil-snipe-T (count &optional keys)
  :jump t
  :type inclusive
  (interactive "<-c>")
  (evil-snipe-t count keys))

;; TODO Write evil-snipe-p
;; (evil-define-operator evil-snipe-p (count &optional first second))

;; TODO Write evil-snipe-P
;; (evil-define-operator evil-snipe-P (count &optional first second))

;; TODO Write evil-snipe-r
;; (evil-define-operator evil-snipe-r (count &optional first second))

;; TODO Write evil-snipe-R
;; (evil-define-operator evil-snipe-R (count &optional first second))

;; TODO Write evil-snipe-p-inner
;; (evil-define-text-object evil-snipe-p-inner (count &optional first second))

;; TODO Write evil-snipe-P-inner
;; (evil-define-text-object evil-snipe-P-inner (count &optional first second))

;; TODO Write evil-snipe-r-outer
;; (evil-define-text-object evil-snipe-r-outer (count &optional first second))

;; TODO Write evil-snipe-R-outer
;; (evil-define-text-object evil-snipe-R-outer (count &optional first second))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Is this necessary?
;; (defun evil-snipe-space-compatibility ()
;;   "Allow evil-space to 'index' the the motion keybindings for evil-snipe."
;;   (let ((old-s-map (lookup-key evil-motion-state-map "s"))
;;         (old-S-map (lookup-key evil-motion-state-map "S")))

;;     ;; A roundabout way of doing it, but evil-space-setup doesn't account for
;;     ;; bindings that aren't in evil-motion-state-map, and I'd rather not
;;     ;; duplicate code by pulling in a modified copy of evil-space-setup.
;;     (define-key evil-motion-state-map "s" 'evil-snipe)
;;     (define-key evil-motion-state-map "S" 'evil-snipe-backward)

;;     (evil-space-setup "s" "n" "N")
;;     (evil-space-setup "S" "N" "n")

;;     (define-key evil-motion-state-map "s" old-s-map)
;;     (define-key evil-motion-state-map "S" old-S-map)))

(defgroup evil-snipe nil
  "vim-seek/sneak emulation for Emacs"
  :prefix "evil-snipe-"
  :group 'evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-snipe-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'motion   map "s" 'evil-snipe-f)
    (evil-define-key 'motion   map "S" 'evil-snipe-F)
    (evil-define-key 'operator map "z" 'evil-snipe-f)
    (evil-define-key 'operator map "Z" 'evil-snipe-F)
    (evil-define-key 'operator map "x" 'evil-snipe-t)
    (evil-define-key 'operator map "X" 'evil-snipe-T)

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
(defun turn-on-evil-snipe-mode ()
  "Enable evil-snipe-mode in the current buffer."
  (advice-add 'evil-force-normal-state :before #'evil-snipe--highlight-clear)
  (evil-snipe-mode 1))

;;;###autoload
(defun turn-off-evil-snipe-mode ()
  "Disable evil-snipe-mode in the current buffer."
  (advice-remove 'evil-force-normal-state :before #'evil-snipe--highlight-clear)
  (evil-snipe-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-snipe-mode
  evil-snipe-mode turn-on-evil-snipe-mode
  "Global minor mode to emulate surround.vim.")


(provide 'evil-snipe)
;;; evil-snipe.el ends here
