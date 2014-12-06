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

(defvar evil-snipe-enable-skulking t
  "(NOT IMPLEMENTED YET)")

(defvar evil-snipe-enable-sniping t
  "(NOT IMPLEMENTED YET)")

(defvar evil-snipe-search-highlight nil
  "(NOT IMPLEMENTED YET)")

(defvar evil-snipe-search-incremental-highlight nil
  "(NOT IMPLEMENTED YET) If non-nil, matches of the first key you enter will be
highlighted. Otherwise, only highlight after you've typed both characters.")

(defvar evil-snipe-scope 'line
  "Dictates the scope of searches, which can be one of:

    'line    ;; search only on the line (this is vim-seek behavior) (default)
    'buffer  ;; search rest of the buffer (vim-sneak behavior)
    'visible ;; search rest of visible buffer. Is more performant than 'buffer, but
             ;; will not highlight past the visible buffer")

(defvar evil-snipe-count-scope 'horizontal
  "(NOT IMPLEMENTED YET) Dictates the scope of searches, which can be one of:

    'horizontal  ;; find first match within N lines (default)
    'repeat      ;; jump to Nth match from point
    'vertical    ;; find first match within N (visible) columns")

(defvar evil-snipe-auto-disable-substitute t
  "Disables evil's native s/S functionality (substitute) if non-nil. By default
  this is t, since they are mostly redundant with other motions. s can be done
  via cl and S with cc.")

(defvar evil-snipe-repeat t
  "(NOT IMPLEMENTED YET) Which type of repeat to use, can be any of:

    non-nil  ;; repeat with ; and ,
    'next    ;; repeat with s and S
    'search  ;; repeat with n and N")

(defvar evil-snipe--last nil
  "The last search performed.")

(defvar evil-snipe--was-repeat nil
  "The last search performed.")

(defvar evil-snipe--consume-match-p nil
  "The last search performed.")

(defvar evil-snipe--match-count 2
  "Number of characters to match. Can be let-bound to create motions that search
  for N characters. Do not set directly, unless you want to change the default
  number of characters to search.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Make helper to collect variable keys
;; (defun evil-snipe-collect-keys ()
;;   (interactive)
;;   (evil-half-cursor)
;;   (let* ((i (1- evil-snipe--match-count))
;;          (first (evil-read-key ">"))
;;          (keys (list first))
;;          (keystr (string first)))
;;     (cond ((or (char-equal first ?\n)
;;                (eq first 13))
;;            (list first))
;;           (t (while (> i 0)
;;                (setq keystr (concat keystr (string (evil-read-key (concat ">" keystr)))))
;;                (setq i (1- i))
;;                keys)
;;              )
;;           )
;;     )
;;   )

(defun evil-snipe--collect-keys ())

(defun evil-snipe--repeat (&optional count)
  (if (not evil-snipe--last)
      (site-error "No previous search to repeat")
    (let ((evil-snipe--was-repeat t)
          (-count (nth 1 evil-snipe--last)))
      (funcall (first evil-snipe--last)
               (if (> -count 0)
                   count
                 (* count -1))
               (nth 2 evil-snipe--last)
               (nth 3 evil-snipe--last)))))

(defun evil-snipe--scope-beg ()
  (cond ((eq evil-snipe-scope 'line)
         (line-beginning-position))
        ((eq evil-snipe-scope 'visible)
         (window-start))
        ((eq evil-snipe-scope 'buffer)
         (point-min)))
  )

(defun evil-snipe--scope-end ()
  (cond ((eq evil-snipe-scope 'line)
         (line-end-position))
        ((eq evil-snipe-scope 'visible)
         (window-end))
        ((eq evil-snipe-scope 'buffer)
         (point-max))))

(defun evil-snipe--search (count))

(defun evil-snipe--highlight (beg end &optional first)
  (let ((x (make-overlay beg end)))
    (overlay-put x 'face (if first 'isearch 'region))
    (overlay-put x 'category 'evil-snipe)))

(defun evil-snipe--highlight-rest (matches beg end)
  (let ((string (buffer-substring-no-properties beg end))
        (beg-offset (+ (point-min) beg -1)))
    (let ((i 0))
      (while (and (< i (length string))
                  (string-match matches string i))
        (setq i (1+ (match-beginning 0)))

        (evil-snipe--highlight (+ beg-offset (match-beginning 0)) (+ beg-offset (match-end 0)))
        )
      )
    )
  )

(defun evil-snipe--highlight-clear ()
  (remove-overlays nil nil 'category 'evil-snipe)
  (remove-hook 'pre-command-hook 'evil-snipe--highlight-clear))

;; TODO Refactor pls!
(evil-define-motion evil-snipe-f (count &optional first second)
  "Jump to the position of a two-character string."
  :jump t
  :type inclusive
  (interactive "<c>")
  (unless evil-snipe--was-repeat
    (evil-half-cursor))
  (setq count (or count 1))
  (let* ((first (or first (evil-read-key ">")))
         (fwdp (> count 0))
         ;; Beginning of bounds
         (bob (evil-snipe--scope-beg))
         ;; End of bounds
         (eob (evil-snipe--scope-end))

         ;; For highlight bounds
         (beg (if fwdp (1+ (point)) bob))
         (end (if fwdp eob (point))))
    (cond ((or (char-equal first ?\n)
               (eq first 13))
           (evil-snipe--repeat count))
          (t
           (unless (or (evil-operator-state-p) evil-snipe-search-incremental-highlight
             (evil-snipe--highlight-rest (string first) beg end)))
           (setq second (or second (evil-read-key (concat ">" (char-to-string first)))))
           (let ((charstr (string first second))
                 (old-pos (point))
                 new-pos new-pos-p)

             ;; TODO Implement highlighting
             (save-excursion
               (unless (eq old-pos
                           (progn
                             (when fwdp (forward-char 1))
                             (if (search-forward charstr
                                                 (if fwdp (if (< eob 0) 0 eob) bob)
                                                 t count)

                                 (let ((offset (if evil-snipe--consume-match-p 2 1)))
                                   (when fwdp
                                     (backward-char offset))
                                   (setq new-pos (point)))

                               (when fwdp (backward-char 1)))
                             new-pos))
                 (setq new-pos (point))
                 (unless evil-snipe--was-repeat
                   (setq evil-snipe--last (list 'evil-snipe-f count first second evil-snipe--consume-match-p)))
                 ))

             (if new-pos
                 (progn
                   (goto-char new-pos)
                   (unless evil-snipe--was-repeat
                     (evil-snipe--highlight-reset))
                   (unless (evil-operator-state-p)
                     (evil-snipe--highlight new-pos (+ new-pos (length charstr)) t)
                     (evil-snipe--highlight-rest charstr beg end)
                     (add-hook 'pre-command-hook 'evil-snipe--highlight-clear))
                   )
               (evil-snipe--highlight-clear)
               (user-error "Can't find %s" charstr))
             )
           )
          )
    )
  )

(evil-define-motion evil-snipe-F (count &optional first second)
  "Jump backwards to the position of a two-character string."
  :jump t
  :type inclusive
  (interactive "<c>")
  (evil-snipe-f (- (or count 1)) first second))

(evil-define-motion evil-snipe-t (count &optional first second)
  :jump t
  :type exclusive
  (interactive "<c>")
  (let ((evil-snipe--consume-match-p t))
    (evil-snipe-f (or count 1) first second)))

(evil-define-motion evil-snipe-T (count &optional first second)
  :jump t
  :type exclusive
  (interactive "<c>")
  (let ((evil-snipe--consume-match-p t))
    (evil-snipe-F (or count 1) first second)))

;; TODO Write evil-snipe-p
;; (evil-define-motion evil-snipe-p (count &optional first second))

;; TODO Write evil-snipe-P
;; (evil-define-motion evil-snipe-P (count &optional first second))

;; TODO Write evil-snipe-r
;; (evil-define-motion evil-snipe-r (count &optional first second))

;; TODO Write evil-snipe-R
;; (evil-define-motion evil-snipe-R (count &optional first second))

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
    (evil-define-key 'motion   map "s" 'evil-snipe-t)
    (evil-define-key 'motion   map "S" 'evil-snipe-T)
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
  (evil-snipe-mode 1))

;;;###autoload
(defun turn-off-evil-snipe-mode ()
  "Disable evil-snipe-mode in the current buffer."
  (evil-snipe-mode -1))

;;;###autoload
(defun evil-snipe-override-surround ()
  "Map evil-snipe bindings over s and S, overriding evil-surround. I recommend
you add alternate keymaps for surround elsewhere."
  (evil-define-key 'visual evil-snipe-mode-map "z" 'evil-snipe-f)
  (evil-define-key 'visual evil-snipe-mode-map "Z" 'evil-snipe-F))

;;;###autoload
(define-globalized-minor-mode global-evil-snipe-mode
  evil-snipe-mode turn-on-evil-snipe-mode
  "Global minor mode to emulate surround.vim.")

(provide 'evil-snipe)
;;; evil-snipe.el ends here
