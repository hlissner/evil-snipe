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
;; buffer-wide search, see evil-snipe-bounds.
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

(defvar evil-snipe-bounds 'line
  "Dictates the scope of searches, which can be one of:

    'line    ;; search only on the line (this is vim-seek behavior)
    'buffer  ;; search rest of the buffer (vim-sneak behavior)
    'visible ;; search rest of *visible* buffer (more performant than 'buffer... maybe)
    'count   ;; search within [count] lines after (point), otherwise behaves like
             ;; line.")

(defvar evil-snipe-auto-disable-substitute t
  "Whether or not to disable evil's native s/S functionality (substitute). By
  default this is t, since they are mostly redundant with other motions. s can
  be done via cl and S with cc.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-motion evil-snipe-f (count &optional first second consume-match-p)
  "Jump to the position of a two-character string."
  :jump t
  :type inclusive
  (interactive "<c>")
  (evil-half-cursor)
  (let ((first (or first (evil-read-key ">"))))
    (cond ((or (char-equal first ?\n)
               (eq first 13))
           (message "Repeat unimplemented"))
          (t
           (setq second (or second (evil-read-key (concat ">" (char-to-string first)))))
           (setq count (or count 1))
           (let* ((fwdp (> count 0))
                  (charstr (string first second))

                  ;; Beginning of bounds
                  (bob (cond ((eq evil-snipe-bounds 'line)
                              (line-beginning-position))
                             ((eq evil-snipe-bounds 'visible)
                              (window-start))
                             ((eq evil-snipe-bounds 'buffer)
                              (point-min))
                             ((eq evil-snipe-bounds 'count)
                              (error "Not implemented yet"))))

                  ;; End of bounds
                  (eob (cond ((eq evil-snipe-bounds 'line)
                              (line-end-position))
                             ((eq evil-snipe-bounds 'visible)
                              (window-end))
                             ((eq evil-snipe-bounds 'buffer)
                              (point-max))
                             ((eq evil-snipe-bounds 'count)
                              (error "Not implemeneted yet"))))

                  ;; For highlight bounds
                  ;; (beg (if fwdp (1+ (point)) bob))
                  ;; (end (if fwdp eob (point))
                  ;; (bound (if fwdp (point-max) (point-min)))
                  )

             ;; TODO Implement highlighting
             (if (search-forward charstr
                                 (if fwdp (if (< eob 0) 0 eob) bob)
                                 t count)
                 (when fwdp
                   (backward-char (if consume-match-p 2 1)))
               (user-error "Can't find %s" charstr)))))))

(evil-define-motion evil-snipe-F (count &optional first second consume-match-p)
  "Jump backwards to the position of a two-character string."
  :jump t
  :type inclusive
  (interactive "<c>")
  (evil-snipe-f (- (or count 1)) first second consume-match-p))

(evil-define-motion evil-snipe-t (count &optional first second)
  :jump t
  :type inclusive
  (interactive "<c>")
  (evil-snipe-f (or count 1) first second t))

(evil-define-motion evil-snipe-T (count &optional first second)
  :jump t
  :type inclusive
  (interactive "<c>")
  (evil-snipe-F (or count 1) first second t))

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

;;;###autoload
(defun evil-snipe-surround-compatibility ()
  "Map evil-snipe bindings over s and S, overriding evil-surround. I recommend
you add alternate keymaps for surround elsewhere."
  (evil-define-key 'visual evil-snipe-mode-map "s" 'evil-snipe-f)
  (evil-define-key 'visual evil-snipe-mode-map "S" 'evil-snipe-F))

;;;###autoload
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
(define-globalized-minor-mode global-evil-snipe-mode
  evil-snipe-mode turn-on-evil-snipe-mode
  "Global minor mode to emulate surround.vim.")

(provide 'evil-snipe)
;;; evil-snipe.el ends here
