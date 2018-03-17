;;; evil-snipe-highlight-test.el

(defun face-at-pt! (&optional pos)
  (let ((pt (or pos (point))))
    (or (get-char-property pt 'read-face-name)
        (get-char-property pt 'face))))

(defun overlays-p! (count keys &optional pos-list)
  (evil-snipe--cleanup)
  (let (list)
    (if (progn
          (mapc (lambda (ov)
                  (when (eq (overlay-get ov 'category) 'evil-snipe)
                    (push (list (overlay-start ov) (overlay-end ov))
                          list)))
                (evil-snipe--highlight-all
                 count (> count 0) (mapcar #'evil-snipe--process-key keys)))
          (unless list (error "List is empty"))
          list)
        (sort list (lambda (a b) (< (car a) (car b))))
      (when pos-list
        (error "%s != %s" pos-list it)))))

(ert-deftest evil-snipe-highlight-test ()
  (let ((evil-snipe-scope 'whole-buffer)
        (evil-snipe-smart-case t))
    (with! "money money money is so funny in a rich man's world"
      (should (from! (point-min)
                (evil-snipe--highlight 7 8 t)
                (eql (face-at-pt! 7) 'evil-snipe-first-match-face)))

      (should (from! (point-min)
                (evil-snipe--highlight 11 12)
                (eql (face-at-pt! 11) 'evil-snipe-matches-face))))))

(ert-deftest evil-snipe-1char-highlight-test ()
  (let ((evil-snipe-scope 'whole-buffer)
        (evil-snipe-smart-case t))
    (with! "money money money is so funny in a rich man's world"
      (should-error (from! (point-max) (overlays-p! 1 [?x])))

      (dotimes (_ 2) ; do twice to test idempotence
        (should (from! (point-min)
                  (overlays-p! 1 [?m] '((1 2) (7 8) (13 14) (41 42))))))

      (should (from! (point-max)
                (overlays-p! -1 [?m] '((1 2) (7 8) (13 14) (41 42))))))))

(ert-deftest evil-snipe-2char-highlight-test ()
  (let ((evil-snipe-scope 'whole-buffer)
        (evil-snipe-smart-case t))
    (with! "money money money is so funny in a rich man's world"
      (should-error (from! (point-max) (overlays-p! 1 [?n ?a])))

      (dotimes (_ 2) ; do twice to test idempotence
        (should (from! (point-min)
                  (overlays-p! 1 [?m ?o] '((1 3) (7 9) (13 15))))))

      ;; Backwards
      (should (from! (point-max)
                (overlays-p! -1 [?m ?o]
                             '((1 3) (7 9) (13 15))))))))

