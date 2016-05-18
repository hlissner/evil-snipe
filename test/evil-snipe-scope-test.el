;;; evil-snipe-scope-test.el

;; `evil-snipe-scope'
(ert-deftest evil-snipe-scope-line-test ()
  (with! "The quick\nbrown fox\njumped over\nthe lazy\ndog"
    (forward-line)
    (let ((evil-snipe-scope 'line))
      (should (equal (evil-snipe--bounds t)
                     (cons (1+ (point)) (line-end-position))))
      (should (equal (evil-snipe--bounds nil)
                     (cons (line-beginning-position) (point)))))))

(ert-deftest evil-snipe-scope-buffer-test ()
  (with! "The quick\nbrown fox\njumped over\nthe lazy\ndog"
    (forward-line)
    (let ((evil-snipe-scope 'buffer))
      (should (equal (evil-snipe--bounds t)
                     (cons (1+ (point)) (point-max))))
      (should (equal (evil-snipe--bounds nil)
                     (cons (point-min) (point)))))))

(ert-deftest evil-snipe-scope-visible-test ()
  (with! ""
    (dotimes (i 200)
      (insert "What a quick fox\n"))
    (goto-char (point-min))
    (let ((evil-snipe-scope 'visible))
      ;; NOTE: This isn't meaningful, temporary buffers don't have size
      (should (equal (evil-snipe--bounds t)
                     (cons (1+ (point)) (1- (window-end)))))
      (should (equal (evil-snipe--bounds nil)
                     (cons (window-start) (point)))))))


;; `evil-snipe-repeat-scope'
(ert-deftest evil-snipe-repeat-scope-test ()
  (with! "The quick brown fox\njumps over the lazy dog"
    (let ((evil-snipe-scope 'line)
          (evil-snipe-repeat-scope 'whole-line))
      (should-error (from! (point-min) (evil-snipe-s 1 [?a ?z])))
      (should-error (evil-snipe-repeat))
      (let ((evil-snipe-repeat-scope 'buffer))
        (should (progn (evil-snipe-repeat) (looking-at-p "azy dog")))))))


;; `evil-snipe-spillover-scope'
(ert-deftest evil-snipe-spillover-scope-test ()
  (with! "The quick foxy\nbrown fox\njumped all fox-like over\nthe lazier\nfox"
    (let ((evil-snipe-scope 'line)
          evil-snipe-spillover-scope)
      (should-error (evil-snipe-s 2 [?f ?o]))
      (let ((evil-snipe-spillover-scope 'buffer))
        (should (from! (point-min) (evil-snipe-s 2 [?f ?o])
                       (looking-at-p "fox\n")))))))

