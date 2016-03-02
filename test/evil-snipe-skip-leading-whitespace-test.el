;;; evil-snipe-skip-leading-whitespace-test.el

(ert-deftest evil-snipe-skip-leading-whitespace-on-test ()
  (with! "    return 0;"
    (let ((evil-snipe-skip-leading-whitespace t))
      (should (progn (evil-snipe! 1 " ") (looking-at " 0;")))
      (should (progn (evil-snipe! -1 " ") (looking-at " return")))
      (should-error (evil-snipe! -1 " ")))))

(ert-deftest evil-snipe-skip-leading-whitespace-off-test ()
  (with! "    return 0;"
    (let (evil-snipe-skip-leading-whitespace)
      (should (progn (evil-snipe! 1 " ") (looking-at "   return")))
      (should (progn (evil-snipe! -1 " ") (looking-at "^ ")))
      (should-error (evil-snipe! -1 " ")))))
