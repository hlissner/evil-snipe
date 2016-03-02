;;; evil-snipe-aliases-test.el

(ert-deftest evil-snipe-add-aliases-test ()
  (let ((evil-snipe-symbol-groups '()))
    (evil-snipe-add-alias ?\; "[;:]")
    (should (equal '((?\; "[;:]")) evil-snipe-symbol-groups))))

(ert-deftest evil-snipe-aliases-test ()
  (with! "They're:coming{;to:take(;me:away[:he he;ho ho"
    (let ((evil-snipe-symbol-groups '((?\; "[;:]"))))
      (should (from! (point-min) (evil-snipe! 1 ?\;)
                     (looking-at-p ":")))
      (should (from! (point-min) (evil-snipe! 2 ?\;)
                     (looking-at-p ";")))

      (should (from! (point-min) (evil-snipe! 1 ?\; ?h)
                     (looking-at-p ":h")))
      (should (from! (point-min) (evil-snipe! 2 ?\; ?h)
                     (looking-at-p ";h"))))))
