;;; evil-snipe-aliases-test.el

(ert-deftest evil-snipe-aliases-test ()
  (with! "They're:coming{;to:take(;me:away[:he he;ho ho"
    (let ((evil-snipe-symbol-groups '((?\; "[;:]"))))
      (should (from! (point-min) (evil-snipe-f 1 [?\;])
                     (looking-at-p ":")))
      (should (from! (point-min) (evil-snipe-f 2 [?\;])
                     (looking-at-p ";")))

      (should (from! (point-min) (evil-snipe-s 1 [?\; ?h])
                     (looking-at-p ":h")))
      (should (from! (point-min) (evil-snipe-s 2 [?\; ?h])
                     (looking-at-p ";h"))))))
