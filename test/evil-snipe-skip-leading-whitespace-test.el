;;; evil-snipe-skip-leading-whitespace-test.el

(ert-deftest evil-snipe-skip-leading-whitespace-on-test ()
  (with! "    return 0;  -  "
    (let ((evil-snipe-skip-leading-whitespace t))
      (should (progn (evil-snipe-f 1 [? ]) (looking-at-p " return")))
      (should-error  (evil-snipe-F 1 [? ]))
      (should (progn (evil-snipe-f 1 [? ]) (looking-at-p " 0;")))

      (goto-char (point-min))
      (should (progn (evil-snipe-s 1 [?  ? ]) (looking-at-p "  return")))

      (goto-char (1- (point-max)))
      (should (progn (evil-snipe-S 1 [?  ? ]) (looking-at-p "  -"))))))

(ert-deftest evil-snipe-skip-leading-whitespace-repeat-test ()
  (with! "    return 0;  -  "
    (let ((evil-snipe-skip-leading-whitespace t))
      (evil-snipe-f 1 [? ])
      (should (progn "1-char repeat-forward" (evil-snipe-repeat) (looking-at-p " 0;")))

      (goto-char (point-min))
      (evil-snipe-s 1 [?  ? ])
      (should (progn "2-char repeat-forward" (evil-snipe-repeat) (looking-at-p "  -")))

      (goto-char (1- (point-max)))
      (evil-snipe-S 1 [?  ? ])
      (should (progn "2-char repeat-backward" (evil-snipe-repeat) (looking-at-p "  return"))))))

(ert-deftest evil-snipe-skip-leading-whitespace-off-test ()
  (with! "    return 0;"
    (let (evil-snipe-skip-leading-whitespace)
      (should (progn (evil-snipe-f 1 [? ]) (looking-at "   return")))
      (should (progn (evil-snipe-F 1 [? ]) (looking-at "^ ")))
      (should-error  (evil-snipe-F 1 [? ])))))
