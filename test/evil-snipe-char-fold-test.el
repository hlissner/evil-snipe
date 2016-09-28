;;; evil-snipe-char-fold-test.el

(when (or (> emacs-major-version 25)
          (and (= emacs-major-version 25) (>= emacs-minor-version 1)))
  (ert-deftest evil-snipe-char-fold-test ()
    (with! "And ªnd ànd ånd ānd ănd ąnd ǎnd ȁnd and"
      (let ((evil-snipe-char-fold t))
        (should (from! (point-min) (evil-snipe-f 1 [?a])
                       (looking-at-p "ª")))
        (should (from! (point-min) (evil-snipe-f 2 [?a])
                       (looking-at-p "à")))

        (should (from! (point-min) (evil-snipe-s 1 [?\  ?a])
                       (looking-at-p " ª")))
        (should (from! (point-min) (evil-snipe-s 2 [?\  ?a])
                       (looking-at-p " à")))))))

(provide 'evil-snipe-char-fold-test)
;;; evil-snipe-char-fold-test.el ends here
