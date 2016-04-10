;;; evil-snipe-smartcase-test.el

(ert-deftest evil-snipe-smartcase-on-test ()
  (with! "I See a sEa of Vertex shaderS"
    (let ((evil-snipe-smart-case t))
      (should (from! (point-min) (evil-snipe-s 1 [?s ?e])
                     (looking-at-p "See")))

      (should (from! (point-min) (evil-snipe-s 1 [?s ?E])
                     (looking-at-p "sEa")))

      (should (from! (point-min) (evil-snipe-s 1 [?v ?e])
                     (looking-at-p "Vertex"))))))

(ert-deftest evil-snipe-smartcase-off-test ()
  (with! "I See a sEa of Vertex shaderS"
    (let (evil-snipe-smart-case)
      (should-error (from! (point-min) (evil-snipe-s 1 [?s ?e])))

      (should (from! (point-min) (evil-snipe-s 1 [?S ?e])
                     (looking-at-p "See")))

      (should-error (from! (point-min) (evil-snipe-f 1 [?v])))

      (should (from! (point-min) (evil-snipe-s 1 [?r ?S])
                     (looking-at-p "rS$"))))))


