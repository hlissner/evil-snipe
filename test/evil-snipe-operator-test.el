;;; evil-snipe-operator-test.el

(ert-deftest evil-snipe-1char-operator-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-min)
              (exec! [?d ?f ?o])
              (looking-at-p "wn fox"))))

  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-max)
              (exec! [?d ?F ?o])
              (looking-at-p "d$")))))

(ert-deftest evil-snipe-1char-exclusive-operator-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-min)
              (exec! [?d ?t ?o])
              (looking-at-p "own fox"))))

  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-max)
              (exec! [?d ?T ?o])
              (looking-at-p "o$")))))

(ert-deftest evil-snipe-2char-operator-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-min)
              (exec! [?d ?z ?o ?x])
              (looking-at-p " jumps"))))

  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-max)
              (exec! [?d ?Z ?o ?x])
              (looking-at-p "f$")))))

(ert-deftest evil-snipe-2char-exclusive-operator-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-min)
              (exec! [?d ?x ?o ?x])
              (looking-at-p "ox jumps"))))

  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-max)
              (exec! [?d ?X ?o ?x])
              (looking-at-p "x$")))))
