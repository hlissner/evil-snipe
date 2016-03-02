;;; evil-snipe-seek-test.el

(ert-deftest evil-snipe-2char-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    ;; Standard
    (should (from! (point-min)
              (evil-snipe! 1 ?o ?x)
              (looking-at-p "ox jumps")))
    (should (from! (point-max)
              (evil-snipe! -1 ?o ?x)
              (looking-at-p "ox jumps")))
    ;; Should ignore match under point
    (should-error (from! 5 (evil-snipe! 1 ?q ?u)))
    ;; No matches
    (should-error (from! (point-min) (evil-snipe! 1 ?g ?z)))))

(ert-deftest evil-snipe-1char-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    ;; Standard
    (should (from! (point-min)
              (evil-snipe! 1 ?o)
              (looking-at-p "own fox")))
    (should (from! (point-max)
              (evil-snipe! -1 ?o)
              (looking-at-p "og\\.")))
    ;; Should ignore match under point
    (should-error (from! 5 (evil-snipe! 1 ?q)))
    ;; No matches
    (should-error (from! (point-min) (evil-snipe! 1 ?=)))))

(ert-deftest evil-snipe-literal-matching-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (should (from! (point-min)
              (evil-snipe! 1 ?/ ?*)
              (looking-at-p "/\\*lazy")))
    (should (from! (point-max)
              (evil-snipe! -1 ?*)
              (looking-at-p "\\*/ dog")))
    (should (from! (point-min)
              (evil-snipe! 1 ?.)
              (looking-at-p "\\.$")))))

(ert-deftest evil-snipe-2char-exclusive-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (let (evil-snipe--consume-match)
      (evil-snipe! 1 ?o ?x)
      (should (eq (point) 17))
      (should (looking-at-p "fox"))

      
      (should (from! (point-max)
                (evil-snipe! -1 ?o ?x)
                (looking-at-p " jumps"))))))

(ert-deftest evil-snipe-1char-exclusive-test ()
  (with! "The quick Brown fox jumps over the /*lazy*/ dog."
    (let (evil-snipe--consume-match)
      (should (from! (point-min)
                (evil-snipe! 1 ?x)
                (looking-at-p "ox jumps")))
      (should (from! (point-max)
                (evil-snipe! -1 ?x)
                (looking-at-p " jumps"))))))

(ert-deftest evil-snipe-repeat-test ()
  "Tests repeating forward and reverse: see `evil-snipe-repeat' and
`evil-snipe-repeat-reverse'"
  (with! "She sourly sells Z-shells by the sullied C XOR, see?"
    (should (from! (point-min)
              (evil-snipe! 1 ?  ?s)
              (looking-at-p " sourly")))

    (should (progn (evil-snipe-repeat) (looking-at-p " sells")))
    (should (progn (evil-snipe-repeat 2) (looking-at-p " see\\?")))
    (should (progn (evil-snipe-repeat -3) (looking-at-p " sourly")))

    ;; Reverse
    (should (from! (point-max)
              (evil-snipe! -1 ?  ?s)
              (looking-at-p " see\\?")))

    (should (progn (evil-snipe-repeat) (looking-at-p " sullied")))
    (should (progn (evil-snipe-repeat 2) (looking-at-p " sourly")))
    (should (progn (evil-snipe-repeat -3) (looking-at-p " see\\?")))

    ;; Ensure idempotency
    (should (string= orig-buffer-string (buffer-string)))))
