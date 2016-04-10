
(require 'ert)
(require 'evil-snipe)

(defmacro with! (initial &rest rest)
  (declare (indent 1)
           (debug (form form body)))
  `(save-window-excursion
     (let ((case-fold-search nil)
           (orig-buffer-string ,initial))
       (with-temp-buffer
         (set-input-method nil)
         (evil-local-mode +1)
         (evil-snipe-local-mode +1)
         (evil-snipe-override-local-mode +1)
         (pop-to-buffer (current-buffer))
         (insert orig-buffer-string)
         (goto-char (point-min))
         ,@rest))))

(defmacro from! (point &rest forms)
  (declare (indent 1))
  `(progn
     (goto-char ,point)
     ,@forms))

(defun exec! (keys)
  (ignore-errors (execute-kbd-macro keys) (point)))

(setq evil-snipe-show-prompt nil)

;;
(evil-define-key 'motion evil-snipe-mode-map "gs" 'evil-snipe-x)
(evil-define-key 'motion evil-snipe-mode-map "gS" 'evil-snipe-X)

(provide 'test-helper)
