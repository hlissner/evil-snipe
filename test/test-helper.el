
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

(defmacro evil-snipe! (count &rest keys)
  `(evil-snipe-seek ,count (evil-snipe--process-keys (list ,@keys))))


;;
(evil-define-key 'motion evil-snipe-local-mode-map "gs" 'evil-snipe-x)
(evil-define-key 'motion evil-snipe-local-mode-map "gS" 'evil-snipe-X)

(provide 'test-helper)
