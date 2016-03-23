(defun starup-game-util/next ()
  (print "(---敲击任意键继续---)")
  (read-char))

(defun starup-game-util/confirm ()
  (print "(---按Y确认, 按其他键跳过---)")
  (let ((answer (downcase  (read-char))))
    (char-equal answer ?y)))

(defun starup-game-util/input ()
  (print "(---请输入---)")
  (string-trim (read-string "")))

(defun starup-game-util/randint (min max)
  (+ min (random (+ 1 (- max min)))))

(defun starup-game-util/randseq (seq)
  (let ((idx (random (length seq))))
    (elt seq idx)))

(defun starup-game-util/output (fmt &rest args)
  (select-window (display-buffer (get-buffer-create "*starup-game*")))
  (goto-char (point-max))
  (insert (apply #'format fmt args))
  (newline))

(provide 'starup-game-util)
