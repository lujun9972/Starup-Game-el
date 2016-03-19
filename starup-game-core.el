(require 'starup-game-util)
(require 'eieio)
(require 'cl-lib)

;; define employee

(defclass employee ()
  ((salary :initarg :salary :initform 0)
   (name :initarg :name :initform nil)))

(defmethod pay ((obj employee) company-money)
  (let ((salary (oref obj salary)))
    (- company-money salary)))

(defclass coder (employee)
  ((encourage-reasons :initform '("刚跑了8公里，精神状态良好" "一边上学一边写代码" "灵感大发") :allocation :class :protection :protected)
   (delay-reasons :initform '("昨晚补番到大半夜，严重睡眠不足" "撸了一个C++模板，然而并没有什么卵用" "进入了拖延症晚期") :allocation :class :protection :protected)
   (consumptions :initform '("一路向西去了东莞" "现阶段对生活毫无追求的他把所有工资存进了余额宝") :allocation :class :protection :protected)
   ))

(defmethod work ((coder coder) remain-difficulty)
  (let* ((code-name (oref coder name))
         (forward (if (> (starup-game-util/randint 0 9)
                         4)
                      (progn
                        (message (concat code-name (starup-game-util/randseq (oref coder encourage-reasons))))
                        (starup-game-util/randint 10 100))
                    (message (concat code-name (starup-game-util/randseq (oref coder delay-reasons))))
                    (starup-game-util/randint 1 5))))
    (message "项目进度增加%d" forward)
    (- remain-difficulty forward)))

(defmethod pay ((coder coder) company-money)
  (let ((coder-name (oref coder name))
        (coder-salary (oref coder salary)))
    (message "%s领取了%元工资. 然后%s" coder-name coder-salary (starup-game-util/randseq (oref coder consumptions)))
    (- company-money coder-salary)))

(defun load-coders ()
  (cl-mapcar (lambda (name salary)
               (coder :salary salary :name name))
             '("小冰冰" "汪小小" "PC哥")
             '(15000 7500 10000)))


;; define company

(cl-defstruct company 
  (name  (read-string "请输入公司名称"))
  (money (read-number "请输入公司的启动资金"))
  (coders nil)
  (min-coders 1)
  (max-coders 3)
  (avalible-coders (load-coders)))

(defun start-hire (company)
  (hire company)
  (let* ((coders (company-coders company))
         (code-names (mapcar (lambda (coder)
                               (oref coder name))
                             coders))))
  (message "%s带着发家致富的梦想加入的团队。" (string-join code-names ","))
  (starup-game-util/next))

(defun could-hire (company)
  (let ((coders (company-coders company))
        (max-coders (company-max-coders company)))
    (<  (length coders) max-coders)))

(cl-defun hire (company)
  (when (could-hire company)
    (message "你可以最多可以雇佣%s名员工，请谨慎选择:%s" (company-max-coders company))
    (starup-game-util/next)
    (dolist (coder (company-avalible-coders company))
      (unless (could-hire company)
        (cl-return))
      (message "------")
      (message "%s:%s 薪水:%s"
               (oref coder job)
               (oref coder name)
               (oref coder salary))
      (when (starup-game-util/confirm)
        (nconc (company-coders (list coder)))
        (message "%s加入了你的团队" (oref coder name)))
      (when (< (length (company-coders company))
               (company-min-coders company))
        (message "你至少需要%s名员工加入你的团队!" (company-min-coders company))
        (hire company)))))

(defun hire-or-not (company)
  (when (could-hire company)
    (message "你有空余的工位, 你需要招募员工么")
    (when (starup-game-util/confirm)
      (hire company))))

(defun could-fire (company)
  (let ((coders (company-coders company))
        (min-coders (company-min-coders company)))
    (>  (length coders) min-coders)))

(defun fire (company)
  )

(defun fire-or-not (company)
  (when (could-hire company)
    (message "你可以解雇让你不爽的员工")
    (when (starup-game-util/confirm)
      (fire company))))


;; define project

(cl-defstruct project 
  (name  (read-string "请输入项目名称"))
  (diffcultiy (starup-game-util/randint 5000 10000)))


(provide 'starup-game-core)
