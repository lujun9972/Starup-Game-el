(require 'cl-lib)
(require 'starup-game-util)
(require 'starup-game-core)

(defvar starup-game-company nil)
(defvar starup-game-project nil)
(defvar starup-game-week 0)

(defun starup-game/opening ()
  (starup-game-util/output  "『2015又是一幅好光景啊』加班结束之后的你看了看四周，已是深夜。")
  (starup-game-util/output  "你握紧了拳头，心想『我不能再这么加班下去了，我要改变世界』，灌木丛里的野狗叫了一声，以示鼓励。")
  (starup-game-util/next)
  (starup-game-util/output  "你决定一起开发一款屌炸天的应用，叫做：")
  (setq starup-game-project (make-project))
  (starup-game-util/output "你决定给公司起名为：")
  (setq starup-game-company (make-company))
  (starup-game-util/output "你成立了%s" (company-name starup-game-company))
  (starup-game-util/output "你拿出了你毕生的积蓄%s作为%s的启动资金。"
                           (company-money starup-game-company)
                           (project-name starup-game-project))
  (starup-game-util/output "点子和钱都有了，就差几个员工了")
  (starup-game-util/next))

(defun starup-game/hire-coders ()
  (start-hire starup-game-company))

(defun starup-game/run ()
  (starup-game//mvp ))

(defun starup-game//mvp ()
  (starup-game-util/output  "经过一番估计, 大家认为%s的开发难度为%s点困难度, 这可是个不小的工程，要加油干了。"
                            (project-name starup-game-project)
                            (project-diffcultiy starup-game-project))
  (starup-game-util/next)
  (while (and (> (project-diffcultiy starup-game-project) 0)
              (> (company-money starup-game-company) 0))
    (starup-game/work-one-week)
    (starup-game-util/next))
  (if (> (company-money starup-game-company) 0)
      (progn
        (starup-game-util/output "%s的第一版终于撸出来啦，他已经具备基本功能" (project-name starup-game-project))
        (starup-game/beta))
    (starup-game-util/output "资金耗尽，项目失败...")))

(defun starup-game/beta ()
  (starup-game-util/output "作者正在开发中"))

(defun starup-game/work-one-week ()
  (incf starup-game-week)
  (let ((old-remain-difficulty (project-diffcultiy starup-game-project)))
    (starup-game-util/output  "第%d周开始了，键盘的敲击声响起" starup-game-week)
    (starup-game/random-events)
    (starup-game/weekly-work)
    (fire-or-not starup-game-company)
    (hire-or-not starup-game-company)
    (when (= 0 (mod starup-game-week 4))
      (starup-game/pay-salary))
    (let* ((remain-difficulty (project-diffcultiy starup-game-project))
           (forward (- old-remain-difficulty remain-difficulty)))
      (starup-game-util/output "第%d周结束了，成功完成了%d点困难点，还剩下%d困难点等待开发"
                               starup-game-week
                               forward
                               remain-difficulty))))

(defun starup-game/pay-salary ()
  (starup-game-util/output "==================发薪水咯==========================")
  (let ((old-money (company-money starup-game-company)))
    (dolist (coder (company-coders starup-game-company))
      (setf (company-money starup-game-company)
            (pay coder (company-money starup-game-company))))
    (let ((cost (- old-money (company-money starup-game-company))))
      (starup-game-util/output "共计发出工资：%d ,%s剩余资金%d"
                               cost
                               (company-name starup-game-company)
                               (company-money starup-game-company))))
  (starup-game-util/output "===================================================="))

(defun starup-game/random-events ()
  )

(cl-defun starup-game/weekly-work ()
  (dolist (coder (company-coders starup-game-company))
    (setf (project-diffcultiy starup-game-project)
          (work coder (project-diffcultiy starup-game-project)))
    (when (< (project-diffcultiy starup-game-project) 0)
      (setf (project-diffcultiy starup-game-project) 0)
      (cl-return))))

(defun starup-game/staff ()
  (let* ((coders (load-coders))
         (coder-names (mapcar (lambda (coder)
                                (oref coder name))
                              coders)))
    (starup-game-util/output "Many thanks to :%s" (string-join code-names ","))))


;;;###autoload
(defun starup-game ()
  (interactive)
  (starup-game/opening)
  (starup-game/hire-coders)
  (starup-game/run)
  (starup-game/staff))

(provide 'starup-game)
