#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
 
;;; 以下三个定义 env0, ent-env, lookup 是对环境(environment)的基本操作:  
 
;; 空环境  
 
(define env0 '())  
 
;; 扩展。对环境 env 进行扩展,把 x 映射到 v,得到一个新的环境  
 
(define ext-env  
 (lambda (x v env)  
   (cons `(,x . ,v) env)))  
 
;; 查找。在环境中 env 中查找 x 的值  
 
(define lookup  
            (lambda (x env)  
            (let ([p (assq x env)])  
               (cond  
                  [(not p) x]  
                    [else (cdr p)]))))  
 
;; 闭包的数据结构定义,包含一个函数定义 f 和它定义时所在的环境  
 
(struct Closure (f env))  
 
;; 解释器的递归定义(接受两个参数,表达式 exp 和环境 env)  
 
;; 共 5 种情况(变量,函数,调用,数字,算术表达式)  
 
(define interp1  
 (lambda (exp env)  
   (match exp                                          ; 模式匹配 exp 的以下情况(分支)  
     [(? symbol? x) (lookup x env)]                    ; 变量  
     [(? number? x) x]                                 ; 数字  
     [`(lambda (,x) ,e)                                ; 函数  
      (Closure exp env)]  
     [`(,e1 ,e2)                                       ; 调用  
      (let ([v1 (interp1 e1 env)]  
            [v2 (interp1 e2 env)])  
        (match v1  
          [(Closure `(lambda (,x) ,e) env1)  
           (interp1 e (ext-env x v2 env1))]))]  
     [`(,op ,e1 ,e2)                                   ; 算术表达式  
      (let ([v1 (interp1 e1 env)]  
            [v2 (interp1 e2 env)])  
        (match op  
          ['+ (+ v1 v2)]  
          ['- (- v1 v2)]  
          ['* (* v1 v2)]  
          ['/ (/ v1 v2)]))])))  
 
;; 解释器的“用户界面”函数。它把 interp1 包装起来,掩盖第二个参数,初始值为 env0  
 
(define interp  
 
(lambda (exp)  
 
  (interp1 exp env0)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;测试例子这里有一些测试的例子。你最好先玩一下再继续往下看,或者自己写一些新的例子。学习程序的最好办法就是玩弄这个程序,给它一些输入,观察它的行为。有时候这比任何语言的描述都要直观和清晰。  
 
(interp '(+ 1 2))  
 
;; => 3 
 
(interp '(* 2 3))  
 
;; => 6 
 
(interp ' (* 2 (+ 3 4)))  
 
;; => 14 
 
(interp ' (* (+ 1 2) (+ 3 4)))  
 
;; => 21 
 
(interp ' (((lambda (x) (lambda (y) (* x y))) 2) 3))  
 
;; => 6 
 
(interp ' ((lambda (x) (* 2 x)) 3))  
 
;; => 6 
 
   
 
(interp ' ((lambda (y) (((lambda (y) (lambda (x) (* y 2))) 3) 0)) 4))  
 
;; => 6;; (interp ' (1 2))  
 
