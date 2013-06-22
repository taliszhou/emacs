(setenv "HOME" "D:/BIN/TOOLS/emacs/home")
; (setenv "PATH" "D:/BIN/TOOLS/emacs")
; (setenv "PATH"
;         (concat
;          (getenv "PATH")
;          ":""D:/Developer/Erl510//bin"))
; (setenv "PATH"
;         (concat
;          (getenv "PATH")
;          ":""D:/Developer/Erl510//bin"))


;;set the default file path
;(setq default-directory "~/")
(add-to-list 'load-path "~/.emacs.d/")
;(add-to-list 'load-path "~/site-lisp")
(setq cjk-font-size 10)

;;Wubi
 (add-to-list 'load-path "~/.emacs.d/wubi")
 (require 'wubi)
 (wubi-load-local-phrases) 
 ; add user's Wubi phrases
 (register-input-method "chinese-wubi" "Chinese-GB" 'quail-use-package "WuBi" "WuBi" "wubi")
 (if (< emacs-major-version 21)    
 (setup-chinese-gb-environment)  
 (set-language-environment 'Chinese-GB))
 (setq default-input-method "chinese-wubi")

 (global-set-key [?\S- ] 'set-mark-command)


;;;; This snippet enables lua-mode
;; This line is not necessary, if lua-mode.el is already on your load-path
(add-to-list 'load-path "~/.emacs.d/lua-mode")

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'eval-after-load)
 
(eval-after-load "tree-widget"
  '(if (boundp 'tree-widget-themes-load-path)
       (add-to-list 'tree-widget-themes-load-path "~/.emacs.d/")))

(autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
(autoload 'tags-tree "tags-tree" "TAGS tree" t)


;Erlang Mode
(setq load-path (cons"D:/Developer/Erl510/lib/tools-2.6.10/emacs" load-path))
(setq erlang-root-dir "D:/Developer/Erl510/lib")
(setq exec-path (cons "D:/Developer/Erl510/bin" exec-path))
(require 'erlang-start)

;; This is needed for Distel setup
(let ((distel-dir "~/.emacs.d/plugins/distel/elisp"))
;(let ((distel-dir "/usr/local/share/distel/elisp"))
  (unless (member distel-dir load-path)
    ;; Add distel-dir to the end of load-path
    (setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)
;; Some Erlang customizations
(add-hook 'erlang-mode-hook
    (lambda ()
      ;; when starting an Erlang shell in Emacs, default in the node name
      (setq inferior-erlang-machine-options '("-name" "distel@localhost.local" "-pa" "../deps/log4erl/ebin" "../deps/emysql/ebin"))
      ;; add Erlang functions to an imenu menu
      (imenu-add-to-menubar "imenu")))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)  
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind) 
    ("\M-*"      erl-find-source-unwind) 
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
    (lambda ()
      ;; add some Distel bindings to the Erlang shell
      (dolist (spec distel-shell-keys)
        (define-key erlang-shell-mode-map (car spec) (cadr spec)))))


; Для фодинга
(defun my-erlang-mode-hook2 ()
  (setq hs-special-modes-alist 
        (cons 
         '(erlang-mode "^\\([a-z][a-zA-Z0-9_]*\\|'[^\n']*[^\\]'\\)\\s *(" nil 
                       "%" erlang-end-of-clause ) 
         hs-special-modes-alist))
  (hs-minor-mode 1)
  (local-set-key [?\M-s] 'hs-toggle-hiding)
  (local-set-key [?\M-h] 'hs-hide-all)
  (local-set-key [?\M-u] 'hs-show-all))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook2)

;;显示行列号
(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode 'linum-mode);;在左边显示行号
(setq linum-format "%d  ")


(require 'erlang-flymake)
;;仅在存盘时进行检查
(erlang-flymake-only-on-save)
;;键盘映射
(defvar flymake-mode-map (make-sparse-keymap))
(define-key flymake-mode-map (kbd "<f2>") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c <f2>") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "<f3>") 'flymake-display-err-menu-for-current-line)
(or (assoc 'flymake-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'flymake-mode flymake-mode-map)
                minor-mode-map-alist)))

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/")
(require 'clippy)


(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)   ;default is t
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1.0)


(global-set-key (kbd "C-;") 'clippy-describe-function) 


;emacs -batch -f batch-byte-compile  filename
(autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
  (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


(require 'rainbow-mode)
(add-to-list 'auto-mode-alist '("\\.css\\'" . rainbow-mode))



(dolist (hook '(css-mode-hook
             html-mode-hook
             python-mode-hook
             c++-mode-hook
             lisp-mode-hook))
  (add-hook hook (lambda () (rainbow-mode 1))))

(require 'deft)
(setq deft-extension "txt")
(setq deft-directory "D:/WORKPLC/notebooks")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)




(require 'util)
(require 'emaci)

(apply-define-key
 global-map
 `(("M-l" emaci-mode-on)
   ("C-M-l" emaci-mode-off)))




(tool-bar-mode 0);去掉那个大大的工具栏

;;在标题栏显示buffer的名字，而不是 emacs@wangyin.com 这样没用的提示; %b：buffer名; %f：文件名
(setq frame-title-format "emacs@%f")

;;关闭emacs启动时的画面
(setq inhibit-startup-message t)
;;关闭gnus启动时的画面
(setq gnus-inhibit-startup-message t)

;;ido的配置,这个可以使你在用C-x C-f打开文件的时候在后面有提示;
;;这里是直接打开了ido的支持，在emacs23中这个是自带的.
(ido-mode t)

;;滚动条设置
(scroll-bar-mode 0) ;;不显示滚动条 太丑

;(add-to-list 'load-path "~/.emacs.d/plugins/color-theme-6.6.0/")
;(require 'color-theme)
;(color-theme-initialize)  
;; 这个是你选择的主题，后面的calm forest就是它的名字，注意使用小写。
;(color-theme-calm-forest)

(defun split-v-3 () 
  "Change 3 window style from horizontal to vertical"
  (interactive) 

  (select-window (get-largest-window)) 
  (if (= 3 (length (window-list))) 
      (let ((winList (window-list))) 
      (let ((1stBuf (window-buffer (car winList))) 
      (2ndBuf (window-buffer (car (cdr winList)))) 
      (3rdBuf (window-buffer (car (cdr (cdr winList)))))) 
        (message "%s %s %s" 1stBuf 2ndBuf 3rdBuf) 

        (delete-other-windows) 
        (split-window-horizontally) 
        (set-window-buffer nil 1stBuf) 
        (other-window 1) 
        (set-window-buffer nil 2ndBuf) 
        (split-window-vertically) 
        (set-window-buffer (next-window) 3rdBuf) 
        (select-window (get-largest-window)) 
      )))) 
(global-set-key (kbd "C-x 4 c") (quote split-v-3)) 

(set-default-font"-apple-Consolas-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")



(add-to-list 'load-path "~/.emacs.d/")
(require 'tabbar)
(tabbar-mode)

(global-set-key [(meta j)] 'tabbar-backward)
(global-set-key [(meta k)] 'tabbar-forward)

;(require 'tabbar)
;(tabbar-mode)
(global-set-key (kbd "<M-up>") 'tabbar-backward-group)
(global-set-key (kbd "<M-down>") 'tabbar-forward-group)
;(global-set-key (kbd "<M-left>") 'tabbar-backward)
;(global-set-key (kbd "<M-right>") 'tabbar-forward)

;git
(add-to-list 'load-path "~/.emacs.d/plugins/git-emacs/")
(require 'git-emacs)

(defun display-buffer-name ()
  (interactive)
  (message (buffer-file-name (current-buffer))))
(global-set-key (kbd "M-4") 'ido-kill-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'other-window)
(global-set-key (kbd "M-5") 'display-buffer-name)
(global-set-key (kbd "C-x f") 'find-file-at-point)

  
(require 'sr-speedbar)
  
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-width 30)
(setq sr-speedbar-right-side nil)
  
(global-set-key (kbd "<f5>") (lambda()
                                (interactive)
                                (sr-speedbar-toggle)))

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

(setq-default make-backup-files nil)
(setq auto-save-default nil)

;esense

;(add-to-list 'load-path "~/.emacs.d/plugins/esense-1.12/")
;(require 'esense-start)
;(setq esense-indexer-program "~/.emacs.d/plugins/esense-1.12/esense.sh")

;(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
;(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
;(require 'w3m-load)



(add-to-list 'load-path "~/w3m")
(add-to-list 'exec-path "~/w3m")
(require 'w3m-load)  

(setq w3m-command-arguments '("-cookie" "-F"))
(setq w3m-use-cookies t)
(setq w3m-use-mule-ucs t)
(setq w3m-home-page "http://www.baidu.com")  
(setq w3m-default-display-inline-images t)

;;系统剪贴板快捷键（C-c C-c复制，C-c C-v粘贴）  
(global-set-key "\C-c\C-c" 'clipboard-kill-ring-save)  
(global-set-key "\C-c\C-v" 'clipboard-yank)  



(defun yodao-dict-search-wordap (&optional word)
"Use python script dict to look up word under point"
(interactive)
(or word (setq word (current-word)))
(shell-command (format "D:/Developer/python32/python d:/bin/tools/emacs/.emacs.d/dict.py %s" word)))
(global-set-key [f7] 'yodao-dict-search-wordap)


;; 这个是 Emacs 自带的功能，可是知道它的人不多。很多人用的是M-/ (dabbrev-expand) 这样的东西。 hippie-expand 要强大的多。因为它可以使用 扩展函数任意扩充！你可以把你的 M-/ 绑定到 hippie-expand，马上就可以体 会到它的方便。
;; 这是说，首先使用当前的buffer补全，如果找不到，就到别的可见的窗口里寻找， 如果还找不到，那么到所有打开的buffer去找，如果还……那么到kill-ring里， 到文件名，到简称列表里，到list，…… 当前使用的匹配方式会在 echo 区域 显示。

;; 特别有意思的是 try-expand-line，它可以帮你补全整整一行文字。我很多 时后有两行文字大致相同，只有几个字不一样，但是我懒得去拷贝粘贴以下。那 么我就输入这行文字的前面几个字。然后多按几下 M-/ 就能得到那一行。
(global-set-key [(meta ?/)] 'hippie-expand)

(setq hippie-expand-try-functions-list 
      '(try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))


;; 这是一个很小的函数。你是不是觉得 Emacs 在匹配的括号之间来回跳转的时 候按 C-M-f 和 C-M-b 太麻烦了？vi的 % 就很方便，我们可以把 % 设置为匹配 括号。可是你想输入 % 怎么办呢？
;; 一个很巧妙的解决方案就是，当 % 在括号上按下时，那么匹配括号，否则输 入一个 %。你只需要在 .emacs 文件里加入这些东西就可以达到目的：

(global-set-key "%" 'match-paren)
          
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))


(require 'switch-window)


(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially try-complete-lisp-symbol
        try-complete-file-name-partially try-complete-file-name))




   
(setq byte-compile-warnings nil)
(setq stack-trace-on-error t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Cedet 1.1
;;
(load-file "~/.emacs.d/plugins/cedet-1.1/common/cedet.el")
      (global-ede-mode 1)                      ; Enable the Project management system
      (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
      (global-srecode-minor-mode 1)            ; Enable template insertion menu

(global-set-key [f12] 'semantic-ia-fast-jump)


(global-set-key [S-f12]
                (lambda ()
                  (interactive)
                  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
                      (error "Semantic Bookmark ring is currently empty"))
                  (let* ((ring (oref semantic-mru-bookmark-ring ring))
                         (alist (semantic-mrub-ring-to-assoc-list ring))
                         (first (cdr (car alist))))
                    (if (semantic-equivalent-tag-p (oref first tag)
                                                   (semantic-current-tag))
                        (setq first (cdr (car (cdr alist)))))
                    (semantic-mrub-switch-tags first))))

;(define-key c-mode-base-map [M-S-f12] 'semantic-analyze-proto-impl-toggle)
;(define-key c-mode-base-map (kbd "M-n") 'semantic-ia-complete-symbol-menu)

  (when (and window-system
             (> emacs-major-version 21)
             (require 'semantic-tag-folding nil 'noerror))
    (global-semantic-tag-folding-mode 1)
    (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode)
    (define-key semantic-tag-folding-mode-map
      (kbd "C-c , -") 'semantic-tag-folding-fold-block)
    (define-key semantic-tag-folding-mode-map
      (kbd "C-c , +") 'semantic-tag-folding-show-block)
    (define-key semantic-tag-folding-mode-map
      (kbd "C-_") 'semantic-tag-folding-fold-all)
    (define-key semantic-tag-folding-mode-map
      (kbd "C-+") 'semantic-tag-folding-show-all))


;; (setq semanticdb-project-roots (list (expand-file-name "/")))
; (defconst cedet-user-include-dirs
;   (list ".." "../include" "../inc" "../common" "../public"
;         "../.." "../../include" "../../inc" "../../common" "../../public"))
; (defconst cedet-win32-include-dirs
;   (list 
;         "C:/MinGW/include"
;         "C:/MinGW/include/c++/3.4.5"
;         "C:/MinGW/include/c++/3.4.5/mingw32"
;         "C:/MinGW/include/c++/3.4.5/backward"
;         "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
;         "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
; (defconst cedet-gnu/linux-include-dirs
;   (list "/usr/include"
;         "/usr/include/c++/4.8.1"
;         "/usr/include/c++/4.8.1/tr1"
;         "/usr/include/c++/4.8.1/tr2"
;         "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.1/include"))
;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      ECB 2.40
;;

(add-to-list 'load-path
                    "~/.emacs.d/plugins/ecb-2.40")

;(require 'ecb)
;(require 'ecb-autoloads)



;; ecb
(require 'ecb-autoloads nil 'noerror)
(unless (boundp 'stack-trace-on-error)
  (defvar stack-trace-on-error nil))
(when (fboundp 'ecb-minor-mode)
  (defvar ecb-minor-mode nil)
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
        ecb-source-path '("/")
        ecb-layout-name 'left3
        ecb-toggle-layout-sequence '("left3"
                                     "left8"
                                     "left-analyse"
                                     "left-symboldef")
        ecb-windows-width 0.25
        ecb-compile-window-height 0.15
        ecb-compile-window-width 'edit-window
        ecb-compile-window-temporally-enlarge 'after-selection
        ;; ecb-enlarged-compilation-window-max-height 0.8
        ecb-tip-of-the-day nil
        ecb-auto-compatibility-check nil))
(eval-after-load "ecb"
  '(progn
     (setq ecb-cedet-required-version-max '(2 0 4 9))
     (setq ecb-compilation-buffer-names
           (append ecb-compilation-buffer-names '(("*Process List*")
                                                  ("*Proced*")
                                                  (".notes")
                                                  ("*appt-buf*")
                                                  ("*Compile-Log*")
                                                  ("*etags tmp*")
                                                  (" *svn-process*")
                                                  ("*svn-info-output*")
                                                  ("*Python Output*")
                                                  ("*Org Agenda*")
                                                  (" *EMMS Playlist*")
                                                  ("*Moccur*")
                                                  ("*Directory"))))
     (setq ecb-compilation-major-modes
           (append ecb-compilation-major-modes '(change-log-mode
                                                 calendar-mode
                                                 diary-mode
                                                 diary-fancy-display-mode
                                                 xgtags-select-mode
                                                 svn-status-mode
                                                 svn-info-mode
                                                 svn-status-diff-mode
                                                 svn-log-view-mode
                                                 svn-log-edit-mode
                                                 erc-mode
                                                 gud-mode)))))


;; ;;;;窗口间切换
(global-set-key [M-left]  'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up]    'windmove-up)
(global-set-key [M-down]  'windmove-down)

 ;;;;show&hide window
;(global-set-key [C-f1] 'ecb-hide-ecb-windows)
;(global-set-key [C-f2] 'ecb-show-ecb-windows)


;; ;;;; 使某一ecb窗口最大化
(global-set-key (kbd "C-c 0") 'ecb-hide-ecb-windows)
(global-set-key (kbd "C-c 1") 'ecb-show-ecb-windows)
(global-set-key (kbd "C-c 2") 'ecb-maximize-window-directories)
(global-set-key (kbd "C-c 3") 'ecb-maximize-window-sources)
(global-set-key (kbd "C-c 4") 'ecb-maximize-window-methods)
(global-set-key (kbd "C-c 5") 'ecb-maximize-window-history)
(global-set-key (kbd "C-c 6") 'ecb-maximize-window-analyse)
(global-set-key (kbd "C-c 7") 'ecb-restore-default-window-sizes)


;;;; 启动ECB，并且不显示每日提示
(setq ecb-auto-activate t)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;------------------------------------------------------------------------------ 
;; Java Development Enviroment 
;;------------------------------------------------------------------------------ 
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/jdee-2.4.1/lisp")) 
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/elib-1.0")) 


;; 设置JDK目录 
'(jde-jdk-registry (quote (("1.6.0_10" . "C:/Program Files (x86)/Java/jdk1.6.0_10")))) 
(setq jde-web-browser "netscape")
(setq jde-doc-dir "C:/Program Files (x86)/Java/jdk1.6.0_10/docs/")
;(jde-db-set-source-paths "C:/Program Files (x86)/Java/jdk1.6.0_10/src/")

(add-to-list 'load-path "~/.emacs.d/plugins/jdee-2.4.1/lisp")
;(require 'jde)
;; jde
(add-hook 'java-mode-hook
          '(lambda ()
             (when (require 'jde nil 'noerror)
               (setq jde-enable-abbrev-mode t))))

(defun screen-width nil -1)
(define-obsolete-function-alias 'make-local-hook 'ignore "21.1")




(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

; ;;时间显示包括日期和具体时间
; (setq display-time-day-and-date t)

; ;;时间的变化频率
; (setq display-time-interval 1)

; ;;显示时间，格式如下
; (display-time-mode 1)
; (setq display-time-24hr-format t)
; (setq display-time-day-and-date t) 


;;Font
;;(set-default-font "10x20")
;;(set-default-font "STHeiti-10")
(set-default-font "Microsoft Yahei-10")

;;(add-to-list 'default-frame-alist '(font . "STHeiti-10"))
(add-to-list 'default-frame-alist '(font . "Microsoft Yahei-10"))


;;以 y/n 替代 yes/no
(fset 'yes-or-no-p 'y-or-n-p)


;;去掉菜单栏，将F10绑定为显示菜单栏，需要菜单栏了可以摁F10调出，再摁F10就去掉菜单
;;(menu-bar-mode nil)


;;有了这段代码之后，当你按 C-c a x (x 是任意一个字符) 时，光 标就会到下一个 x 处。再次按 x，光标就到下一个 x。比如 C-c a w w w w ..., C-c a b b b b b b ...
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
             char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(define-key global-map (kbd "C-c a") 'wy-go-to-char)



;;======================            auto-header            =====================
;(add-to-list 'load-path "~/.emacs.d/plugins/")
;(require 'header2)

;;加载auto-header.el文件,自动添加文件头
(require 'auto-header)

;; 设置文件头中的姓名
(setq header-full-name "Talis Zhou")

;; 设置邮箱
(setq header-email-address "taliszhou@msn.com")

;;;; 设置每次保存时要更新的项目
(setq header-update-on-save
      '(  filename
          modified
          counter
          copyright))
;;;; 设置文件头的显示格式
(setq header-field-list
      '(  
        filename  ;文件名
        copyright ;版权
        version ;版本
        author    ;作者
        created   ;创建时间
        modified_by ;修改者
        modified ;修改时间
        update ;修改次数
        description   ;描述
        ;; blank
        ;;status  ;状态，是否发布
        ;;更新
        ;;blank
        ))
;;------------------------End auto-header-----------------------



;;------------------------------------------------------------------------------
;; session
;;------------------------------------------------------------------------------
(require 'session)
   (add-hook 'after-init-hook 'session-initialize)

   (desktop-save-mode 1)
;;------------------------End session-----------------------
 
;;;; 高亮当前行
(require 'hl-line)
(setq global-hl-line-mode t)


 (setq sql-connection-alist
      '((pool-a
         (sql-product 'mysql)
         (sql-server "192.168.2.89")
         (sql-user "root")
         (sql-password "123456")
         (sql-database "fpa_host")
         (sql-port 3306))
        (pool-b
         (sql-product 'mysql)
         (sql-server "127.0.0.1")
         (sql-user "root")
         (sql-password "123456")
         (sql-database "fpa_host")
         (sql-port 3306))))

(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name sql-connection-alist))
    (flet ((sql-get-login (&rest what)))
      (sql-product-interactive sql-product)))))

(defun mysql-a ()
  (interactive)
  (sql-connect-preset 'pool-a))

(defun mysql-b ()
  (interactive)
  (sql-connect-preset 'pool-b))

(require 'android-mode)

(custom-set-variables
 '(android-mode-avd "android233")
 '(android-mode-sdk-dir "d:/Developer/Android/android-sdk/"))
  

;; 先格式化再补全
(setq tab-always-indent 'complete)


;; 按下C-x k立即关闭掉当前的buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)


(if is-before-emacs-21
    (progn
      ;; gnuserv
      (require 'gnuserv-compat)
      (gnuserv-start)
      ;; 在当前frame打开
      (setq gnuserv-frame (selected-frame))
      ;; 打开后让emacs跳到前面来
      (setenv "GNUSERV_SHOW_EMACS" "1"))
  (if is-after-emacs-23
      (server-force-delete))
  (server-start))


;(load-file "~/.emacs.d/eproject/eproject.el")


 (condition-case err
     (progn
     (require 'xxx) )
   (error
    (message "Can't load xxx-mode %s" (cdr err))))