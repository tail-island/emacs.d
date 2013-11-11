;; 機種を判定します。

(defvar mac?
  (eq system-type 'darwin))

(defvar linux?
  (eq system-type 'gnu/linux))

(defvar windows?
  (eq system-type 'windows-nt))

;; load-pathを設定します。

(defun set-load-path ()
  (let ((default-directory (expand-file-name (concat user-emacs-directory "elisp"))))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; exec-pathを設定します。

(defun set-exec-path ()
  (when mac?
    (setq exec-path (append '("/opt/local/bin") exec-path))))

;; package.el

(defun init-package-el ()
  (require 'package)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))

;; 言語を設定します。

(defun set-language-for-mac ()
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

(defun set-language-for-windows ()
  (setq default-file-name-coding-system 'cp932))

(defun set-language ()
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (when mac?
    (set-language-for-mac))
  (when windows?
    (set-language-for-windows)))

;; フォントを設定します。

(defun set-face-for-mac ()
  (set-face-attribute 'default nil :family "Menlo" :height 120)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic Pro"))
  (setq face-font-rescale-alist '((".*Menlo.*"    . 1.0)
                                  (".*Hiragino.*" . 1.2))))

(defun set-face-for-linux ()
  (set-face-attribute 'default nil :family "VL Gothic" :height 90))

(defun set-face-for-windows ()
  (set-face-attribute 'default nil :family "VL Gothic" :height 105))

(defun set-face ()
  (custom-set-faces
   '(default ((t (:background "black" :foreground "white"))))
   '(cursor ((((class color)
               (background dark))
              (:background "black"))
             (((class color)
               (background light))
              (:background "white"))
             (t ()))))
  (when window-system
    (when mac?
      (set-face-for-mac))
    (when linux?
      (set-face-for-linux))
    (when windows?
      (set-face-for-windows))))

;; 見た目を設定します。

(defun set-appearance-for-mac ()
  (setq-default line-spacing 2))

(defun set-appearance-for-linux ()
  (menu-bar-mode 0))

(defun set-appearance-for-windows ()
  (menu-bar-mode 0))

(defun set-appearance ()
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (fringe-mode 0)
  (column-number-mode t)
  (when mac?
    (set-appearance-for-mac))
  (when linux?
    (set-appearance-for-linux))
  (when windows?
    (set-appearance-for-windows)))

;; 動作を設定します。

(defun set-behavior ()
  (setq inhibit-startup-message t)
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (when linux?
    (setq x-select-enable-clipboard t)))

;; インデントを設定します。

(defun set-indent ()
  (setq c-basic-offset    2)
  (setq css-indent-offset 2)
  (setq js-indent-level   2))

;; キーボードを設定します。

(defun set-keyboard-for-mac ()
  (define-key global-map (kbd "M-c") 'kill-ring-save)  ; KeyRemap4MacBookがM-wをM-cに割り当てるので、再割当てします。
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super))

(defun set-keyboard ()
  (define-key global-map (kbd "RET") 'newline-and-indent)
  (define-key global-map (kbd "C-t") 'toggle-truncate-lines)
  (define-key global-map (kbd "C-o") 'other-window)
  (define-key global-map (kbd "M-y") 'anything-show-kill-ring)
  (require 'dired)
  (define-key dired-mode-map (kbd "C-o") 'other-window)
  (when mac?
    (set-keyboard-for-mac)))

;; Input Methodを設定します。

(defun init-input-method-for-mac ()
  (setq default-input-method "MacOSX"))

(defun init-input-method-for-linux ()
  (require 'mozc)
  (setq default-input-method "japanese-mozc"))

(defun init-input-method-for-windows ()
  (w32-ime-initialize)
  (setq w32-ime-show-mode-line t)
  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-inidicator-list
        '("[--]" "[あ]" "[--]"))
  (setq default-input-method "W32-IME"))

(defun init-input-method ()
  (when mac?
    (init-input-method-for-mac))
  (when linux?
    (init-input-method-for-linux))
  (when windows?
    (init-input-method-for-windows)))

;; anythingを設定します。

(defun init-anything ()
  (define-key global-map (kbd "C-;") 'anything)
  (put 'upcase-region 'disabled nil))

;; clojure-modeを設定します。

(defun init-clojure-mode ()
  (require 'clojure-mode)
  (define-clojure-indent
    (cond                  0)
    (as->                  2)
    (cond->                1)
    (cond->>               1)
    (some->                1)
    (some->>               1)
    (thrown-with-msg?      2)
    (thrown?               1)
    (query                 1)
    (execute!              1)
    (db-transaction        1)
    (defroutes             'defun)
    (GET                   2)
    (POST                  2)
    (PUT                   2)
    (DELETE                2)
    (HEAD                  2)
    (ANY                   2)
    (context               2)
    (hitokotonushi-session 0)
    (form-for              1)
    (weave-aspect          2)
    (condp\'               1)))

;; nXML modeを設定します。

(defun nxml-mode-hook-handler ()
  (auto-fill-mode 0))

(defun init-nxml-mode ()
  (add-hook 'nxml-mode-hook
            'nxml-mode-hook-handler))

;; Ruby modeを設定します。

(defun ruby-mode-hook-handler ()
  (inf-ruby-keys))

(defun init-ruby-mode ()
  (require 'ruby-mode)
  (add-to-list 'auto-mode-alist
               '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist
               '("ruby"   . ruby-mode))
  (require 'inf-ruby)
  (add-hook 'ruby-mode-hook
            'ruby-mode-hook-handler)
  (setq exec-path (append '("~/.rvm/bin") exec-path))
  (setq ruby-deep-indent-paren-style nil))

;; Rinariを設定します。

(defun rhtml-mode-hook-handler ()
  (rinari-launch)
  (auto-fill-mode -1))

(defun init-rinari ()
  (require 'rinari)
  (require 'rhtml-mode)
  (add-hook 'rhtml-mode-hook
            'rhtml-mode-hook-handler))

;; Text modeを設定します。

(defun text-mode-hook-handler ()
  (auto-fill-mode t))

(defun init-text-mode ()
  (add-hook 'text-mode-hook
            'text-mode-hook-handler))

;; Emacs Lisp modeを設定します。

(defun emacs-lisp-mode-hook-handler ()
  (require 'eldoc)
  (turn-on-eldoc-mode))

(defun init-emacs-lisp-mode ()
  (add-hook 'emacs-lisp-mode-hook
            'emacs-lisp-mode-hook-handler))

;; これまでに定義した関数を呼び出して、実際の設定をします。

(set-load-path)
(set-exec-path)

(init-package-el)

(set-language)
(set-face)
(set-appearance)
(set-behavior)
(set-indent)
(set-keyboard)

(init-input-method)
(init-anything)
(init-clojure-mode)
(init-nxml-mode)
;; (init-ruby-mode)
;; (init-rinari)
(init-text-mode)
(init-emacs-lisp-mode)
