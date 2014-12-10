;; 機種を判定します。

(defvar mac?
  (eq system-type 'darwin))

(defvar linux?
  (eq system-type 'gnu/linux))

(defvar windows?
  (eq system-type 'windows-nt))

;; package.el

(defun init-package-el ()
  (require 'package)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
  (package-initialize))

;; load-pathを設定します。

(defun set-load-path ()
  (let ((default-directory (expand-file-name (concat user-emacs-directory "elisp"))))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; exec-pathを設定します。

(defun set-exec-path ()
  (when mac?
    (exec-path-from-shell-initialize)))

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
  (set-face-attribute 'default nil :family "VL Gothic" :height 120))
  ;; (set-face-attribute 'default nil :family "Menlo" :height 120)
  ;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic Pro"))
  ;; (setq face-font-rescale-alist '((".*Menlo.*"    . 1.0)
  ;;                                 (".*Hiragino.*" . 1.2))))

(defun set-face-for-linux ()
  (set-face-attribute 'default nil :family "VL Gothic" :height 120))

(defun set-face-for-windows ()
  (set-face-attribute 'default nil :family "VL Gothic" :height 105))

(defun set-face ()
  (custom-set-faces
   '(default ((t (:background "black" :foreground "white")))))
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
  (menu-bar-mode 0)
  (setq-default line-spacing 2))

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
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (require 'dired)
  (define-key dired-mode-map (kbd "C-o") 'other-window)
  (when mac?
    (set-keyboard-for-mac))
  (when linux?
    (define-key global-map (kbd "<mouse-6>") 'scroll-right)
    (define-key global-map (kbd "<mouse-7>") 'scroll-left)))

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

;; helmを設定します。

(defun init-helm ()
  (define-key global-map (kbd "C-;") 'helm-for-files)
  (put 'upcase-region 'disabled nil))

;; clojure-modeを設定します。

(defun init-clojure-mode ()
  (require 'clojure-mode)
  (put 'm/letfn\' 'clojure-backtracking-indent '((2) 2))
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
    (condp\'               1)
    (this-as               1)
    (keep-state            1)
    (let-node-value        1)
    (letfn\'               1)
    )
  (setq nrepl-hide-special-buffers t)
  )

;; Haskell-modeを設定します。

(defun haskell-mode-hook-handler ()
  (turn-on-haskell-indentation))

(defun init-haskell-mode ()
  (add-hook 'haskell-mode-hook
            'haskell-mode-hook-handler))

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

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; Rinariを設定します。

(defun rhtml-mode-hook-handler ()
  (rinari-launch)
  (auto-fill-mode -1))

(defun init-rinari ()
  (require 'rinari)
  (require 'rhtml-mode)
  (add-hook 'rhtml-mode-hook
            'rhtml-mode-hook-handler))

;; coffee-modeを設定します。

(defun coffee-mode-hook-handler ()
  (set (make-local-variable 'tab-width) 2)
  (setq coffee-tab-width 2))

(defun init-coffee-mode ()
  (add-hook 'coffee-mode-hook
            'coffee-mode-hook-handler))

;; Emacs Lisp modeを設定します。

(defun emacs-lisp-mode-hook-handler ()
  (require 'eldoc)
  (turn-on-eldoc-mode))

(defun init-emacs-lisp-mode ()
  (add-hook 'emacs-lisp-mode-hook
            'emacs-lisp-mode-hook-handler))

;; nXML modeを設定します。

(defun nxml-mode-hook-handler ()
  (auto-fill-mode 0))

(defun init-nxml-mode ()
  (add-hook 'nxml-mode-hook
            'nxml-mode-hook-handler))

;; html-modeを設定します。

(defun html-mode-hook-handler ()
  (turn-off-auto-fill))

(defun init-html-mode ()
  (add-hook 'html-mode-hook
            'html-mode-hook-handler))

;; css-modeを設定します。
(defun init-css-mode ()
  (setq auto-mode-alist
        (cons '("\\.css.scss\\'" . css-mode) auto-mode-alist)))

;; text-modeを設定します。

(defun text-mode-hook-handler ()
  (auto-fill-mode t))

(defun init-text-mode ()
  (add-hook 'text-mode-hook
            'text-mode-hook-handler))

;; これまでに定義した関数を呼び出して、実際の設定をします。

(init-package-el)

(set-load-path)
(set-exec-path)
(set-language)
(set-face)
(set-appearance)
(set-behavior)
(set-indent)
(set-keyboard)

(init-input-method)
(init-helm)
(init-clojure-mode)
(init-haskell-mode)
(init-ruby-mode)
;; (init-rinari)
(init-coffee-mode)
(init-emacs-lisp-mode)
(init-nxml-mode)
(init-html-mode)
(init-css-mode)
(init-text-mode)
