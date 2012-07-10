;; 機種を判定します。

(defvar mac?
  (eq system-type 'darwin))

(defvar linux?
  (eq system-type 'gnu/linux))

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

(defun set-language-for-japanese ()
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8))

(defun set-language-for-mac ()
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

(defun set-language ()
  (set-language-for-japanese)
  (when mac?
    (set-language-for-mac)))

;; フォントを設定します。

(defun set-font-for-mac ()
  (set-face-attribute 'default nil :family "Menlo" :height 120)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic Pro"))
  (setq face-font-rescale-alist '((".*Menlo.*"    . 1.0)
                                  (".*Hiragino.*" . 1.2))))

(defun set-font-for-linux ()
  (set-face-attribute 'default nil :family "VL Gothic" :height 105))

(defun set-font ()
  (when window-system
    (when mac?
      (set-font-for-mac))
    (when linux?
      (set-font-for-linux))))

;; 見た目を設定します。

(defun set-appearance ()
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (fringe-mode 0)
  (column-number-mode t)
  (when mac?
    (setq-default line-spacing 4))
  (when linux?
    (setq-default line-spacing 2)
    (menu-bar-mode 0)))

;; 動作を設定します。

(defun set-behavior ()
  (setq inhibit-startup-message t)
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (when linux?
    (setq x-select-enable-clipboard t)))

;; キーボードを設定します。

(defun set-keyboard ()
  (define-key global-map (kbd "RET") 'newline-and-indent)
  (define-key global-map (kbd "C-t") 'toggle-truncate-lines)
  (define-key global-map (kbd "C-o") 'other-window)
  (define-key global-map (kbd "M-y") 'anything-show-kill-ring)
  (require 'dired)
  (define-key dired-mode-map (kbd "C-o") 'other-window)
  (when mac?
    (define-key global-map (kbd "M-c") 'kill-ring-save)  ; KeyRemap4MacBookがM-wをM-cに割り当てるので、再割当てします。
    (setq ns-command-modifier 'meta)
    (setq ns-alternate-modifier 'super)))

;; Input Methodを設定します。

(defun init-input-method-for-mac ()
  (setq default-input-method "MacOSX"))

(defun init-input-method-for-linux ()
  (require 'mozc)
  (setq default-input-method "japanese-mozc"))

(defun init-input-method ()
  (when mac?
    (init-input-method-for-mac))
  (when linux?
    (init-input-method-for-linux)))

;; anythingを設定します。

(defun init-anything ()
  (define-key global-map (kbd "C-;") 'anything)
  (put 'upcase-region 'disabled nil))

;; Emacs Lisp modeを設定します。

(defun emacs-lisp-mode-hook-handler ()
  (require 'eldoc)
  (turn-on-eldoc-mode))

(defun init-emacs-lisp-mode ()
  (add-hook 'emacs-lisp-mode-hook
            'emacs-lisp-mode-hook-handler))

;; Text modeを設定します。

(defun text-mode-hook-handler ()
  (auto-fill-mode t))

(defun init-text-mode ()
  (add-hook 'text-mode-hook
            'text-mode-hook-handler))

;; これまでに定義した関数を呼び出して、実際の設定をします。

(set-load-path)
(set-exec-path)

(init-package-el)

(set-language)
(set-font)
(set-appearance)
(set-behavior)
(set-keyboard)

(init-input-method)
(init-anything)
(init-emacs-lisp-mode)
(init-text-mode)
