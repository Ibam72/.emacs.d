;;; package --- Sammury
;;; Commentary:
;;; Code:
(set-language-environment 'Japanese)

;; Coding system
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Package
(require 'package)
(setq package-user-dir "~/.emacs.d/elisp/elpa/")
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)
;(package-refresh-contents)

;;(package-install 'ac-php)
(package-install 'irony)
(package-install 'kotlin-mode)
;;(package-install 'ac-html-bootstrap)
(package-install 'twittering-mode)
(package-install 'sr-speedbar)
(package-install 'use-package)
(package-install 'company-irony)
(package-install 'hiwin)
(package-install 'linum)
(package-install 'elscreen)
(package-install 'neotree)
(package-install 'company)
(package-install 'company-web)
(package-install 'init-loader)
(package-install 'js2-mode)
(package-install 'yaml-mode)
(package-install 'emmet-mode)
(package-install 'php-mode)
(package-install 'quickrun)
(package-install 'flycheck)
(package-install 'flycheck-irony)
(package-install 'tree-mode)
(package-install 'elmacro)
(package-install 'magit)
(package-install 'go-mode)
(package-install 'company-go)
(global-flycheck-mode)

(require 'use-package)
(require 'twittering-mode)
(require 'elmacro)
(elmacro-mode)
(require 'magit)

(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face "#ffeeff")
(require 'linum)
(global-linum-mode 1)
(setq-default tab-width 4 indent-tabs-mode nil)
(require 'elscreen)
(elscreen-start)
(global-set-key (kbd "s-t") 'elscreen-create)
(global-set-key "\C-l" 'elscreen-next)
(global-set-key "\C-r" 'elscreen-previous)
(global-set-key (kbd "s-d") 'elscreen-kill)
(set-face-attribute 'elscreen-tab-background-face nil
                    :background "grey10"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-control-face nil
                    :background "grey20"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-current-screen-face nil
                    :background "grey20"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-other-screen-face nil
                    :background "grey30"
                    :foreground "grey60")
(require 'neotree)
(global-set-key "\C-o" 'neotree-toggle)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-<f5>") 'next-buffer)
(global-set-key (kbd "M-<f5>") 'previous-buffer)

(global-set-key (kbd "<f6>") 'buffer-menu)
(global-set-key (kbd "C-<f6>") 'other-window)
(global-set-key (kbd "M-<f6>") 'other-frame)

(global-set-key (kbd "<f7>") 'kill-buffer)
(global-set-key (kbd "C-<f7>") 'delete-window)
(global-set-key (kbd "M-<f7>") 'delete-frame)

(global-set-key (kbd "<f8>") 'eshell)
;;(global-set-key (kbd "C-<f8>") ')
;;(global-set-key (kbd "M-<f8>") ')

(global-set-key (kbd "<f9>") 'replace-string)
;;(global-set-key (kbd "C-<f9>") ')
;;(global-set-key (kbd "M-<f9>") ')

(global-set-key (kbd "<f10>") 'quickrun)
(global-set-key (kbd "C-<f10>") 'quickrun-with-arg)
(global-set-key (kbd "M-<f10>") 'quickrun-compile-only)

(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)

;; Company
(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 4)
(setq company-selection-wrap-around t)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 4)))
;; C,C++
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-to-list 'company-backends 'company-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(require 'flycheck)
;(add-hook 'c++-mode-hook
;          (lambda () (setq flycheck-clang-include-path
;                           (list (expand-file-name "~/work/gitlab/galaxy/src/")))))

;; C,C++ compile option
(setq irony-lang-compile-option-alist
      (quote ((c++-mode . "c++ -std=c++17 -lstdc++")
              (c-mode . "c")
              (objc-mode . "objective-c"))))
(defun ad-irony--lang-compile-option ()
  (defvar irony-lang-compile-option-alist)
  (let ((it (cdr-safe (assq major-mode irony-lang-compile-option-alist))))
    (when it (append '("-x") (split-string it "\s")))))
(advice-add 'irony--lang-compile-option :override #'ad-irony--lang-compile-option)

;; tern && js2-mode 4 javasprict
(require 'tern)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;(setq company-tern-property-marker "")
(defun company-tern-depth (candidate)
  "Return depth attribute for CANDIDATE.'nil' entries are treated as 0."
  (let ((depth (get-text-property 0 'depth candidate)))
    (if (eq depth nil) 0 depth)))
(add-hook 'js2-mode-hook 'tern-mode)
(add-to-list 'company-backends 'company-tern)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; ac-php
;; before this use,
;; comannd "touch .ac-php-conf.json"
;; and     " M-x ac-php-remake-tags-all
;; in top derectory
;;(require 'cl)
;;(require 'php-mode)
;;(add-hook 'php-mode-hook
;;            '(lambda ()
;;               (auto-complete-mode t)
;;               (setq ac-sources  '(ac-source-php ) )
;;               (yas-global-mode 1)
;;               (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
;;               (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
;;               ))



(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

;(setq web-mode-engines-alist
;'(("php"    . "\\.phtml\\'")
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-arguments nil)
 '(package-selected-packages
   (quote
    (go-mode company-go flycheck php-mode company linum irony yaml-mode web-mode use-package twittering-mode tree-mode sr-speedbar quickrun neotree markdown-mode magit kotlin-mode js2-mode init-loader hiwin flycheck-irony emmet-mode elscreen elmacro company-web company-tern company-irony-c-headers company-irony ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Goのパスを通す
(add-to-list 'exec-path (expand-file-name "/usr/local/bin/go"))
;; go get で入れたツールのパスを通す
(add-to-list 'exec-path (expand-file-name "/Users/karen-kamishiro/dev/bin"))

;; flycheck-modeを有効化してシンタックスエラーを検知
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
       (add-hook 'before-save-hook' 'gofmt-before-save)
       (local-set-key (kbd "M-.") 'godef-jump)
       (set (make-local-variable 'company-backends) '(company-go))
       (setq indent-tabs-mode nil)    ; タブを利用
       (setq c-basic-offset 4)    ; tabサイズを4にする
       (setq tab-width 4)))

;; company-modeとの連携してコード補完する
(require 'company-go)
(add-hook 'go-mode-hook (lambda()
      (company-mode)
      (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
      (setq company-idle-delay 0) ; 遅延なしにすぐ表示
      (setq company-minimum-prefix-length 3) ; デフォルトは4
      (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
      (setq completion-ignore-case t)
      (setq company-dabbrev-downcase nil)
      (global-set-key (kbd "C-M-i") 'company-complete)
      ;; C-n, C-pで補完候補を次/前の候補を選択
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous)
      (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
      (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
      (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
      ))
