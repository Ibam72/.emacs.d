;; Language
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
;;(package-refresh-contents)


(package-install 'ac-php)
(package-install 'irony)
(package-install 'kotlin-mode)
(package-install 'ac-html-bootstrap)
(package-install 'twittering-mode)
(package-install 'sr-speedbar)
(package-install 'use-package)


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
(global-flycheck-mode)

(add-to-list 'exec-path "C:/Program Files/LLVM/bin") ;;windows
(require 'use-package)
(require 'twittering-mode)
(require 'elmacro)
(elmacro-mode)
(require 'magit)

;; init load
;;(require 'init-loader)
;;(setq init-loader-show-log-after-init nil)
;;(init-loader-load "~/.emacs.d/inits/")

;; emacs style
;;(add-to-list 'default-frame-alist '(font . "ricty-12")) ;mac
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(setq custom-theme-directory "~/.emacs.d/themes")
(require 'hiwin)
(hiwin-activate)
(set-face-background 'hiwin-face "#ffeeff")
(require 'linum)
(global-linum-mode 1)
(setq default-tab-width 4)
; elscreen
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
;; neotree
(require 'neotree)
(global-set-key "\C-o" 'neotree-toggle)

;; macro
;;(let frame-parameter buffer-list)
;;(let frame-selected-window )
;;(fset 'kill-buffer-and-frame
;;	  [() ?\S-\C-e ?\M-w ?\C-e ?\C-j ?\C-a ?\S-\C-e ?\C-y ?\C-a])
;; Keys

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

;;(global-set-key (kbd "<f9>") 'eshell)
;;(global-set-key (kbd "C-<f9>") ')
;;(global-set-key (kbd "M-<f9>") ')

(global-set-key (kbd "<f10>") 'quickrun)
(global-set-key (kbd "C-<f10>") 'quickrun-with-arg)
(global-set-key (kbd "M-<f10>") 'quickrun-compile-only)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(twitering-mode tree-mode company-web company-axiom magit ac-html-bootstrap kotlin-mode elmacro sr-speedbar flycheck-irony init-loader quickrun ac-php yaml-mode neotree elscreen linum hiwin el-get use-package markdown-mode emmet-mode web-mode php-mode js2-mode flycheck company-irony irony company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)

;; Company
(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 4)
(setq company-selection-wrap-around t)
(setq w32-pipe-read-delay 0) ; for windows


;; C,C++
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-to-list 'company-backends 'company-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; C,C++ compile option
(setq irony-lang-compile-option-alist
      '((c++-mode . ("c++" "-std=c++17" "-lstdc++" "-lm"))
        (c-mode . ("c"))
        (objc-mode . '("objective-c"))))
(defun irony--lang-compile-option ()
  (irony--awhen (cdr-safe (assq major-mode irony-lang-compile-option-alist))
    (append '("-x") it)))


 ;; tern && js2-mode 4 javasprict
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq company-tern-property-marker "")
(defun company-tern-depth (candidate)
  "Return depth attribute for CANDIDATE. 'nil' entries are treated as 0."
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
(require 'cl)
(require 'php-mode)
(add-hook 'php-mode-hook
            '(lambda ()
               (auto-complete-mode t)
               (require 'ac-php)
               (setq ac-sources  '(ac-source-php ) )
               (yas-global-mode 1)
               (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
               (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
               ))



(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
  ("blade"  . "\\.blade\\.")))
(require 'company-web-html)
(require 'company-web-jade) 
(require 'company-web-slim)
(define-key web-mode-map (kbd "C-'") 'company-web-html)
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil))
(keyboard-translate ?\C-i ?\H-i) ;;C-i と Tabの被りを回避
(define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line) ;; C-i で展開

;;markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
