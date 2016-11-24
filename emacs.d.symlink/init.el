;;; -------------
;;; BOOTSTRAPPING
;;; -------------

;; Melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; --------
;;; SETTINGS
;;; --------

;;; ----------------
;;; System Clipboard
;;; ----------------

;; Copy
(defun copy-to-x-clipboard()
  (interactive)
  (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "xsel -ib")
        (message "Yanked region to clipboard!")
        (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

;; Paste
(defun paste-from-x-clipboard()
  (interactive)
  (shell-command "xsel -ob")
  (insert-buffer "*Shell Command Output*")
  (kill-buffer "*Shell Command Output*"))


;;; -----------
;;; Key Binding
;;; -----------

(global-set-key (kbd "C-o") (kbd "C-e RET"))
(global-set-key (kbd "C-j") (kbd "C-a RET <up>"))
(global-set-key (kbd "C-x C-w") 'copy-to-x-clipboard)
(global-set-key (kbd "C-x C-y") 'paste-from-x-clipboard)
;; (global-set-key (kbd "DEL") 'backward-delete-char)


;;; -----------
;;; Indentation
;;; -----------

;; Default indentation with 4 space tabs
(setq-default indent-tabs-mode t
              tab-width 4)


;;; --------
;;; PACKAGES
;;; --------

;; Install packages that are used
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-defer  t
      use-package-always-ensure t)

;; Defined packages
(use-package tex
  :ensure auctex
  :init
  ;; LaTeX
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (setq LaTeX-indent-level tab-width
                    LaTeX-item-indent 0
                    TeX-brace-indent-level tab-width
                    indent-tabs-mode t))))

(use-package autorevert
  :init
  ;; Auto refresh buffer after save
  (global-auto-revert-mode))

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package avy-zap
  :bind
  ("M-z" . avy-zap-up-to-char))

(use-package cc-mode
  :config
  ;; C 4 space tabs
  (setq c-basic-offset tab-width
        c-backspace-function #'backward-delete-char))

(use-package csharp-mode)

(use-package haskell-mode)

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package js
  :init
  ;; Javascript 4 space tabs
  (add-hook 'js-mode-hook
            (lambda ()
              (setq js-indent-level 4))))

(use-package linum-relative
  :init
  (global-linum-mode)
  (linum-relative-mode)
  :config
  ;; Relative line numbers
  (setq linum-relative-current-symbol "")
  ;; Default linum string
  (unless (display-graphic-p)
    (add-hook 'linum-before-numbering-hook
              (lambda ()
                (setq-local
                 linum-relative-format
                 (let ((lines (count-lines (point-min) (point-max))))
                   (format "%%%ds│" (length (number-to-string lines)))))))))

(use-package markdown-mode)

(use-package prolog
  :mode ("\\.pl\\'" . prolog-mode)
  :config
  (setq prolog-system 'swi))

(use-package python
  :init
  ;; Python 4 spaces
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil))))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package vhdl-mode
  :config
  (setq-default vhdl-indent-tabs-mode t))

(use-package web-beautify)

(use-package web-mode
  :init
  (add-hook 'web-mode-hook
            (lambda()
              (setq web-mode-markup-indent-offset 4    ; HTML
                    web-mode-css-indent-offset    4    ; CSS
                    web-mode-code-indent-offset   4))) ; JS/PHP/etc.
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode))

(use-package windmove
  :init
  ;; Enable window moving
  (windmove-default-keybindings))


;;; -------
;;; CUSTOM
;;; -------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (org auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
