;;; -------------
;;; BOOTSTRAPPING
;;; -------------
;; Melpa
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)

;; Enable window moving
(windmove-default-keybindings)


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
(setq use-package-always-ensure t)

;; Defined packages
(use-package tex
  :ensure auctex
  :defer)

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package avy-zap
  :bind
  ("M-z" . avy-zap-up-to-char))

(use-package csharp-mode
  :defer)

(use-package haskell-mode
  :defer)

(use-package ivy
  :defer
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package linum-relative
  :init
  (global-linum-mode 1)
  (linum-relative-mode 1)
  :config
  ;; Relative line numbers
  (setq linum-relative-current-symbol "")
  ;; Default linum string
  (unless window-system
	(add-hook 'linum-before-numbering-hook
			  (lambda ()
				(setq-local linum-relative-format
							(let ((w (length (number-to-string
											  (count-lines (point-min) (point-max))))))
							  (concat "%" (number-to-string w) "s\u2502")))))))

(use-package markdown-mode
  :defer)

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package web-beautify
  :defer)

(use-package web-mode
  :defer
  :init
  (add-hook 'web-mode-hook
			(lambda()
			  ;; HTML
			  (setq web-mode-markup-indent-offset 4)
			  ;; CSS
			  (setq web-mode-css-indent-offset 4)
			  ;; JS/PHP/etc
			  (setq web-mode-code-indent-offset 4)))
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode))


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


;;; --------
;;; Enabling
;;; --------
;; Auto refresh buffer after save
(global-auto-revert-mode t)


;;; -----------
;;; Key Binding
;;; -----------
(global-set-key (kbd "C-o") (kbd "C-e RET"))
(global-set-key (kbd "C-j") (kbd "C-a RET <up>"))
(global-set-key (kbd "C-x C-w") 'copy-to-x-clipboard)
(global-set-key (kbd "C-x C-y") 'paste-from-x-clipboard)


;;; -----------
;;; Indentation
;;; -----------
;; Default indentation with 4 space tabs
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64
						68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; C 4 space tabs
(setq c-basic-offset tab-width)
;(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

;;; -----------------
;;; PROGRAMMING HOOKS
;;; -----------------
;; Python 4 spaces
(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)))

;; LaTeX
(add-hook 'LaTeX-mode-hook
		  (lambda()
			(setq LaTeX-indent-level tab-width)
			(setq LaTeX-item-indent 0)
			(setq TeX-brace-indent-level tab-width)
			(setq indent-tabs-mode t)))

;; Javascript 4 space tabs
(add-hook 'js-mode-hook
		  (lambda ()
			(setq js-indent-level 4)))


;;; -----
;;; Modes
;;; -----			
;; Prolog
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
							  auto-mode-alist))

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
 '(package-selected-packages (quote (org auctex)))
 '(vhdl-indent-tabs-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
