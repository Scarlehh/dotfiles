;;; -------------
;;; Bootstrapping
;;; -------------
;; Melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))


;;; ------------
;;; Line Numbers
;;; ------------
;; Turn on line numbers
(global-linum-mode 1)

;; Relative line numbers
(linum-relative-mode 1)
(setq linum-relative-current-symbol "")

;; Default linum string
(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-relative-format
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "s\u2502"))))))

;;; -----------
;;; Indentation
;;; -----------
;; Default indentation with 4 space tabs
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64
						68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; C tab with 4 space tabs
(defvaralias 'c-basic-offset 'tab-width)
;(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

;; Python tab with 4 spaces
(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)))

;; Latex indentation
(setq LaTeX-indent-level 4)
(setq LaTeX-item-indent 0)
(setq TeX-brace-indent-level 4)

;; Javascript tab with 4 spaces
(add-hook 'js-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)))


;;; --------
;;; Web Mode
;;; --------
(require 'web-mode)

;; Auto-load
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; Hooks
(defun my-web-mode-hook ()
  ;; Indentation
  (setq-default indent-tabs-mode nil)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

		  
;;; -------
;;; CUSTOM
;;; -------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(vhdl-indent-tabs-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
