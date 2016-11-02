;;; -------------
;;; BOOTSTRAPPING
;;; -------------
;; Melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; Custom Packages
(add-to-list 'load-path "./custom-packages/")

;; Enable window moving
(windmove-default-keybindings)

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

;; Ivy
(ivy-mode 1)


;;; -----------
;;; Key Binding
;;; -----------
(global-set-key (kbd "C-o") (kbd "C-e RET"))
(global-set-key (kbd "C-j") (kbd "C-a RET <up>"))
(global-set-key (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "C-x C-w") 'copy-to-x-clipboard)
(global-set-key (kbd "C-x C-y") 'paste-from-x-clipboard)


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

;; C 4 space tabs
(setq c-basic-offset tab-width)
										;(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)

;; Python 4 spaces
(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)))

;; Latex
(setq LaTeX-indent-level tab-width)
(setq LaTeX-item-indent 0)
(setq TeX-brace-indent-level tab-width)

;; Javascript 4 space tabs
(add-hook 'js-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(setq js-indent-level 2)))


;;; -----
;;; Modes
;;; -----
;; Web Mode
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun my-web-mode-hook ()
  (setq-default indent-tabs-mode nil)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
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
 '(package-selected-packages
   (quote
	(avy ivy haskell-mode web-beautify csharp-mode org web-mode markdown-mode linum-relative auctex)))
 '(vhdl-indent-tabs-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
