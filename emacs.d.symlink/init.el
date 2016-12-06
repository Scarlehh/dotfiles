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

;;; -----------
;;; Definitions
;;; -----------

(defun fast-line-count ()
  "Return number of lines within accessible portion of buffer.
See URL `http://emacs.stackexchange.com/a/3822' for limitations
and URL `https://github.com/basil-conto/dotfiles/blob/master/\
.emacs.d/init.el' for inspiration."
  (let ((line-number-display-limit-width most-positive-fixnum) ; Any line width
        (line-number-display-limit nil)                        ; Any buffer size
        (pmax (point-max)))
    (save-excursion
      (goto-char pmax)
      ;; Adjust for trailing newline
      (funcall (if (= pmax (line-beginning-position)) #'1- #'identity)
               (string-to-number (format-mode-line "%l"))))))


;;; ----------------
;;; System Clipboard
;;; ----------------

;; Copy
(defun copy-to-x-clipboard (&optional start end)
  (interactive "r")
  (let ((code (shell-command-on-region
               (or start (region-beginning))
               (or end   (region-end))
               "xsel -ib")))
    (if (/= code 0)
        (message "xsel returned error code %d" code)
      (message "Yanked region to clipboard!")
      (deactivate-mark))))

;; Paste
(defun paste-from-x-clipboard ()
  (interactive)
  (let ((code (call-process-shell-command "xsel -ob" nil t)))
    (unless (zerop code)
      (message "xsel returned error code %d" code))))


;;; -----------
;;; Indentation
;;; -----------

;; Default indentation with 4 space tabs
(setq-default indent-tabs-mode t
              tab-width 4)


;;; ---------
;;; Scrolling
;;; ---------

;; keyboard scroll one line at a time
(setq scroll-step 1)


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

(use-package bm
  :init
  (defun bm-at-line (line-number)
	(interactive "*nEnter relative line number: ")
	(save-excursion
	  (forward-line line-number)
	  (bm-toggle)))
  :bind
  ("C-x p t" . bm-at-line)
  ("C-x p s" . bm-toggle)
  ("C-x p n" . bm-next)
  ("C-x p p" . pm-previous))

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
  :config
  ;; JavaScript 4 space tabs
  (setq-default js-indent-level 4))

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
                 (let ((lines (fast-line-count)))
                   (format "%%%ds│" (length (number-to-string lines)))))))))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (defun orgtbl-to-gfm (table params)
	"Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
	(let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
								 org-table-last-alignment ""))
		   (params2
			(list
			 :splice t
			 :hline (concat alignment "|")
			 :lstart "| " :lend " |" :sep " | ")))
	  (orgtbl-to-generic table (org-combine-plists params2 params))))
  (defun md-table-insert (table-name)
	(interactive "*sEnter table name: ")
	(insert "<!---
#+ORGTBL: SEND " table-name " orgtbl-to-gfm

-->
<!--- BEGIN RECEIVE ORGTBL " table-name " -->
<!--- END RECEIVE ORGTBL " table-name " -->")
	(previous-line)
	(previous-line)
	(previous-line)))

(use-package org)

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
  :config
  (setq-default
   web-mode-markup-indent-offset 4      ; HTML
   web-mode-css-indent-offset    4      ; CSS
   web-mode-code-indent-offset   4)     ; JS/PHP/etc.
  :mode
  "\\.html?\\'"
  "\\.phtml\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'")

(use-package whitespace
  :init
  (global-whitespace-mode)
  :config
  (setq-default whitespace-style '(face
                                   trailing  ; trailing blanks
                                   empty)))  ; empty start/end of buffer

(use-package windmove
  :init
  ;; Enable window moving
  (windmove-default-keybindings))


;;; -----------
;;; Key Binding
;;; -----------

(bind-keys
 ;; ("DEL"     . backward-delete-char)
 ;; `bind-keys' interpets "C-e RET" literally, so use keyboard macro syntax ;_;
 ("C-o"     . "\C-e\C-m")
 ("C-j"     . "\C-a\C-m\C-p")
 ("C-x C-w" . copy-to-x-clipboard)
 ("C-x C-y" . paste-from-x-clipboard)
 ("C-x w"   . whitespace-cleanup)
 ("M-k"     . "\C-k\C-y"))


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
	(bookmark+ auto-capitalize auctex web-mode web-beautify use-package undo-tree swiper markdown-mode linum-relative haskell-mode goto-chg csharp-mode avy-zap))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
