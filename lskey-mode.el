;;; lskey-mode.el --- summary -*- lexical-binding: t -*-

;; Author: ISHIDA Tatsuhiro
;; Version: version 0.1
;; Package-Requires:
;; Homepage: https://github.com/dsjt/lskey-mode
;; Keywords:

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Overview
;; ========
;;
;; This library provides major mode `lskey-mode' which helps
;; you edit ls-dyna keyword files.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using "C-h v
;; load-path RET" within Emacs), then add the following line to your
;; .emacs startup file:
;;
;;    (require 'lskey-mode)
;;    (add-to-list 'auto-mode-alist '("\\.k$\\|\\.key$" . lskey-mode))
;;
;; If you want to use hide-show.el in keyword files, add the following.
;;
;;    (add-hook 'lskey-mode-hook '(lambda () (hs-minor-mode 1)))
;;    (add-to-list 'hs-special-modes-alist lskey-hs-special)


;;; Code:

(require 's)
(eval-when-compile (require 'cl))


;;; Constants =================================================================

(defconst lskey-version "0.1")

(defconst lskey-keywords-regexp
  (regexp-opt '("*KEYWORD" "*end")))


;;; Global Variables ==========================================================

(defface lskey-bg
  '((t (:underline t)))
  "background color 1 for lsdyna keyword mode.")

(defvar lskey-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-a") 'lskey-beginning-of-defun)
    (define-key map (kbd "C-M-e") 'lskey-end-of-defun)
    (define-key map (kbd "C-M-f") 'lskey-forward-defun)
    (define-key map (kbd "C-M-b") 'lskey-backward-defun)
    (define-key map (kbd "C-M-h") 'lskey-mark-defun)
    map)
  "Keymap for `lskey-mode'.")

(defvar lskey-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?$ "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `lskey-mode'.")

(defvar lskey-hs-special
  '(lskey-mode "\\*" "" "\\$" lskey-end-of-defun nil))

(defvar lskey-value-regexp
  ".\\([ 0-9a-zA-Z.\\&+-]\\{9\\}\\)")

(defvar lskey-font-lock-keywords
  `((,lskey-keywords-regexp . font-lock-constant-face)
    ("\\*.+\n" . font-lock-function-name-face)
    ("^[^\\& ]\\{11\\}.*" . font-lock-defaults)
    ,@(loop for i from 1 to 8
            collect `(,(s-concat "^" (s-repeat i lskey-value-regexp))
                      . ,(loop for j from 1 to i
                               collect `(,j 'lskey-bg)))))
  "Keyword highlighting specification for `lskey-mode'.")

(defvar lskey-imenu-generic-expression
  '(("CARD" "^\\(\\*.+\\)$" 1)
    ("BLOCK" "^\\$+-+\\s-*\\(.+\\)\\s--*\\$*$" 1)))

(defvar lskey-outline-regexp
  "\\*.+$")


;;; Functions ==========================================================

(defun lskey-beginning-of-block ()
  (interactive)
  (if (re-search-backward markdown-regex-block-separator nil t)
      (goto-char (or (match-end 2) (match-end 0)))
    (goto-char (point-min))))

(defvar lskey-regex-defun
  "^\\*.*")

;;;###autoload
(defun lskey-beginning-of-defun (&optional arg)
  "`beginning-of-defun-function' for lsdyna keyword files"
  (interactive "P")
  (or arg (setq arg 1))
  (if (looking-at lskey-regex-defun)
      (beginning-of-line)
    (or (re-search-backward lskey-regex-defun nil nil arg)
        (goto-char (point-min)))))

;;;###autoload
(defun lskey-end-of-defun (&optional arg)
  "`end-of-defun-function' for lsdyna keyword files."
  (interactive "P")
  (or arg (setq arg 1))
  (when (looking-at lskey-regex-defun)
    (goto-char (match-beginning 0))
    (forward-char 1))
  (if (re-search-forward lskey-regex-defun nil nil arg)
      (goto-char (match-beginning 0))
    (goto-char (point-max)))
  (backward-char 1)
  (point))

;;;###autoload
(defun lskey-forward-defun (&optional arg)
  (interactive "P")
  (or arg (setq arg 1))
  (when (looking-at lskey-regex-defun)
    (goto-char (match-beginning 0))
    (forward-char 1))
  (progn (re-search-forward lskey-regex-defun nil nil arg)
         (beginning-of-line)))

;;;###autoload
(defun lskey-backward-defun (&optional arg)
  (interactive "P")
  (or arg (setq arg 1))
  (or (re-search-backward lskey-regex-defun nil nil arg)
      (goto-char (point-min))))

;;;###autoload
(defun lskey-0padding-and-yank (&optional arg)
  (interactive "P")
  (or arg (setq arg 10))
  (insert (format (format "%%0%dd" 10) (string-to-number (car kill-ring)))))

;;;###autoload
(defun lskey-padding-and-yank (&optional arg)
  (interactive "P")
  (or arg (setq arg 10))
  (insert (format (format "%%%dd" 10) (string-to-number (car kill-ring)))))

;;;###autoload
(defun lskey-mark-defun (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (and (< arg 0) (setq arg (- arg 1)))
  (when (not (equal last-command 'lskey-mark-defun))
    (lskey-beginning-of-defun)
    (set-mark (point)))
  (lskey-forward-defun arg))

;;;###autoload
(define-derived-mode lskey-mode
  nil
  "LSKey"
  "Major mode which helps you edit ls-dyna keyword files."
  :syntax-table lskey-syntax-table
  (setq-local comment-start "$")
  (setq-local comment-start-skip "$+\\s-*")
  (setq-local comment-style 'plain)
  (setq-local font-lock-defaults '(lskey-font-lock-keywords nil nil))
  (setq-local imenu-generic-expression
              lskey-imenu-generic-expression)
  (setq-local outline-regexp lskey-outline-regexp)
  (setq-local paragraph-start "^\\*")
  (setq-local paragraph-separate "^\\$$")
  (use-local-map lskey-map))


(provide 'lskey-mode)

;;; lskey-mode.el ends here
