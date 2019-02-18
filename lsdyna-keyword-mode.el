;;; lsdyna-keyword-mode.el --- summary -*- lexical-binding: t -*-

;; Author: ISHIDA Tatsuhiro
;; Version: version 0.1
;; Package-Requires:
;; Homepage: https://github.com/dsjt/lsdyna-keyword-mode
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
;; This library provides major mode `lsdyna-keyword-mode' which helps
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
;;    (require 'lsdyna-keyword-mode)
;;    (add-to-list 'auto-mode-alist '("\\.k$\\|\\.key$" . lsdyna-keyword-mode))
;;
;; If you want to use hide-show.el in keyword files, add the following.
;;
;;    (add-hook 'lsdyna-keyword-mode-hook '(lambda () (hs-minor-mode 1)))
;;    (add-to-list 'hs-special-modes-alist lsky-hs-special)


;;; Code:

(require 's)
(eval-when-compile (require 'cl))


;;; Constants =================================================================

(defconst lsky-version "0.1")

(defconst lsky-keywords-regexp
  (regexp-opt '("*KEYWORD" "*end")))


;;; Global Variables ==========================================================

(defface lsky-bg
  '((t (:underline t)))
  "background color 1 for lsdyna keyword mode.")

(defvar lsky-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-a") 'lsky-beginning-of-defun)
    (define-key map (kbd "C-M-e") 'lsky-end-of-defun)
    (define-key map (kbd "C-M-f") 'lsky-forward-defun)
    (define-key map (kbd "C-M-b") 'lsky-backward-defun)
    (define-key map (kbd "C-M-h") 'lsky-mark-defun)
    map)
  "Keymap for `lsdyna-keyword-mode'.")

(defvar lsky-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?$ "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `lsdyna-keyword-mode'.")

(defvar lsky-hs-special
  '(lsdyna-keyword-mode "\\*" "" "\\$" lsky-end-of-defun nil))

(defvar lsky-value-regexp
  ".\\([ 0-9a-zA-Z.\\&-]\\{9\\}\\)")

(defvar lsky-font-lock-keywords
  `((,lsky-keywords-regexp . font-lock-constant-face)
    ("\\*.+\n" . font-lock-function-name-face)
    ("^[^\\& ]\\{11\\}.*" . font-lock-defaults)
    ,@(loop for i from 1 to 8
            collect `(,(s-concat "^" (s-repeat i lsky-value-regexp))
                      . ,(loop for j from 1 to i
                               collect `(,j 'lsky-bg)))))
  "Keyword highlighting specification for `lsdyna-keyword-mode'.")

(defvar lsdyna-keyword-imenu-generic-expression
  '(("CARD" "^\\(\\*.+\\)$" 1)
    ("BLOCK" "^\\$+-+\\s-*\\(.+\\)\\s--*\\$*$" 1)))

(defvar lsky-outline-regexp
  "\\*.+$")


;;; Functions ==========================================================

(defun lsky-beginning-of-block ()
  (interactive)
  (if (re-search-backward markdown-regex-block-separator nil t)
      (goto-char (or (match-end 2) (match-end 0)))
    (goto-char (point-min))))

(defvar lsky-regex-defun
  "^\\*.*")

;;;###autoload
(defun lsky-beginning-of-defun (&optional arg)
  "`beginning-of-defun-function' for lsdyna keyword files"
  (interactive "P")
  (or arg (setq arg 1))
  (if (looking-at lsky-regex-defun)
      (beginning-of-line)
    (or (re-search-backward lsky-regex-defun nil nil arg)
        (goto-char (point-min)))))

;;;###autoload
(defun lsky-end-of-defun (&optional arg)
  "`end-of-defun-function' for lsdyna keyword files."
  (interactive "P")
  (or arg (setq arg 1))
  (when (looking-at lsky-regex-defun)
    (goto-char (match-beginning 0))
    (forward-char 1))
  (if (re-search-forward lsky-regex-defun nil nil arg)
      (goto-char (match-beginning 0))
    (goto-char (point-max)))
  (backward-char 1)
  (point))

;;;###autoload
(defun lsky-forward-defun (&optional arg)
  (interactive "P")
  (or arg (setq arg 1))
  (when (looking-at lsky-regex-defun)
    (goto-char (match-beginning 0))
    (forward-char 1))
  (progn (re-search-forward lsky-regex-defun nil nil arg)
         (beginning-of-line)))

;;;###autoload
(defun lsky-backward-defun (&optional arg)
  (interactive "P")
  (or arg (setq arg 1))
  (or (re-search-backward lsky-regex-defun nil nil arg)
      (goto-char (point-min))))

;;;###autoload
(defun lsky-0padding-and-yank (&optional arg)
  (interactive "P")
  (or arg (setq arg 10))
  (insert (format (format "%%0%dd" 10) (string-to-number (car kill-ring)))))

;;;###autoload
(defun lsky-padding-and-yank (&optional arg)
  (interactive "P")
  (or arg (setq arg 10))
  (insert (format (format "%%%dd" 10) (string-to-number (car kill-ring)))))

;;;###autoload
(defun lsky-mark-defun (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (and (< arg 0) (setq arg (- arg 1)))
  (when (not (equal last-command 'lsky-mark-defun))
    (lsky-beginning-of-defun)
    (set-mark (point)))
  (lsky-forward-defun arg))

;;;###autoload
(define-derived-mode lsdyna-keyword-mode
  nil
  "LSKey"
  "Major mode which helps you edit ls-dyna keyword files."
  :syntax-table lsky-syntax-table
  (setq-local comment-start "$")
  (setq-local comment-start-skip "$+\\s-*")
  (setq-local comment-style 'plain)
  (setq-local font-lock-defaults '(lsky-font-lock-keywords nil nil))
  (setq-local imenu-generic-expression
              lsdyna-keyword-imenu-generic-expression)
  (setq-local outline-regexp lsky-outline-regexp)
  (setq-local paragraph-start "^\\*")
  (setq-local paragraph-separate "^\\$$")
  (use-local-map lsky-map))


(provide 'lsdyna-keyword-mode)

;;; lsdyna-keyword-mode.el ends here
