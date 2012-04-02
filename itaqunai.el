;; itaqunai.el   :interaction quick native insert

;; Copyright (C) 2012
;; (mukaer atmark gmail period com)
;; Version: 0.0.1
;; Last-Updated: 2012-04-02 17:00:00
;; URL: https://github.com/mukaer

;; This file is NOT a part of GNU Emacs.

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Installation:
;;
;; Copy itaqunai.el to your load-path and add to your ~/.emacs
;;
;;   (require 'itaqunai)

;;; Customize:
;;
;;  (defun itaqunai-program-path ()
;;    (cond
;;      ((string= major-mode "ruby-mode")
;;       "~/.rbenv/shims/ruby")
;;      ((string= major-mode "php-mode")
;;       "/usr/local/bin/php")
;;      ((string= major-mode "js2-mode")
;;       "/usr/local/bin/node")
;;      ((string= major-mode "cperl-mode")
;;       "/usr/bin/perl")
;;      ((string= major-mode "sh-mode")
;;       "bash")
;;      (t "cat")))
;;
;;
;;    (setq itaqunai-tmp-script-file
;;      "/dev/shm/itaqunai-tmp-script-file")
;;
;;   (add-hook 
;;   	'ruby-mode-hook
;;   	    '(lambda ()
;;   		(define-key ruby-mode-map (kbd "C-m") 'newline-and-indent)
;;   		(define-key ruby-mode-map (kbd "C-j") 'itaqunai-exec)
;;   	))




;;; Change log:
;;
;; * Init:
;;   * mukaer:
;;     * version: 0.0.1



;;; Code:



(defvar itaqunai-tmp-script-file
  "/tmp/itaqunai-tmp-script-file")


(defun itaqunai-exec ()
  (interactive)
  ;mark set check
  (if mark-active
      ;true
      (itaqunai-multiliner)
    ;false
    (itaqunai-oneliner)))
; M-: (itaqunai-exec)
; mark set
; M-: (itaqunai-exec)



(defun itaqunai-oneliner ()
  (setq beg (itaqunai-begin-point))
  (setq end (itaqunai-end-point))

  (if (not (eq beg end ))
	  (progn
	    (itaqunai-make-script   beg end )
	    (itaqunai-insert-result)
	    (goto-char end))))
;itaqunai-oneliner



(defun itaqunai-multiliner ()
  (setq beg (itaqunai-multi-begin-point))
  (setq end (itaqunai-multi-end-point))

  (goto-char end)
  (itaqunai-make-script   beg end)
  (itaqunai-insert-result)
  (goto-char end))
; mark set
; M-x itaqunai-multiliner



(defun itaqunai-make-script(start end )

  (setq curbuf (current-buffer))
  ;バッファ位置保持し、下記フォーム実行
  (with-temp-buffer

    ;現在のbufferに内容入れる  curbufのstart end間の内容
    (insert-buffer-substring curbuf start end)

    ;buffer 内容をファイルに保存
    (write-region (point-min) (point-max) itaqunai-tmp-script-file )))

;(itaqunai-make-script 1  20)


(defun itaqunai-insert-result ()
  ;文字挿入
  (insert "\n"
	  ;program 起動
	  (shell-command-to-string
	   ;コマンド用文字列結合
	   (concat (itaqunai-program-path) " " itaqunai-tmp-script-file))))
;(itaqunai-insert-result (point))


;行頭 point取得
(defun itaqunai-begin-point ()
  (setq now-point (point))
  (beginning-of-line)
  (setq beg-point (point))
  (goto-char now-point)
  beg-point)


;行末 point取得
(defun itaqunai-end-point ()
  (setq now-point (point))
  (end-of-line)
  (setq end-point (point))
  (goto-char now-point)
  end-point)

;multiline先頭 point取得
(defun itaqunai-multi-begin-point ()
  (if (< (mark) (point))
      (mark)
    (point)))

;multiline終了 point取得
(defun itaqunai-multi-end-point ()
  (if (< (mark) (point))
      (point)
    (mark)))


(provide 'itaqunai)