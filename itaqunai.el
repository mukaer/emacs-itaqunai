;; itaqunai.el   :interaction quick native insert

;; Copyright (C) 2012
;; (mukaer atmark gmail period com)
;; Version: 0.0.3
;; Last-Updated: 2012-04-03 20:30:00
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
;; 1 Copy hash-lib.el to your load-path
;;    URL: https://raw.github.com/mukaer/emacs-hash-lib/master/hash-lib.el
;;
;;
;; 2 Copy itaqunai.el to your load-path and add to your ~/.emacs
;;    URL: https://raw.github.com/mukaer/emacs-itaqunai/master/itaqunai.el
;;
;;   (require 'itaqunai)
;;

;;; Customize:
;;
;;  (setq itaqunai-config 
;; 	(append-hash itaqunai-config 
;; 		   (list-to-hash 
;; 		    '(
;; 		      ruby-mode '("command" "~/.rbenv/shims/ruby"
;; 				  "header_befor_search"	 "^[ \t]*\\(require\\)\s+\\('\\|\"\\)\\(\\w+\\)\\('\\|\"\\)"
;; 				  "header"   ""
;; 				  "footer"   ""
;; 				  )
;; 				php-mode  '("header_befor_search" '("1" "^[ \t]*\\(require\\)\s+\\('\\|\"\\)\\(\\w+\\)\\('\\|\"\\)"
;; 							   "2" "^[ \t]*\\(include\\)\s+\\('\\|\"\\)\\(\\w+\\)\\('\\|\"\\)"
;; 							   )
;; 					    )
;; 				)))
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



;;; Code:

(require 'cl)
(require 'hash-lib)

(defvar itaqunai-tmp-script-file
   "/tmp/itaqunai-tmp-script-file")

(defvar itaqunai-config
   (list-to-hash 
    '(
      default '("command" "cat"
		"haeder_befor_search" ""
		"header"              ""
		"haeder_after_search" ""
		"footer_befor_search" ""
		"footer"	      ""
		"footer_after_search" ""
		)

	      ruby-mode '(
		  "command" "/usr/local/bin/ruby"
		  "header"    ""
		  "footer"    "")

		php-mode '(
			   "command" "/usr/loca/bin/php"
			   "header"    ""
			   "footer"    ""
			   )

		js2-mode '(
			   "command" "/usr/loca/bin/node"
			   "header"    ""
			   "footer"    ""
			   )

		sh-mode '(
			   "command" "/bin/bash"
			   "header"    ""
			   "footer"    ""
			   ))))


(defun itaqunai-exec ()
  (interactive)
  ;mark set
  (if mark-active
      ;true
      (itaqunai-multiliner)

    ;false
    (itaqunai-oneliner))
)
; M-: (itaqunai-exec)
; mark set
; M-: (itaqunai-exec)



(defun itaqunai-oneliner ()
  (let (
	(beg (itaqunai-begin-point))
	(end (itaqunai-end-point))
	)

  (if (not (eq beg end ))
	  (progn
	    (itaqunai-make-script   beg end )
	    (itaqunai-insert-result)
	    (goto-char end)))))
;itaqunai-oneliner



(defun itaqunai-multiliner ()
  (let (
	(beg (itaqunai-multi-begin-point))
	(end (itaqunai-multi-end-point))
	)

  (goto-char end)
  (itaqunai-make-script   beg end)
  (itaqunai-insert-result)
  (goto-char end)))
; mark set
; M-x itaqunai-multiliner



(defun itaqunai-make-script(start end )
  (let ((curbuf (current-buffer))
	(mode major-mode)
	(h_befor (itaqunai-code-search "haeder_befor_search"))
	(h_after (itaqunai-code-search "header_after_search"))
	(f_befor (itaqunai-code-search "footer_befor_search"))
	(f_after (itaqunai-code-search "footer_after_search"))
	)

    ;バッファ位置保持し、下記フォーム実行
    (with-temp-buffer
      ;h_befor
      (if  h_befor  (insert h_befor))

      ;header
      (let ((cont (get-hash itaqunai-config  mode "header" )))
	(if (> 0 (length cont)) (insert cont)))

      ;h_after
      (if  h_after  (insert h_after))


      ;現在のbufferに内容入れる  curbufのstart end間の内容
      (insert-buffer-substring curbuf start end)


      ;f_befor
      (if  f_befor  (insert f_befor))

      ;footer
      (let ((cont (get-hash itaqunai-config  mode "footer" )))
	(if (> 0 (length cont)) (insert cont)))

      ;f_after
      (if  f_after  (insert f_after))

      ;buffer 内容をファイルに保存
      (write-region (point-min) (point-max) itaqunai-tmp-script-file ))))
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
  (let (( now-point (point)))
    (beginning-of-line)
    (let ((beg-point (point)))
      (goto-char now-point)
      beg-point)))


;行末 point取得
(defun itaqunai-end-point ()
  (let ((now-point (point)) )
    (end-of-line)
    (let ((end-point (point)) )
      (goto-char now-point)
      end-point)))

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

(defun itaqunai-program-path ()
  (let ( ( mode  (get-hash itaqunai-config  major-mode "command" )))
    (if mode
	mode
      (get-hash itaqunai-config 'default "command"))))


(defun itaqunai-code-search (option)
  (let (
	(search_ins (get-hash itaqunai-config major-mode option)))
    (if search_ins
	(if (typep search_ins 'hash-table)
	    ;true
	    (let (index)
	      (loop for key being the hash-keys of search_ins using (hash-values val)
		    do (push (itaqunai-re-search val) index))
 
	      (mapconcat 'concat (nreverse index) "\n")
	      )
 
	  ;false
	  (itaqunai-re-search search_ins)))))

(defun itaqunai-re-search (regxp)

  (let (index )
    (save-excursion
      (goto-char (point-min))
      (while
	  (re-search-forward regxp (point-max) t)
	(push  (match-string-no-properties 0)  index))

      (mapconcat 'concat (nreverse index) "\n")

)))

(provide 'itaqunai)