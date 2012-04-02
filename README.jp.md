# Interaction Quick Native Interpreter: itaqunai ～いたくない～
emacsの編集画面でワンライナーを実行し結果を貼り付け

lisp-interaction-modeの機能劣化版みたいなものです。

できることは２つ

- ワンライナー（１行コード）
- マルチラインライナー（複数行コード）


## できることその１:ワンライナー

1. コードを書く  
    編集画面（current-buffer）に

        print "Hello World !"

2. `M-x itaqunai-exec` とする  
    行は移動せずそのまま


3. 下の行に結果が表示される！

        "Hello World !"

## できることその２:複数行指定

1. コードを書く  
    編集画面（current-buffer）に

        str="Hello World !"
        print str

2. Markset`C-Space`で指定部分を選択する

        ( markset start )
        str="Hello World !"
        print str
        ( markset end )

3. `M-x itaqunai-exec` とする


4. 下の行に結果が表示される！

        "Hello World !"


## cliで動く各種スクリプト言語で動作します。

- ruby
- perl
- javascript with node.js
- php
- bash


その他、拡張はご自由に！


## Installation

1. itaqunai.elのロードパスへの設置

        * https://github.com/emacs-itaqunai/itaqunai.el

2. `.emacs`の記載

    (require 'itaqunai)

    ;各種パスの指定
    (defun itaqunai-program-path ()
      (cond 
        ((string= major-mode "ruby-mode")
             "~/.rbenv/shims/ruby")
        ((string= major-mode "php-mode")
             "/usr/local/bin/php")
        ((string= major-mode "js2-mode")
             "/usr/local/bin/node")
        ((string= major-mode "cperl-mode")
             "/usr/bin/perl")
        (t "cat")
         ))
    
    ;一時ファイルの作成場所指定
    (defvar itaqunai-tmp-script-file
      "/dev/shm/itaqunai-tmp-script-file")


3. キーバインドの設定（必要な場合)

    ;例　ruby-mode C-j割り当て
    (add-hook 
        'ruby-mode-hook
            '(lambda ()
                (define-key ruby-mode-map (kbd "C-m") 'newline-and-indent)
                (define-key ruby-mode-map (kbd "C-j") 'itaqunai-exec)
        ))


## Todo
* 指定の記述を挿入する
    `#!/bin/bash`などを行頭にする
* 指定の記述がある行を勝手に取ってくる。
    ruby `require 'webrick'`  php `include 'hogehoge.inc'`
* ブロックなど簡単に取り込めるように。
    rubyだと `if end`  php `if{ }`
* 実行時間の制限をつける？
    スクリプトに埋め込み？ phpだと limit timeを関数で指定できたはず・・・
* 変数などを格納した場合保持する？
    その際 print系は削除する。　あとはエラー処理が問題？
* major-mode 化？
