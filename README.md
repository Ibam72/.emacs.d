emascの設定
===

Overview


あとでCaskかel-getでパッケージ管理する(package.elでいい気がする)

init-loader使おうよ(https://qiita.com/tadsan/items/181a352edcda740582ec)

そのうち書き直そう

require ではなくuse-package 使うか検討

# install packages list

* use-packages
* init-loader
* hiwin
* linum
* elscreen
* neotree
* company
* irony
* js2-mode
* web-mode
* yaml-mode
* emmet-mode
* php-mode
* ac-php
* flycheck
* flycheck-irony

# 導入予定

定型文挿入		yasnippet (導入予定)(emmetあるから少し分からない)emmet-mode有効時に無効化するか干渉するやつ制限するか

rtags　　　　　　https://qiita.com/MitsutakaTakeda/items/2f526a85ad39424a8363

## Javasprict

補完機能			term + company

## php 

* php-mode

* ac-php

"touch .ac-php-conf.json"
"M-x ac-php-remake-tags-all"


# 注意事項

*C/C++　C++のコンパイルオプションが17になっていることに注意(パスが通ってないので使えない)
*irony-server-install時にllvmとかcmakeが必要。macだとこける(どこかに入れ方がある)。windowsでもこける(cmakeのllvmの参照先が"program file (x86)"ではなく"program file"だったのが問題だった)
