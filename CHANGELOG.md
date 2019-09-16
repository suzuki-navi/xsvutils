# CHANGELOG

To see version information, `xsvutils --version`.

To see compatibility information, `xsvutils help compatibility`.

## v13 -> v14 (Unreleased)

出力オプション `--o-chart2` が追加された。

従来の実行時のJava依存の機能が削除され、 --install-rt, --jvm は廃止された。

以下のコマンドが廃止された。
- cutidx
- stridx

Scalaによる実装のパーサを実験的に動かすための --x-jvm が追加された。
--x-jvm はまだほとんどの機能が実装されていない。


## v12 -> v13 (2019/08/13)

-v12 --install のバグを解決し -v13 --install はできるようにした。
-v12 --install 自体は解決できない。

~/.xsvutils/repos-build の中のソースを手動修正してある場合でも強制的に上書きして
-vXX --install を実行できるようにした。

git clone 以外の方法でソースをダウンロードしてきた場合にビルドできない問題を解消したつもり。

入力の文字コードがSJISの場合にiconvでの指定を SHIFT-JIS から cp932 に変更した。


## v11 -> v12 (2019/07/17)

以下のサブコマンドが追加された。
- mcut

以下のコマンドがRustでの実装に置き換わり、高速化された。
- insconst

insconst に依存していた sort コマンドも高速化となった。

CSVからTSVへの変換がGo言語からRustでの実装に置き換わり、CSVファイルの読み込みが高速化された。

--install-rt はJavaが必要となるまでは実行しなくてもいいように変更した。

[追記]
-v12 --install にはバグがありビルドできない。 -v13 --install を使うこと。


## v10 -> v11 (2019/06/29)

Rustソースのビルド時にOSの共有オブジェクトへの動的リンクをなくして、バイナリのポータビリティを改善した。

GithubのレポジトリのURLを変更した。

sbtがインストールできないバグを修正した。


## v9 -> v10 (2018/12/03)

以下のコマンドがRustでの実装に置き換わり、高速化された。
- cut
- uriparams

src/install-openjdk.sh でJavaインストールできないバグを修正した。


## v8 -> v9 (2018/10/14)

※ `-v9` は v10 で修正された src/install-openjdk.sh のバグにより動かないかもしれない。

以下のコマンドが追加された。
- cutidx (--jvm のみ)
- stridx (--jvm のみ)

以下のコマンドを --jvm にて対応した。
- filter
- where

以下のオプションが追加された。
- filter コマンドの --stridx (--jvm のみ)
- where コマンドの --stridx (--jvm のみ)

--install-rt の仕組みによりJavaをシステムインストールのものからxsvutils自身の管理に変更した。


## v7 -> v8 (2018/09/26)

xsvutils 異常終了時に子プロセスが残ってしまうことがある問題を改善した。

以下のコマンドの進捗表示を廃止した。
- wcl

コマンドラインパーサのScalaでの再実装を目指して実験的に --jvm オプションを追加した。

以下のコマンドを --jvm にて対応した。
- cut
- update
- wcl

端末へのテーブル表示時に罫線の表示に罫線素片を利用するようにした。
また、行の先頭と末尾に罫線を表示しないようにした。


## v6 -> v7 (2018/09/12)

以下のオプションが追加された。
- cols コマンドの --remove
- uriparams コマンドの --sjis

端末へのテーブル表示時に途中にあるヘッダ行を反転表示するように変更した。


## v5 -> v6 (2018/08/06)

以下のコマンドが追加された。
- inslinenum
- expandmultivalue

以下のオプションが追加された。
- grep コマンドの -v

uriparamsの入力に `?...` や `http://.../...?...` の形式を許容するように変更した。

uriparams --name-list で生成するカラム名をオプション名に合わせて `names` から `name-list` に変更した。

エラーのハンドリングを少し強化した。

`--o-diffable` での空行でのバグを修正した。


## v4 -> v5 (2018/07/16)

以下のコマンドが追加された。
- assemblematrix
- grep

入力ファイルの圧縮フォーマットとしてxzがサポートされた。

標準入力を明示的に指定するための `-` が追加された。

sort コマンドで数値の逆順ソートの機能が追加された。

filter, where コマンドに `=~`, `!~` 演算子が追加された。

insconst のパラメータの順序がもとに戻った。 (see `xsvutils help insconst`)

以下のコマンドが削除された。
- insweek        (use -v1)
- addconst       (use insconst)
- addcopy        (use inscopy)
- addlinenum     (use -v1)
- addcross       (use -v1)
- addmap         (use insmap)
- parseuriparams (use uriparams)
- wordsflags     (use -v1)

以下のオプションが廃止された。
- --i-header     (use --header)
- paste, join, unin コマンドの --right オプション (use --file)


## 0.2.7 -> v4

以下のコマンドが追加された。
- mergecols
- rmnoname
- insunixtime
- ratio

以下のオプションが追加された。
- --o-chart
- cols コマンドの --last
- diff コマンドの -b, -w

互換性を維持するための -v1, -v2, -v3, -v4 オプションの仕組みが追加された。

insconst のパラメータの順序が逆になってしまった。 (see `xsvutils help insconst`)

