#+TITLE:漁海況業務ノート
* 漁獲量
** とりまとめファイルの準備
*** DONE 「各県いわし_向.xls」をNASの前回予報のフォルダからコピーし，日付でリネーム
*** DONE 昨年の集計シート「YYYYMM漁海況」をコピーし，リネーム．
同じ月のシートをコピーすること．でないと参照セルの関係で困ることになる．
10月漁海況なら昨年10月のもの．
3月漁海況なら昨年3月のもの．
*** DONE 「YYYYMM漁海況」シートの当年，前年の集計用セルの参照を更新．
ただし平年値の計算式は更新しなくてよい．
** DONE 県単位の作業
県ごとに一気にやると，二度手間が少ない．
*** DONE データ集計，値のチェック
各県の漁獲量を集計し，県の集計値が違っていないかをチェックする．
前年，平年の値もチェックする．
- 10月予報
2〜8月分のデータを整理する（漁期は4〜8月）．
漁獲量の平年値は当年を含まない．今年がn年なら平年値の集計期間は(n-5):(n-1)年
- 3月予報
9〜1月分のデータを整理する．

*** DONE 予報文を各県いわしシートにコピー．
各県の漁況を整理する．
妥当性を確認．
機械的に値を比較するだけでなく，特異的な値についても考慮する．
例えば，昨年の漁獲量が特異的に高かった場合，数値自体は前年を下回っても，平年と比較するとかなり高い，という場合も生じる．
OKならばpptスライドの「各県予報とまとめ」に転記する．

** 来季の予報時系列

*** モデルによる予測

**** 時系列
[[/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/fmtcatch/R/forcast.R][スクリプト]]
**** ただのGLM
[[/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/fmtcatch/R/gyokaikyo_2017_katakuchi_GLM_170315.txt][スクリプト]]
** スライト用グラフの作成
漁獲量や体長のデータ，どうやって読み込もうか．
*** 漁獲量
漁獲量データは，手作業で集計したのでRのオブジェクトがない．
「各県いわし」ファイルを読み込むか．仕方ない．
[[/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/fmtcatch/make_list.R][「各県いわし」を整形してメモリ上にリスト形式で格納する関数]]を作った

*** 体長
[[/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/fmtbl/R/plot_figs.R][★カタクチを読み込んでグラフを書くスクリプト]]
体長データもせっかく★カタクチファイルがあるので，これを読み込むか．

*** 資源量
#+BEGIN_SRC R :session *R:gyokaikyo*
  param <- list(lwd = 7,
                lwd.axis = 2,
                pch = list(pred = 16,
                           real = 21),
                pcol = list(pred = hsv(20/360, 0.8, 0.7),
                            pred.bg = hsv(20/360, 0.4, 0.95),
                            real = hsv(0, 0, 0),
                            recent = hsv(0, 0, 0.3),
                            recent.bg = hsv(0, 0, 0.6, alpha = 0.7)),
                cex = 3,
                cex.axis = 5,
                year = 2018,
                myfont = "HiraKakuProN-W3")

  baa <- read.csv("/Users/ahayashi/Documents/GitHub/stockAssess/output/resrvpa_baa.csv") %>%
    mutate(ssb = (age1_ton + age2_ton)/1000) %>%
    filter(between(year, 1995, param$year))
  lastdata <- baa %>%
    filter(year == param$year-1)
  blimit <- 91

  png("../../figs/biomass.png", width = 1700, height = 1100)
  par(family = param$myfont, mai=  c(1, 2.2, 0.5, 4), omi = c(2, 2, 0, 0))
  plot(baa$year, baa$ssb, axes = FALSE, ann = FALSE, yaxs = "i",
       type = "n", xlim = c(1995, param$year - 1), ylim = c(0, 400))
  axis(2, at = seq(0, 300, 100), las = 2, cex.axis = param$cex.axis, lwd = param$lwd.axis)
  axis(1, at = seq(1970, param$year, 5), las = 1, cex.axis = param$cex.axis, lwd = 0, pos = -30)
  axis(1, at = seq(1970, param$year, 5), labels = FALSE, lwd = param$lwd.axis)
  axis(1, at = seq(1970, param$year, 1), tcl = 0, labels = FALSE, lwd = param$lwd.axis)
  rect(param$year - 5.5, 0, param$year - 0.5, 300, col = hsv(147/360, 1, 0.7, 0.7), border = FALSE)
  abline(h = blimit, lwd = 3, lty = 2)
  lines(baa$year, baa$ssb, lwd = param$lwd)
  points(baa$year, baa$ssb, pch = 16, lwd = param$lwd, cex = param$cex)
  points(lastdata$year, lastdata$ssb, pch = 21, lwd = param$lwd, cex = param$cex + 3, bg = hsv(200/360, 0.7, 0.7))
  text(param$year+2, blimit, "Blimit", cex = param$cex.axis, xpd = TRUE)
  mtext("親魚量（千トン）", side = 2, cex = param$cex.axis, outer = TRUE)
  mtext("年", side = 1, cex = param$cex.axis, outer = TRUE, line = 5)
  dev.off()
#+END_SRC
* 体長組成
** ★カタクチファイルを準備する
**** 10月
資源評価のフォルダから，「★カタクチYY-1.xlsx」ファイルをコピーし，リネーム．

例）2018（平成30）年10月の漁海況の場合は，
/Volumes/評価研/資源評価/平成30年/カタクチイワシ/★カタクチ17.xlsx
をコピーし，「★カタクチ18.xlsx」とする．
**** 3月

** データ整理
各県のデータを整理し，「★カタクチ」ファイルの該当シートに貼り付ける．
* 各県ごとの作業進捗とメモ，スクリプト
漁獲量については，2018年10月のデータは手作業で集計した．
**** DONE 山口
***** DONE 漁獲量
[[][]]
****** データ形式
2018湊銘柄別水揚表.xlsx
というファイル一つのみ．
2004年から，集計対象が湊だけになった．
****** 作業メモ
******* 県の集計値チェック
県の月別集計値は不明だが，合計値は合っているので良しとする
| 月 | 県集計値 | 林集計値 |
|----+----------+----------|
|  1 |          |    186.1 |
|  2 |          |   350.96 |
|  3 |          |   147.58 |
|  4 |          |     1.92 |
|  5 |          |     1.92 |
|  6 |          |    47.78 |
|  7 |          |    94.48 |
|  8 |          |   101.46 |
******* 比較値チェック
前年613.98トン．OK．
平年値もOK．
***** DONE 予報と漁況
前年851.9トンを下回り，平年388.1トン並みとの予報．
311〜668トンくらいになると想像しているらしい．
前年は特異的に高かったためだろう．
予報は問題ないと思われる．

***** DONE 体長
精密と体長から作った体長組成が一致したことを確認

**** DONE 福岡
***** DONE 漁獲量
****** データ形式
S51_H30経年福岡漁獲量データ(提出分).xlsx
というファイルに，「まき網」と「棒受け網」というシートがある．どちらも集計する．
****** 前年・平年値チェック
7.6トン，前年比，平年比問題なし．
***** DONE 予報と漁況
例年「散発的に漁獲される」と予報している．
確かに，西海ブロックの中では一番漁獲量が少ない．
OKとする．

***** CANCELLED 体長
県提出の体長組成だが，例年カタクチは測定が無い．
**** DONE 佐賀
***** DONE 漁獲量
****** データ形式
佐賀県_予報対象種漁獲量.xls
というファイルの，「カタクチ（唐津港県内船）」のみを集計する．
「カタクチ（中まき）」「カタクチ（定置）」シートを集計したものが「カタクチ（唐津港県内船）」シート．
****** 県集計値のチェック
4〜8月の合計値がおかしい．
4.3トンとの報告だが，7.7トンではないか．
-> 県から訂正があった（8トン）．okとする．
***** DONE 予報と漁況
前年15.3トン，平年62.9トンをともに下回る予報．
12.2トンを下回ると予想していることになり，かなり悲観的な予報．
平年の中でもかなり少ない前年をさらに下回ると予想したのは，恐らく前期の漁況（7.7トン，平年の中で第4位）が前年（32.1トン）を下回っているからだろう．
かなり悲観的だが，具体的な反論もないので，OKとする．
***** DONE 体長
fresco．
平成30年度はデータがなかった．ウルメもなかったらしい．
**** DONE 長崎
***** DONE 漁獲量
****** データ形式について
九十九島，奈留，橘湾，長崎魚市のデータがある．
一応，みな同じ形式．
2〜8月と9〜1月にシートが分けられている．
一番若いシートは1996年．
****** 作業メモ
今週中に値のチェックが必要なので，手作業でやる．
******* 4港の漁獲量を整理
******* 県の集計値が違っていないかをチェック
| 港       | 県集計 |  林集計 | 備考                    |
|----------+--------+---------+-------------------------|
| 長崎魚市 |   1105 | 1104.66 | ok                      |
| 奈留     |      0 |       0 | ok                      |
| 九十九島 |   3857 | 4444.50 | 4445トンと 訂正があった |
| 橘湾     |    371 |  371.27 | ok                      |
******* 前年・平年値をチェック
前年値OK
平年値 6195トンとなっているが，6199.5トンではないか．
-> 恐らく，過去の値がこっそり更新されていることによっていると思われる．
　　複雑になりそうなので，依田さんと相談して今回はよしとすることにした．
   「各県いわし」ファイルを，過去の値を反映して更新しないと行けない（いずれ）．
***** DONE 予報と漁況
前年（940.3トン）を上回り，平年（1918.9トン）並みとの予報．
1535〜2302トンと予想していることになる．
恐らく，前年が平年の中では特異的に低かったためだろう．
OKとする．
***** DONE 体長
県提出体長組成．前年ファイルを参照する必要あり．
資源評価の時のスクリプトを使う．
[[file:/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/fmtbl/R/nagasaki.R][スクリプト]]
**** DONE 熊本
***** DONE 漁獲量
****** データ形式
01_H30まき網漁獲量（熊本県）_.xls
の「カタクチイワシ」シートを整理する．
棒受網（02_H30棒受網漁獲量（熊本県）.xls）は整理しなくてよい（資源評価では使う？）．
****** 県の集計値
集計値OK．
前年・平年値もOK．
***** DONE 予報と漁況
前年（509.8トン）並みで，平年（989.6トン）を下回るという予報．
407〜611.8トンと予想していることになる．
後期の漁況が直近3年で下むいているからだろうか．
前期の経過は，平年の中でトップだったので，予報はやや控えめな印象．
反論の具体的材料がないので，OKとする．
***** DONE 体長
県提出体長組成．前年ファイルも送ってくれる．
漁海況ではまき網のデータだけ使う．
[[/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/fmtbl/R/kumamoto.R][スクリプト]]
**** DONE 鹿児島
***** DONE 漁獲量
****** データ形式
４港月計表（n年度）.xlsxと４港月計表（n-1年度）.xlsxの
「４港計」シートの値を使用する（このシートの値は「阿久根」「枕崎」「山川」「内之浦」の合計値と思われる）．
単位はトンなのでそのまま使用．
****** 集計値チェック
集計値，前年値，平年値OK
***** DONE 予報と漁況
前年（0トン）を上回り，平年（1788.1トン）を下回るという予報．
1〜1430.5トンと予想していることになる．
前年は特異的に低かったから良しとする．
平年比の根拠ついては，恐らく近年3年が現象傾向であること，前期の経過が平年の中でワースト1だったこと，などであろう．
OKとする．
***** DONE 体長
精密測定データは資源評価のときにしか送られてこない．
漁海況では「体長組成（fiscal年度）.xlsx」ファイルを2年分，整理する必要がある．
[[/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/fmtbl/R/kagoshima.R][スクリプト]]
**** CANCELLED 鳥取
集計するのはマイワシのみ
**** CANCELLED 島根
集計するのはマイワシのみ
* 予報案作成
* スライド作成
* TeX版アイデア
* パッケージ化アイデア
** 関数のエコシステム
make_bug   <- function(indir, ystart, yend, prefec=list_prefec)
make_df    <- function(param, prefec="all")
get_blhist <- function(param, year, prefec) {
  df <- make_df(param)
}
get_catch  <- function(param, year, prefec) {
  df <- make_df(param)
}

param <- make_param("datdir", 2018)
param_long <- make_param("datdir", 2011, 2018)


** チェックリスト生成
** 歴代データ出力

#  LocalWords:  yend ystart indir bug df param datdir prefec
