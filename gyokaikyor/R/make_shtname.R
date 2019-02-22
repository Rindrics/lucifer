make_shtname <- function(prefecture, spcs) {
  switch(prefecture,
         "kumamoto" = {
           switch(spcs,
                  "katakuchi" = shtname <- "カタクチ",
                  "urume"     = shtname <- "ウルメ",
                  "maiwashi"  = shtname <- "マイワシ",
                  "sabarui"   = shtname <- "サバ類",
                  stop("Unknown spcs name"))

         },
         "nagasaki" = {
           switch(spcs,
                  "katakuchi" = shtname <- "カタクチ",
                  "urume"     = shtname <- "ウルメ",
                  "maiwashi"  = shtname <- "マイワシ",
                  "masaba"    = shtname <- "マサバ",
                  "gomasaba"  = shtname <- "ゴマサバ",
                  "maaji"     = shtname <- "マアジ",
                  stop("Unknown spcs name"))
         },
         stop("Unknown prefecture")
         )
  shtname
}
