source("./_packages.R")
source("./_constants.R")
source("./hoshifile2figs.R")

blbin <- 5
class <- list(left  = seq(0, 295, blbin),
              right = CLASS_LEFT + blbin,
              name  = make_classname(CLASS_LEFT, blbin, width = 3))
border <- list(kaeri_koba = 50,
              koba_chuba = 80,
              chuba_ohba = 100)
param_bl <- list(year = 2018,
                 months = 1:12,
                 myfont = "HiraKakuProN-W3",
                 meigara = c("kaeri","koba","chuba","ohba"),
                 border = border,
                 blrange = list(kaeri = c(0, border$kaeri_koba),
                                koba = c(border$kaeri_koba, border$koba_chuba),
                                chuba = c(border$koba_chuba, border$chuba_ohba),
                                ohba = c(border$chuba_ohba, 295)),
                 blbin = blbin,
                 class = class,
                 barwidth = 4,
                 xmax = 150,
                 ymax = list(init = 100,
                             updated = 0),
                 age = c(0, 1, 2),
                 col_bg = list(kaeri = hsv( 60/360, 0.9, 0.9, 0.1),
                                koba = hsv( 60/360, 0.9, 0.9, 0.35),
                                chuba = hsv( 60/360, 0.9, 0.9, 0.65),
                                ohba = hsv( 60/360, 0.9, 0.9, 1)),
                 col_bar = list(age0 = hsv(200/360, 0, 0),
                               age1 = hsv(200/360, 0, 0),
                               age2 = hsv(200/360, 0, 0)),
                 tickbin = list(x = 1,
                                y = 10),
                 ticklen = list(x = 0.25,
                                y = 0.25),
                 labelbin = list(x = 5,
                                 y = 20),
                 lwd_axis = 3,
                 cex_axis = 2.2,
                 cex_label = 2.5)

png("../../figs/blhist.png", width=1800, height=1100)
plot_blhist(param_bl, month.max = 12)
dev.off()
png("../../figs/blhist_jan2aug.png", family="Helvetica", width=1800, height=1100)
plot_blhist(param_bl, 8)
dev.off()
png("../../figs/blhist_apr2mar.png", family="Helvetica", width=1800, height=1100)
plot_blhist_apr(param_bl)
dev.off()
