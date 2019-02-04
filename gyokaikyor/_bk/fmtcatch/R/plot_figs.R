# year <- 2018
library(tidyverse)
library(modelr)
library(readxl)
source("./make_list.R")

# pref_katakuchi <- make_list(fname = "/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/各県いわし_181004_カタクチ.xlsx",
#                            year.end = year, sheet = "カタクチイワシ")
# saveRDS(pref_katakuchi, paste0("./output/各県漁獲量", year, ".rds"))
# seikai_katakuchi <- summarize_seikai(pref_katakuchi, is.gyokaikyo = TRUE)
# dat_prefec <- filter(pref_katakuchi$df$山口)

sum_by_fiscal <- function(df) {
  years <- unlist(df$year)
  data  <- df[,2:13]
  df_fiscal   <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  out         <- list(normal = df, fiscal = df_fiscal)
  for (j in 4:12) {
    df_fiscal[,j-3] <- data[, j]
  }
  for (j in 1:3) {
    df_fiscal[1:(nrow(data)-1), j+9] <- data[2:nrow(data), j]
  }
  df_fiscal <- as.data.frame(df_fiscal)
  df_fiscal$year.fiscal <- years
  df_fiscal$sum.apr_aug <- rowSums(df_fiscal[,1:5])
  df_fiscal$sum.apr_sep <- rowSums(df_fiscal[,1:6])
  df_fiscal$sum.nov_jan <- rowSums(df_fiscal[,8:10])
  df_fiscal$sum.nov_mar <- rowSums(df_fiscal[,8:12])
  df_fiscal$year.fiscal <- years
  colnames(df_fiscal)   <- c(paste0(tolower(month.abb), "_ton"), colnames(df_fiscal)[13:17])
  df_fiscal <- df_fiscal %>%
    select(year.fiscal, jan_ton:sum.nov_mar)
  df_fiscal
  out$fiscal = df_fiscal
  out$normal <- out$normal %>%
    mutate(sum.apr_aug = out$fiscal$sum.apr_aug,
           sum.apr_sep = out$fiscal$sum.apr_sep,
           sum.nov_jan = out$fiscal$sum.nov_jan,
           sum.nov_mar = out$fiscal$sum.nov_mar)
  out$normal[nrow(out$normal), 10:ncol(out$normal)] <- NA
  out$fiscal[nrow(out$fiscal),  6:ncol(out$fiscal)] <- NA
  out
}

# seikai_fiscal <- sum_by_fiscal(seikai_katakuchi)
# seikai_fiscal

# list <- seikai_fiscal
make_summary <- function (list, year) {
  df.normal   <- list$normal
  years       <- df.normal$year
  year_last   <- year-1
  year_recent <- (year-5):year_last
  row_year    <- which(df.normal$year == year)
  row_last    <- which(df.normal$year == year_last)
  row_recent  <- which(df.normal$year %in% year_recent)
  data_year   <- unlist(df.normal[row_year, 2:13])
  data_last   <- unlist(df.normal[row_last, 2:13])
  data_recent <- colSums(df.normal[row_recent, 2:13])/5
  out         <- data.frame(month = 1:12,
                            thisyr = data_year,
                            lastyr = data_last,
                            recentyr = data_recent) %>%
    as.tibble() %>%
    mutate(last.lwr = lastyr * 0.8,
           last.upr = lastyr * 1.2,
           recent.lwr = recentyr * 0.8,
           recent.upr = recentyr * 1.2)
  out
}

# summed <- make_summary(seikai_fiscal, 2018)

make_params <- function(hue.thisyr, hue.lastyr, hue.bg) {
  col.thisyr <- hsv(hue.thisyr/360, 0.8, 0.9)
  col.lastyr <- hsv(hue.lastyr/360, 0.5, 0.9)
  col.recentyr <- hsv(0, 0, 0.6)
  col.bg       <- hsv(hue.bg/360, 0.8, 0.9, 0.2)
  cols         <- c(col.thisyr, col.lastyr, col.recentyr, col.bg)
  params <- list(col = list(thisyr = col.thisyr, lastyr=col.lastyr, recentyr = col.recentyr, bg = col.bg),
                 cex = list(thisyr = 4,
                            lastyr = 3.5),
                 pch = list(thisyr = 21,
                            lastyr = 16),
                 lwd = list(thisyr = 8,
                         lastyr = 6,
                         recentyr = 7),
                 cex.axis = 4,
                 cex.lab = 5,
                 lwd.axis = 3)
  params
}
# params <- make_params(0, 200, 40)

plot_monthly <- function(df, year, params) {
  ymax <- max(df[,-1], na.rm = TRUE)/1000
  yseq <- seq(0, ymax, 1)
  this <- df$thisyr / 1000
  last <- df$lastyr / 1000
  recent <- df$recentyr / 1000
  last.lwr <- df$last.lwr / 1000
  last.upr <- df$last.upr / 1000
  recent.lwr <- df$recent.lwr / 1000
  recent.upr <- df$recent.upr / 1000
  margin     <- 0.3
  plot(1:12, this, type = "n", ylim = c(0, ymax), yaxs = "i", axes = FALSE, ann = FALSE)
  rect(1, 0, 12, ymax, col = rgb(0.9, 0.9, 0.9), border = FALSE)
  rect(4, 0, 8, ymax, col = params$col$bg, border = FALSE, xpd = TRUE)
  axis(1, at = seq(1, 11, 2), labels = FALSE, lwd = params$lwd.axis)
  axis(1, at = seq(2, 12, 2), labels = FALSE, lwd = params$lwd.axis, tcl = -1)
  axis(1, at = seq(1, 11, 2), labels = month.abb[seq(1, 11, 2)], cex.axis = params$cex.axis, pos = -0.1, lwd = 0)
  axis(1, at = seq(2, 12, 2), labels = month.abb[seq(2, 12, 2)], cex.axis = params$cex.axis, pos = -0.2, lwd = 0)
  axis(2, cex.axis = params$cex.axis, lwd = params$lwd.axis)
  for (m in 1:12) {
    abline(v = m, col = "white")
  }
  for (y in yseq) {
    abline(h = y, col = "white")
  }
  polygon(c(1:12, 12:1), c(recent.lwr, rev(recent.upr)), col = rgb(0.5, 0.5, 0.5), border = FALSE)
  lines(1:12, recent, col = rgb(0.8, 0.8, 0.8), lwd = params$lwd$recentyr)

  lines(1:12, last, col = "white", lwd = params$lwd$lastyr + margin)
  lines(1:12, last, col = params$col$lastyr, lwd = params$lwd$lastyr)
  arrows(1:12, last.lwr, 1:12, last.upr, length = 0, col = "white", lwd = params$lwd$lastyr + margin)
  arrows(1:12, last.lwr, 1:12, last.upr, length = 0, col = params$col$lastyr, lwd = params$lwd$lastyr)
  points(1:12, last, col = "white", lwd = params$lwd$lastyr, pch = params$pch$lastyr, cex = params$cex$lastyr + margin)
  points(1:12, last, col = params$col$lastyr, lwd = params$lwd$lastyr, pch = params$pch$lastyr, cex = params$cex$lastyr)

  lines(1:12, this, col = "white", lwd = params$lwd$thisyr + margin)
  lines(1:12, this, col = params$col$thisyr, lwd = params$lwd$thisyr)
  points(1:12, this, col = "white", lwd = params$lwd$thisyr, pch = params$pch$thisyr, bg = "white", cex = params$cex$thisyr + margin)
  points(1:12, this, col = params$col$thisyr, lwd = params$lwd$thisyr, pch = params$pch$thisyr, bg = "white", cex = params$cex$thisyr)
  mtext("漁獲量 (千トン)", 2, cex = params$cex.lab, line = 7)
}

# df <- pref_katakuchi
make_database <- function(df) {
  years     <- df$df[[1]]$year
  prefecs   <- unique(df$prefec)
  level_prefec <- c("山口", "福岡", "佐賀", "長崎", "熊本", "鹿児島")
  year      <- df$df[[1]]$year
  out       <- matrix(NA, nrow = length(years) * length(prefecs) * 12, ncol = 4)
  row_start <- 1
  for (i in seq_along(prefecs)) {
    rows   <- row_start:(row_start + length(years) * 12 - 1)
    prefec <- df$prefec[i]
    i_data <- df$df[[i]] %>%
      dplyr::select(-year, -sum)
    yrs    <- rep(year, 12)
    mons   <- rep(1:12, length(year)); mons <- mons[order(mons)]
    out[rows, 1] <- yrs
    out[rows, 2] <- mons
    out[rows, 3] <- prefec
    out[rows, 4] <- unlist(i_data)
    row_start <- row_start + length(years) * 12
  }
  out  <- as.data.frame(out)
  colnames(out) <- c("year", "month", "prefec", "catch_ton")

  out2 <- out %>%
    as.tibble() %>%
    mutate(year = parse_integer(year),
           month = parse_integer(month),
           prefec = parse_factor(prefec, levels = level_prefec),
           catch_ton= parse_double(catch_ton)) %>%
    arrange(prefec)
  out2
}
# db_katakuchi <- make_database(pref_katakuchi)

# db <- db_katakuchi
make_vec <- function(df, yr, recent = FALSE) {
  if (recent == TRUE) {
    vec <- df %>%
      filter(between(year, yr-5, yr-1)) %>%
      group_by(prefec) %>%
      summarize(mean_zenki = mean(catch_zenki, na.rm = TRUE)) %>%
      select(mean_zenki) %>%
      rename(catch_zenki = mean_zenki) %>%
      as.data.frame() %>%
      unlist()
    vec
  } else {
    vec <- df %>%
      as.data.frame() %>%
      filter(year == yr) %>%
      select(catch_zenki) %>%
      unlist()
    vec
  }
}
# make_vec(dat2use, 2018)
# make_vec(dat2use, 2017)
# make_vec(dat2use, 2018, recent = TRUE)
make_sum <- function(df, thisyr, lastyr, recentyr) {
  out <- list(catch = list(this = sum(make_vec(dat2use, thisyr) / 1000),
                           last = sum(make_vec(dat2use, lastyr) / 1000),
                           recent = sum(make_vec(dat2use, thisyr, recent = TRUE) / 1000)))
  out$ymax <-max(c(out$catch$this) * 1.2, out$catch$last * 1.2, out$catch$recent * 1.2, na.rm = TRUE)
  out
}

plot_recent <- function(x, catch.recent, params) {
  rect(x[1], catch.recent * 0.8, x[5], catch.recent * 1.2, col = params$col$recentyr, border = FALSE)
  lines(x[1:5], rep(catch.recent, 5), col = hsv(0, 0, 0.3), lwd = 8)
}
plot_last <- function(x, catch.last, params) {
  arrows(x[6], catch.last * 0.8, x[6], catch.last * 1.2, length = 0, col = "white", lwd = 10, xpd = TRUE)
  arrows(x[6], catch.last * 0.8, x[6], catch.last * 1.2, length = 0, col = params$col$lastyr, lwd = 8, xpd = TRUE)
  points(x[6], catch.last, pch = 16, col = "white", cex = params$cex$lastyr+0.4, xpd = TRUE)
  points(x[6], catch.last, pch = 16, col = params$col$lastyr, cex = params$cex$lastyr, xpd = TRUE)
}
plot_this <- function(x, catch.this, params) {
  points(x[7], catch.this, pch = 16, col = "white", cex = params$cex$thisyr + 1.4, xpd = TRUE)
  points(x[7], catch.this, pch = 16, col = params$col$thisyr, cex = params$cex$thisyr + 1, xpd = TRUE)
}
plot_trends <- function(x, recent, last, this, params) {
  plot_recent(x, recent, params)
  lines(c(x[6], x[7]), c(last, this), lwd = 5)
  plot_last(x, last, params)
  plot_this(x, this, params)
}
plot_prefec <- function(db, year, params) {
  prefecs <- unique(db$prefec)
  xseq    <- 1:(length(prefecs) * 3 + (length(prefecs)+1) - 1)
  thisyr   <- year
  lastyr   <- year-1
  recentyr <- (year-5):lastyr
  dat2use <- db %>%
    filter(between(year, recentyr[1], thisyr) &
           between(month, 4, 8)) %>%
    group_by(year, prefec) %>%
    summarize(catch_zenki = sum(catch_ton, na.rm = TRUE))
  sums    <- make_sum(dat2use, thisyr, lastyr, recentyr)
  xleft   <- seq(1, length(prefecs)*4, 4)
  barwidth <- 0.7
  plot(xseq, rep(sums$ymax, length(xseq)), xlim = c(0, max(xseq + 5)), ylim = c(0, sums$ymax), type = "n", axes = FALSE, ann = FALSE, yaxs = "i")
  rect(0, 0, max(xseq + 5), sums$ymax, col = hsv(0, 0, 0.9), border = FALSE)
  yseq = seq_range(0:15, 7, pretty = FALSE)
  yseq2 = yseq[(yseq %% 5) == 0]
  for (i in yseq) {
    abline(h = i, col = "white", lwd = 2)
  }
  for (i in seq_along(prefecs)) {
    prefec_name <- prefecs[i]
    prefec_data <- filter(dat2use, prefec == prefec_name)
    catch_recent  <- mean(filter(prefec_data, year %in% recentyr)$catch_zenki/1000)
    catch_last  <- filter(prefec_data, year == lastyr)$catch_zenki/1000
    catch_this  <- filter(prefec_data, year == thisyr)$catch_zenki/1000
    x <- c(seq(1, 2, length.out = 5), 2, 3) + 4 * (i - 1 )
    plot_trends(x, catch_recent, catch_last, catch_this, params)
  }
  x.sum <- c(seq(1, 2, length.out = 5), 2, 3) + 4 * (7 - 1 )
  points(x.sum, c(rep(sums$catch$recent, 5), sums$catch$last, sums$catch$this))
  plot_trends(x.sum, sums$catch$recent, sums$catch$last, sums$catch$this, params)
  # rect(0, 0, max(xseq+5), ymax, col = hsv(0, 0, 0.9), border = FALSE)
  # rect(xleft-0.2, 0, xleft + barwidth + 1.2, catch_recent, col = hsv(0, 0, 0.7), border = FALSE)
  # rect(xleft + 1.2, 0, xleft + 1 + barwidth, catch_last, col = params$col$lastyr, border = FALSE)
  # rect(xleft + 2, 0, xleft + 2 + barwidth, catch_this, col = params$col$thisyr, border = FALSE)
  # rect(26-0.2, 0, 26 + barwidth + 1.2, sum(catch_recent), col = hsv(0, 0, 0.7), border = FALSE)
  # rect(26 + 1.2, 0, 26 + barwidth + 1, sum(catch_last), col = params$col$lastyr, border = FALSE)
  # rect(26 + 2, 0, 26  + 2 + barwidth, sum(catch_this), col = params$col$thisyr, border = FALSE)
  axis(1, at = xleft+1, labels = prefecs, lwd = 0, cex.axis = params$cex.axis - 0.5, pos = -0.5)
  axis(1, at = 27, labels = "計", lwd = 0, cex.axis = params$cex.axis, pos = -0.5)
  axis(2, at = yseq, labels = FALSE, lwd = params$lwd.axis)
  axis(2, at = yseq2, cex.axis = params$cex.axis, lwd = params$lwd.axis)
  mtext("漁獲量（千トン）", 2, cex = params$cex.lab, line = 7)
}

myfont <- "HiraKakuProN-W3"
png("../../figs/prefec.png", width = 1500, height = 1100)
par(mai = c(3, 3, 0.5, 0.5), family = myfont)
plot_prefec(db_katakuchi, 2018, params)
dev.off()

#png("../../figs/monthly.png", width = 1500, height = 1100)
#par(mai = c(3, 3, 0.5, 0.5), family = myfont)
#plot_monthly(summed, 2018, params)
#dev.off()
