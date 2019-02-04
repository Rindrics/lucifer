library(readxl)
library(tidyverse)

make_classname <- function(left, bin, width) {
  right <- left + bin
  name  <- paste0(formatC(left, width = width, flag = 0),
                  "-",
                  formatC(right, width = width, flag = 0))
  name
}


class00_10_age0   <- data.frame(matrix(1, nrow=2, ncol=12),
                                row.names=c("000-005", "005-010"))
class00_10_age12  <- data.frame(matrix(0, nrow=2, ncol=12),
                                row.names=c("000-005", "005-010"))
class155_300      <- data.frame(matrix(0, nrow=length(seq(155, 295, 5)), ncol=12),
                                row.names=paste(seq(155, 295, 5),
                                                  seq(160, 300, 5), sep = "_"))
colnames(class00_10_age0)   <- month.abb
colnames(class00_10_age12)  <- month.abb
colnames(class155_300)      <- month.abb
alkey0  <- read.csv("./ageLengthKey0.csv", row.names=1)
alkey0  <- rbind(class00_10_age0, alkey0, class155_300)
alkey1  <- read.csv("./ageLengthKey1.csv", row.names=1)
alkey1  <- rbind(class00_10_age12, alkey1, class155_300)
alkey2  <- read.csv("./ageLengthKey2.csv", row.names=1)
alkey2  <- rbind(class00_10_age12, alkey2, class155_300)
startrow <- 6 # start from 0-5 mm but it is ok
endrow   <- startrow + length(CLASS_LEFT) - 1

# data_org <- read_xlsx("/Users/ahayashi/Documents/GitHub/gyokaikyo/hayashi/★カタクチ18.xlsx",
#                       sheet = "計", range = "A10:N70")
# df <- data_org
load_hoshifile <- function(year) {
  fname       <- paste0("../../★カタクチ", str_sub(year, 3, 4), ".xlsx")
  df          <- read_xlsx(fname, sheet = "計", range = "A10:N70", col_types = "numeric")
  CLASS_LEFT  <- unlist(df[,1])
  class_right <- unlist(df[,1] + 5)
  row_names   <- paste0(formatC(CLASS_LEFT, width=3, flag=0), "-", formatC(class_right, width=3, flag=0))
  data <- df[,3:ncol(df)] %>%
    rename(jan = `1`,
           feb = `2`,
           mar = `3`,
           apr = `4`,
           may = `5`,
           jun = `6`,
           jul = `7`,
           aug = `8`,
           sep = `9`,
           oct = `10`,
           nov = `11`,
           dec = `12`)
  for(j in seq_along(data)) {
    data[,j] <- parse_integer(unlist(data[,j]))
  }
  data <- as.data.frame(data)
  rownames(data) <- row_names
  as.tibble(data)
}

make_bl_database <- function(param) {
  years <- (param$year - 2):param$year
  out   <- matrix(NA, nrow = length(years) * length(param$class$left) * 12, ncol = 4)
  row_start <- 1
  for(y in years){
    df <- load_hoshifile(y)
    for (m in seq_along(param$months)) {
      rows   <- row_start:(row_start + length(param$class$left) - 1)
      m_data <- unlist(df[, m])
      yrs    <- rep(y, length(param$class$left))
      mons   <- rep(m, length(param$class$left))
      out[rows, 1] <- yrs
      out[rows, 2] <- mons
      out[rows, 3] <- param$class$left
      out[rows, 4] <- m_data
      row_start <- row_start + length(param$class$left)
    }
  }
  out  <- as.data.frame(out)
  colnames(out) <- c("year", "month", "left", "count")

  out2 <- out %>%
    as.tibble() %>%
    mutate(year = parse_integer(year),
           month = parse_integer(month),
           left = parse_character(left),
           count= parse_integer(count))
  out2
}


plot_agebar <- function(param_univ, param_local, age) {
  rect(param_univ$class$left,
       unlist(param_local$bottom[param_local$age == age]),
       param_univ$class$left + unlist(param_univ$barwidth),
       unlist(param_local$top[param_local$age == age]),
       col = as.character(param_univ$col_bar[param_univ$age == age]),
       border = FALSE)
}
plot_bg <- function(param_univ, param_local, meigara) {
  polygon(c(unlist(param_univ$blrange[param_univ$meigara == meigara]),
            rev(unlist(param_univ$blrange[param_univ$meigara == meigara]))),
          c(0, 0, param_local$ymax, param_local$ymax),
          col = as.character(param_univ$col_bg[param_univ$meigara == meigara]),
          border = FALSE)
}
plot_blhist <- function(param, month.max = 12) {
  par(mfcol=c(12, 3), mai=c(0.1, 0.8, 0.1, 0.1), ps=20, oma=c(11, 10, 5, 14), family = param$myfont)
  years <- (param$year - 2):param$year
  db    <- make_bl_database(param)
  param_blhist <- list(age = param$age)
  for(y in years){
    for(m in 1:12) {
      if (m > month.max) {
        plot(1, 1, axes = FALSE, ann = FALSE, type = "n")
      } else {
        mdata <- db %>%
          dplyr::filter(year == y & month == m) %>%
          select(count) %>%
          unlist()
        if(sum(mdata)!=0){
          mdata_pcnt  <- mdata / sum(mdata) * 100
          max_pcnt    <- max(mdata_pcnt, na.rm = TRUE)
          if(max_pcnt > param$ymax$updated){
            param$ymax$updated <- max_pcnt
          }
        }
        ymax      <- param$ymax$init
        ymax_int  <- ceiling(max(mdata)/100) * 100
        if(ymax_int > ymax){
          ymax <- ymax_int
        }
        tickbin_y   <- ymax/2
        labelbin_y  <- ymax/1
        m_alkey0  <- alkey0[, m]
        m_alkey1  <- alkey1[, m]
        m_alkey2  <- alkey2[, m]
        bottom0   <- rep(0, length(m_alkey0))
        top0      <- m_alkey0 * mdata
        bottom1   <- top0
        top1      <- (bottom1 + m_alkey1 * mdata)
        bottom2   <- top1
        top2      <- (bottom2 + m_alkey2 * mdata)
        param_blhist$top <- list(age0 = top0,
                                 age1 = top1,
                                 age2 = top2)
        param_blhist$bottom <- list(age0 = bottom0,
                                    age1 = bottom1,
                                    age2 = bottom2)
        param_blhist$ymax   <- ymax
        plot(1, 1, xlim=c(0, param$xmax), ylim=c(0, ymax),
             type="n", xaxs="i", yaxs="i", axes=FALSE, ann=FALSE)
        if (m == 1) {
          mtext(paste0(y, "年"), side = 3, cex = param$cex_label)
        }
        plot_bg(param, param_blhist, "kaeri")
        plot_bg(param, param_blhist, "koba")
        plot_bg(param, param_blhist, "chuba")
        plot_bg(param, param_blhist, "ohba")
        plot_agebar(param, param_blhist, 0)
        plot_agebar(param, param_blhist, 1)
        plot_agebar(param, param_blhist, 2)
        axis(1, at=seq(0, param$xmax, param$tickbin$x * 10), labels=F, tcl=param$ticklen$x)
        axis(1, at=seq(0, param$xmax * 10, param$labelbin$x * 10), labels=FALSE, tcl=-0.3, lwd = param$lwd_axis)
        axis(2, at=seq(0, ymax), tcl=0, labels=FALSE)
        axis(2, at=seq(0, ymax, tickbin_y), labels=FALSE, tcl=param$ticklen$y, lwd = param$lwd_axis)
        axis(2, at=seq(0, ymax, labelbin_y), labels=FALSE, tcl=-0.3, lwd = param$lwd_axis)
        axis(2, at=ymax*0.1, labels=0, las=2, pos=1, lwd = 0, cex.axis = param$cex_axis)
        axis(2, at=ymax*0.9, labels=ymax, las=2, pos=1, lwd = 0, cex.axis = param$cex_axis)
        if (y == param$year) {
          mtext(month.abb[m], side = 4, las = 2, cex = param$cex_label, line = 3)
        }
        if (m == month.max & y == years[2]) {
          axis(1, at = seq(0, 150, 50), labels = seq(0, 15, 5), cex.axis = param$cex_axis, lwd = 0, pos = -ymax * 0.2)
          mtext("被鱗体長（cm）", side = 1, cex = param$cex_label+1, line = 10)
        }
      }
    }
  }
  mtext("標本数", side = 2, cex = param$cex_label+1, outer = TRUE, line = 3)
}

plot_blhist_apr <- function(param) {
  par(mfcol=c(12, 3), mai=c(0.1, 0.8, 0.1, 0.1), ps=20, oma=c(11, 10, 5, 14), family = param$myfont)
  years        <- (param$year - 2):param$year
  db           <- make_bl_database(param)
  param_blhist <- list(age = param$age)
  months       <- c(4:12, 1:3)
  for(fiscalyr in years){
    for(m in months) {
      ifelse(between(m, 4, 12),
             yearkey <- fiscalyr,
             yearkey <- fiscalyr + 1)
      mdata <- db %>%
        dplyr::filter(year == yearkey & month == m) %>%
        select(count) %>%
        unlist()
      if(sum(mdata)!=0){
        mdata_pcnt  <- mdata / sum(mdata) * 100
        max_pcnt    <- max(mdata_pcnt, na.rm = TRUE)
        if (max_pcnt > param$ymax$updated) {
          param$ymax$updated <- max_pcnt
        }
      } else {
        break
      }
      ymax      <- param$ymax$init
      ymax_int  <- ceiling(max(mdata)/100) * 100
      if(ymax_int > ymax){
        ymax <- ymax_int
      }
      tickbin_y   <- ymax/2
      labelbin_y  <- ymax/1
      m_alkey0  <- alkey0[, m]
      m_alkey1  <- alkey1[, m]
      m_alkey2  <- alkey2[, m]
      bottom0   <- rep(0, length(m_alkey0))
      top0      <- m_alkey0 * mdata
      bottom1   <- top0
      top1      <- (bottom1 + m_alkey1 * mdata)
      bottom2   <- top1
      top2      <- (bottom2 + m_alkey2 * mdata)
      param_blhist$top <- list(age0 = top0,
                               age1 = top1,
                               age2 = top2)
      param_blhist$bottom <- list(age0 = bottom0,
                                  age1 = bottom1,
                                  age2 = bottom2)
      param_blhist$ymax   <- ymax
      plot(1, 1, xlim=c(0, param$xmax), ylim=c(0, ymax),
           type="n", xaxs="i", yaxs="i", axes=FALSE, ann=FALSE)
      if (m == 4) {
        mtext(paste0(yearkey, "年度"), side = 3, cex = param$cex_label)
      }
      plot_bg(param, param_blhist, "kaeri")
      plot_bg(param, param_blhist, "koba")
      plot_bg(param, param_blhist, "chuba")
      plot_bg(param, param_blhist, "ohba")
      plot_agebar(param, param_blhist, 0)
      plot_agebar(param, param_blhist, 1)
      plot_agebar(param, param_blhist, 2)
      axis(1, at=seq(0, param$xmax, param$tickbin$x * 10), labels=F, tcl=param$ticklen$x)
      axis(1, at=seq(0, param$xmax * 10, param$labelbin$x * 10), labels=FALSE, tcl=-0.3, lwd = param$lwd_axis)
      axis(2, at=seq(0, ymax), tcl=0, labels=FALSE)
      axis(2, at=seq(0, ymax, tickbin_y), labels=FALSE, tcl=param$ticklen$y, lwd = param$lwd_axis)
      axis(2, at=seq(0, ymax, labelbin_y), labels=FALSE, tcl=-0.3, lwd = param$lwd_axis)
      axis(2, at=ymax*0.1, labels=0, las=2, pos=1, lwd = 0, cex.axis = param$cex_axis)
      axis(2, at=ymax*0.9, labels=ymax, las=2, pos=1, lwd = 0, cex.axis = param$cex_axis)
      if (fiscalyr == param$year) {
        mtext(month.abb[m], side = 4, las = 2, cex = param$cex_label, line = 3)
      } else if (fiscalyr == param$year-1 & m %in% c(10:12, 1:3)) {
        mtext(month.abb[m], side = 4, las = 2, cex = param$cex_label, line = 3)
      }
    }
    for (m in 1:3) {
    }
    axis(1, at = seq(0, 150, 50), labels = seq(0, 15, 5), cex.axis = param$cex_axis, lwd = 0, pos = -ymax * 0.2)
  }
  mtext("標本数", side = 2, cex = param$cex_label+1, outer = TRUE, line = 3)
  mtext("被鱗体長（cm）", side = 1, cex = param$cex_label+1, outer = TRUE, line = 9)
}
