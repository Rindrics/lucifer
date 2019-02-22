#' @export
fmtbl <- function(fname, spcs, nest = FALSE) {
  UseMethod("fmtbl")
}
fname <- "../data.git/漁海況/2018年3月/各県資料/長崎県/2017.09-2018.01小型まき網体長組成.xls"
prefec <- "nagasaki"
tbl_fname <- hash::hash(fname, prefec)
test  <- give_class(fname, tbl_fname)
fname <- test[[1]]

load_alldata <- function(fname, shtname) {
  suppressMessages(
    alldata   <- readxl::read_excel(fname,
                                    sheet = shtname, col_names = FALSE)
  )
}

fmtbl.nagasaki  <- function(fname, spcs, nest = TRUE) {
  shtname <- make_shtname(prefecture = "nagasaki", spcs = spcs)
  alldata <- load_alldata(fname, shtname)
  years           <- (YEAR-1):YEAR
  month_gyokaikyo <- c(3, 10)
  yearmon <- NULL
  for(y in years){
    for(m in month_gyokaikyo){
      yearmon <- append(yearmon, paste0(y, "年", m, "月"))
    }
  }
  dirlist   <- paste0("/Volumes/評価研/漁海況/", yearmon, "/各県資料/", todoufuken)

  monthrow        <- 3
  classstart_col  <- 2
  out             <- NULL
  colname_monyear <- NULL
  for(dir in dirlist){
    blhist <- NULL
    infile <- list.files(dir, pattern="^[^~].+体長組成", full.names = TRUE)
    if (length(infile) > 0) {
      sheets <- excel_sheets(infile)
      sheet  <- sheets[which(gregexpr(species, sheets)>0)]
      data   <- read_xls(infile, sheet=sheet)
      for(i in 1:nrow(data)){
          i_class_start <- as.numeric(as.character(data[i, classstart_col]))
          if(is.na(i_class_start) == FALSE){
              if(i_class_start == BLBREAK_MIN_mm){
                  startrow <- i
                  break
              }
          }
      }
      endrow       <- startrow + length(CLASSNAME)-1
      monthcollist <- which(gregexpr("月", data[monthrow,]) > 0)
      for(j in monthcollist){
          month      <- as.numeric(sub("月", "", data[monthrow, j]))
          ycandidate <- data[monthrow-1, j]
          if (is.na(ycandidate) == FALSE){
            year <- as.numeric(substr(ycandidate, 1, 4))
          }
          blhist <- cbind(blhist, parse_number(parse_character(unlist(data[startrow:endrow, j]))))
          colname_monyear <- append(colname_monyear, paste0(month.abb[month], year))
      }
      out <- cbind(out, blhist)
    }
  }
  out <- as.data.frame(out)

  colnames(out) <- colname_monyear
  rownames(out) <- CLASSNAME
  outfname    <- paste("../", OUTDIR, "/体長組成_", PREFEC_TABLE[[prefecture]], ".csv", sep="")
  out4stac    <- select(out, paste0(month.abb, YEAR-1))[rownames(out) %in% CLASSNAME4Excel,]
  out4gkk     <- select(out, paste0(month.abb[1:8], YEAR))[rownames(out) %in% CLASSNAME4Excel,]
  out4stac[is.na(out4stac)] <- 0
  out4gkk[is.na(out4gkk)]   <- 0
  write.csv(out4gkk, outfname)

  print(out2)
}

fmtbl.kumamoto  <- function(fname, spcs, nest = TRUE) {
  out       <- list()
  shtname   <- make_shtname(prefecture = "kumamoto", spcs = spcs)
  suppressMessages(
    alldata   <- readxl::read_excel(fname,
                                    sheet = shtname, col_names = FALSE)
  )
  cpos_date <- get_col2load(alldata[1,], regex = "[0-9]+", offset = 0)
  cpos_bl   <- cpos_date # Column position of "被鱗体長" is
                         #   same as that of "%Y.%m.%d" column
  date      <- alldata[1, cpos_date] %>%
    tinyplyr::jpdate2julian() %>%
    tinyplyr::num2date()
  method    <- alldata[1, cpos_date + 4] %>%
    unlist() %>%
    as.vector()
  bl        <- purrr::map(cpos_date, get_measdata, df = alldata)
  unlist(bl)

  out$date   <- date
  out$method <- method
  out$year   <- lubridate::year(out$date)
  out$month  <- lubridate::month(out$date)
  out$bl     <- bl

  out <- tibble::as_tibble(out)
  if (nest == FALSE) {
    out <- tidyr::unnest(out)
  }
  out
}
