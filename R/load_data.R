#' Load data from all range of Excel sheet as character
#'
#' @inheritParams readxl::read_excel
#' @export
load_alldata <- function(path, sheet) {
  suppressMessages(
    alldata   <- readxl::read_excel(path,
                                    sheet = sheet, col_names = FALSE,
                                    col_types = "text")
  )
}
