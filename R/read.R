#' Reads local *.csv file
#'
#' @param file ...
#' @param delim ...
#' @param locale ...
#' @param ... ...
#'
#' @keywords internal
read_local_csv <- function(file, delim = ";", locale = readr::locale(encoding = "latin1", tz = "Etc/GMT-1"), ...){

  data <- readr::read_delim(file, delim = delim, locale = locale, ...)

  return(data)
}

