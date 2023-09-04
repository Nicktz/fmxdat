#' @title Date_Roller
#' @description Creates a list of rolling date entries to be used easily using a map call.
#' It will start at first date, look ahead N periods and create the first list entry.
#' Thereafter it drops first entry, and adds next. This keeps a constant N length rolling window.
#' See my question on stack here: https://stackoverflow.com/q/54733427/4198868
#' @return Vector of dates to be used in map.
#' @param DateVec Any arranged vector of dates. Function will sort your date vector just in case you forget...
#' @param N Rolling N periods.
#' @importFrom dplyr arrange
#' @importFrom zoo as.Date
#' @importFrom zoo rollapply
#' @examples
#' library(tidyverse);library(lubridate);library(rmsfuns)
#' DateVec <- rmsfuns::dateconverter(lubridate::ymd(20020101), lubridate::ymd(20200131), Transform = "weekdayEOM")
#' Rolldates <- Date_Roller(DateVec, N = 24)
#' @export
#'
Date_Roller <- function(DateVec, N = 24){
  if(!class(DateVec) %in% c("Date")) stop("Please provide a valid Date vector.")
  r <- zoo::as.Date( zoo::rollapply(sort(DateVec), N, c) ) ; L <- lapply(1:nrow(r), function(i) r[i, ])
  L
}
