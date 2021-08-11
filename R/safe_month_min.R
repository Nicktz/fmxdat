#' @title safe_month_min
#' @description Safely goes back N months for monthly data.
#' This is an improved version of using filter(date >= last(date) %-% months(6)) - c.f. below:
#' library(tidyverse);library(lubridate)
#' df <- data.frame(date = rmsfuns::dateconverter(StartDate = lubridate::ymd(20200831), EndDate = lubridate::ymd(20210228), Transform = 'calendarEOM'))
#' df %>% filter(date > last(date) %m-% months(6))
#' Note that this gives 7 months, not 6.
#' Instead, use:
#' df %>% filter(date >= safe_month_min(last(date), N = 6))
#' @return N months back
#' @param Ra date, specified in dataframe as e.g. last(date)
#' @param Ra N months back
#' @examples
#' library(tidyverse);library(lubridate)
#' df <- data.frame(date = rmsfuns::dateconverter(StartDate = lubridate::ymd(20200131), EndDate = lubridate::ymd(20210228), Transform = 'calendarEOM'))
#' df %>% filter(date >= safe_month_min(last(date), N = 6))
#' @export
#'
safe_month_min <- function(datesel, N = 6){
  floor_date(datesel %m-% months(N-1), unit = "months")
}
