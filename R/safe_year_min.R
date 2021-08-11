#' @title safe_year_min
#' @description Safely goes back N years for monthly data.
#' This is an improved version of using filter(date >= last(date) %-% months(6)) - c.f. below:
#' library(tidyverse);library(lubridate)
#' df <- data.frame(date = rmsfuns::dateconverter(StartDate = lubridate::ymd(20180831), EndDate = lubridate::ymd(20210228), Transform = 'calendarEOM'))
#' df %>% filter(date > last(date) %m-% years(2))
#' Note that this gives 25 months, not 24.
#' Instead, use:
#' df %>% filter(date >= safe_year_min(last(date), N = 2))
#' @return N months back
#' @param Ra date, specified in dataframe as e.g. last(date)
#' @param Ra N months back
#' @examples
#' library(tidyverse);library(lubridate)
#' df <- data.frame(date = rmsfuns::dateconverter(StartDate = lubridate::ymd(20180831), EndDate = lubridate::ymd(20210531), Transform = 'calendarEOM'))
#' df %>% filter(date >= safe_year_min(last(date), N = 2))
#' @export
#'
safe_year_min <- function(datesel, N = 2){
  YMSel = format(datesel, "%Y%B")
  data.frame(date = rmsfuns::dateconverter(StartDate = ymd(19900101), EndDate = ymd(20500101), Transform = 'calendarEOM')) %>%
    mutate(YM = format(date, "%Y%B")) %>% mutate(cons = ifelse(YM == YMSel, "Keep", NA)) %>% tidyr::fill(cons, .direction = "up") %>%
    filter(cons == "Keep") %>%
    tail(N*12 + 1) %>% slice(1) %>% pull(date) + 1
}
