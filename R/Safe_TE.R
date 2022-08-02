#' @title Safe_TE
#' @description Safely calculated TE.
#' Problem with PerformanceAnalytics - if the dates aren't exactly aligned, the calculation is wrong.
#' See ?Safe_TE for an example of date misallignment, its impact on TE and how the corrected version works.
#' Will auto correct if there is one entry per month.
#' @param Ra Returns series (xts). If not xts, will make it xts.
#' @param Rb BM series (xts). If not xts, will make it xts.
#' @examples
#'
#'# Problem with PerformanceAnalytics - if the dates aren't exactly aligned, the calculation is wrong.
#'# An example of this is:
#'
#'library(tidyverse); library(fmxdat);library(lubridate);library(xts)
#'# Load capped swix as bm
#'alsi <- fmxdat::Jalshtr %>% mutate(YM = format(date, "%Y%B")) %>% group_by(YM) %>% filter(date == last(date)) %>% ungroup() %>% mutate(Returns = TRI / lag(TRI)-1) %>%filter(!is.na(Returns)) %>% filter(date > ymd(20100131))
#'funds <- fmxdat::asisa %>% filter(Funds %in% c("Fund_1", "Fund_10")) %>% mutate(YM = format(date, "%Y%B")) %>% filter(date > ymd(20100131))
#'Endate <- ymd(20210430)
#'
#'# For the past three years, the dates align:
#'Ra <- alsi %>% filter(date <= Endate) %>% filter(date >= RA::safe_month_min(last(date), N = 36)) %>% tbl2xts::tbl_xts(cols_to_xts = Returns)
#'Rb <- funds %>% filter(date <= Endate)   %>% filter(date >= fmxdat::safe_month_min(last(date), N = 36)) %>% tbl2xts::tbl_xts(cols_to_xts = Returns, spread_by = Funds)
#'PerformanceAnalytics::TrackingError(Ra, Rb, scale = 12)
#'fmxdat::Safe_TE(Ra, Rb, scale = 12)
#'
#'# But they have different dates for certain months (e.g. see: index(Ra)[!index(Ra) %in% index(Rb)])
# Now we want this to throw an error for safety - as PerformanceAnalytics::TrackingError bizarrely simply ignores those months!!
#'Rb <- funds %>% tbl2xts::tbl_xts(cols_to_xts = Returns, spread_by = Funds) %>% filter(date <= Endate)
#'Ra <- alsi %>% tbl2xts::tbl_xts(cols_to_xts = Returns) %>% filter(date <= Endate)
#'PerformanceAnalytics::TrackingError(Ra, Rb, scale = 12)
#'fmxdat::Safe_TE(Ra, Rb, scale = 12)
#'
#'
#'
#' @export
#'
Safe_TE <- function(Ra, Rb, scale = 12) {

  if( !any(grepl("xts", class(Ra)) ) ) Ra <- tbl_xts(Ra)
  if( !any(grepl("xts", class(Rb)) ) ) Rb <- tbl_xts(Rb)

  if( any( grepl( paste(names(Ra), collapse="|"), names(Rb) ) ) ) {
    # Naming causes issues:
    names(Rb) <- "SomeRandomNameNotToBeConfused"
  }

  if(length(index(Ra)) != length(index(Rb))) stop("Dates not aligning for Ra and Rb. Please check this.")

  test <-
    bind_cols(tibble( date1 = index(Ra)),
              tibble( date2 = index(Rb) )
    ) %>% mutate(Diffcheck = abs(date1-date2)) %>% filter(Diffcheck > 0)


  if(nrow(test) > 0 ) {

    xdf <- Ra %>% xts_tbl()
    seriesnames <- names(xdf %>% select(-date))
    ydf <- Rb %>% xts_tbl() %>% purrr::set_names(c("date", "BM"))


    if( xdf %>% RA::YM() %>% group_by(YM) %>% filter(row_number() > 1) %>% nrow() == 0  && ydf %>% RA::YM() %>% group_by(YM) %>% filter(row_number() > 1) %>% nrow() == 0 ) {

      warning("Dates not aligning - this will produce wrong TE resutls.\nRearranged to have months alligned instead (assuming monthly returns data)...")

      transformed <-
        left_join(
          xdf %>% RA::YM(),
          ydf %>% RA::YM() %>% select(-date), by = "YM")

      xtr <- transformed %>% select(date, all_of(seriesnames)) %>% tbl_xts()
      ytr <- transformed %>% select(date, "BM") %>% tbl_xts()

      TE <- PerformanceAnalytics::TrackingError(xtr, ytr, scale)

    }

  } else {

    TE <- PerformanceAnalytics::TrackingError(Ra, Rb, scale)

  }

  TE

}
