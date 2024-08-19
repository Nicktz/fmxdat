#' @title YM
#' @description Creates a Year-Month column
#' @return Adds column to existing df
#' @examples
#' dta <- fmxdat::BRICSTRI %>% rename(date = Date) %>% fmxdat::YM()
#' @export
#'
YM <- function(df) {

  if(!"date" %in% colnames(df)) stop("...must have a column called 'date'")

  df %>% mutate(YM = format(date, "%Y%B"))


}
