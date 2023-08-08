#' @title plot_order_set
#' @description Sets order for a ggplot using a column and a vector
#' @return Orders ggplot
#' @param df tidy data frame to be ordered
#' @param Column which column to be ordered.
#' @param Order Vector to order by.
#' @examples
#' library(tidyverse);library(lubridate)
#' dfplot <- fmxdat::SectorTRIs %>% gather(type, val, -Date)
#' ord <- rev(unique(dfplot$type))
#' dfplot %>% ggplot() + geom_line(aes(Date, val, color = type))
#' # Reordered now:
#' dfplot %>% plot_order_set( Column = "type", Order = ord) %>% ggplot() + geom_line(aes(Date, val, color = type))
#' @export
#'
plot_order_set <- function(df, Column, Order) {

  df[,Column][[1]] <- factor(df[,Column][[1]], levels = Order)

  df

}
