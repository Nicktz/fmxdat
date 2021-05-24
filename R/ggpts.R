#' @title ggpts
#' @description Makes text size changes much easier in ggplot
#' @return changes text size in plot
#' @param x size
#' @examples
#' g <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
#' annotate(geom="label", x = 3.5, y = 22.5, label = "Some Text", size = ggpts(12))
#' @export
#'

ggpts <- function(x) {

  as.numeric(grid::convertX(grid::unit(x, "points"), "mm"))

}
