#' @title fmx_fills
#' @description Plotting colors
#' @return manual color theme
#' @examples
#' g + fmx_fills() + ...
#' @export
#'
fmx_fills <- function() {

  Pastelfills <- c("#AD3636", "chocolate3", "mediumseagreen", "dodgerblue4", "cornflowerblue", "darkslategray",
                   "darkolivegreen2", "darkorchid4", "darkgoldenrod4", "gray45", "darkolivegreen", "gray3", "yellow2")


  scale_fill_manual(values = Pastelfills)


}
