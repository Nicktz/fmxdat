#' @title fmx_cols
#' @description Plotting colors
#' @return manual color theme
#' @examples
#' g + fmx_cols() + ...
#' @export
#'
fmx_cols <- function() {

  PastelCols <- c("dodgerblue4", "chocolate3", "springgreen3", "#AD3636", "cornflowerblue", "darkslategray",
                  "darkorange", "darkorchid4", "darkgoldenrod4", "gray45", "darkolivegreen", "gray3", "yellow2")

  scale_color_manual(values = PastelCols)


}
