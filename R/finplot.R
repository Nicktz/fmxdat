
#' @title finplot
#' @description Plotting aide for Psec plotting functionalities.
#' @return ggplot object
#' @param x.comma.sep Make x axis not show scientific values (e.g. shows 25,000,000 as opposed to 2.5e+6)
#' @param y.comma.sep Make y axis not show scientific values (e.g. shows 25,000,000 as opposed to 2.5e+6)
#' @param x.pct Make x axis percentages
#' @param x.pct_acc Rounding of pct value.
#' @param y.pct Make y axis percentages
#' @param y.pct_acc Rounding of pct value.
#' @param x.vert flip x axis
#' @param y.vert flip y axis
#' @param x.ticks.rm Remove x ticks altogether
#' @param y.ticks.rm Remove x ticks altogether
#' @param legend_pos where to position legend. Defaults to bottom.
#' @param col.hue set color hues. E.g., set hue = 40 to see what happens
#' @param fill.hue set fill hues. E.g., set hue = 40 to see what happens
#' @param title.center Put title in center of figure. Used with grids or facet wraps e.g.
#' @param darkcol Use dark colors
#' @param x.date.type Of e.g. form '\%Y', '\%y_\%b'
#' @param x.date.dist Space between date points on x axis. E.g., "2 years", "10 week"
#' @param x.date.vector.sel Provide a vector of possible dates - this way weekends and holidays are e.g. removed... use bdscale here. E.g. use: x.date.vector.sel = unique(df$date)
#' @param legend.line.size Make legend lines larger
#' @param legend.line.alpha Make legend lines lighter
#' @param legend.text.size Adj legend text
#' @param log.y Apply a log transformation on the y-axis. This is a log base 10 transformation
#' @examples
#' g <- ggplot() + geom_line(aes(date, returns, color = Funds)) + theme_fmx()
#' finplot(g, x.pct = TRUE)
#' @export
#'

finplot <- function(g,
                    x.comma.sep = FALSE,
                    y.comma.sep = FALSE,
                    x.pct = FALSE,
                    x.pct_acc = 0.1,
                    y.pct = FALSE,
                    y.pct_acc = 0.1,
                    zero.anchor = FALSE,
                    x.vert = FALSE,
                    y.vert = FALSE,
                    x.ticks.rm = FALSE,
                    y.ticks.rm = FALSE,
                    legend_pos = NULL,
                    col.hue = NULL,
                    fill.hue = NULL,
                    title.centre = FALSE,
                    darkcol = FALSE,
                    x.date.type = NULL,
                    x.date.dist = NULL,
                    x.date.vector.sel = NULL,
                    legend.line.size = NULL,
                    legend.text.size = NULL,
                    legend.line.alpha = NULL,
                    legend.title = NULL,
                    log.y = FALSE,
                    drop.grids = FALSE,
                    drop.x.grids = FALSE,
                    drop.y.grids = FALSE){


  if( x.comma.sep ) {
    g <-
      g + scale_x_continuous(labels = scales::comma)
  }

  if( y.comma.sep ) {
    g <-
      g + scale_y_continuous(labels = scales::comma)
  }


  if( x.pct ) {
    g <-
      g + scale_x_continuous(labels = scales::percent_format(accuracy = x.pct_acc))
  }

  if( y.pct ) {
    g <-
      g + scale_y_continuous(labels = scales::percent_format(accuracy = y.pct_acc))
  }


  if( zero.anchor ) {
    g <-
      g + expand_limits(x = 0, y = 0)
  }


  if(is.null(x.date.type) & !is.null(x.date.vector.sel) ) {

    g <-
      g +
      bdscale::scale_x_bd(business.dates = x.date.vector.sel)

  }

  if(!is.null(x.date.type) & !is.null(x.date.vector.sel) ) {

    g <-
      g +
      bdscale::scale_x_bd(business.dates = x.date.vector.sel, labels=scales::date_format(x.date.type))

  }


  if(is.null(x.date.vector.sel) & is.null(x.date.type) & !is.null(x.date.dist) ) {

    g <-
      g +
      scale_x_date(date_breaks = x.date.dist)

  }

  if(!is.null(x.date.type) & is.null(x.date.vector.sel) & !is.null(x.date.dist) ) {

    g <-
      g +
      scale_x_date(labels=scales::date_format(x.date.type), date_breaks = x.date.dist)

  }

  if( !is.null(col.hue)){

    g <-
      g + scale_color_hue(l = col.hue)
  }

  if( !is.null(fill.hue)){

    g <-
      g + scale_fill_hue(l = fill.hue)
  }

  if( x.vert & x.ticks.rm ) stop("Cannot have x.vert and x.ticks.rm... please choose")
  if( y.vert & y.ticks.rm ) stop("Cannot have x.vert and x.ticks.rm... please choose")


  if(x.vert) {
    g <-
      g + theme(axis.text.x=element_text(angle = 90, hjust = 1))
  }


  if(x.ticks.rm) {
    g <-
      g + theme(axis.text.x=element_blank())
  }

  if(y.vert) {
    g <-
      g + theme(axis.text.y=element_text(angle = 90, hjust = 1))

  }

  if(y.ticks.rm) {
    g <-
      g + theme(axis.text.y=element_blank())

  }


  if ( darkcol ) {

    g <-
      g +
      scale_color_brewer(palette="Dark2")

  }

  if(!is.null(legend_pos)){
    g <-
      g + theme(legend.position = legend_pos)

  }

  if(title.centre){
    g <- g +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5) )

  }

  if( !is.null( legend.line.size )) {
    g <-
      g + guides(colour = guide_legend(override.aes = list(size = legend.line.size)))

  }

  if( !is.null( legend.text.size )) {
    g <-
      g +
      ggplot2::theme(legend.text = ggplot2::element_text(size = legend.text.size) )

  }


  if( !is.null( legend.line.alpha )) {
    g <-
      g + guides(colour = guide_legend(override.aes = list(alpha=legend.line.alpha)))

  }

  if( !is.null( legend.title  )) {
    g <-
      g + ggplot2::theme(legend.title = ggplot2::element_text(size = legend.title) )

  } else {

    g <-
      g + ggplot2::theme(legend.title = ggplot2::element_blank() )

  }


  if( log.y ) {

    g <-
      g +
      coord_trans(y = 'log10') +
      # annotation_logticks(scaled=FALSE) +
      scale_y_continuous(breaks = scales::trans_breaks("log10", function(x) 10^x),
                         labels = scales::trans_format("log10", scales::math_format(10^.x)))

  }




  if( drop.grids ){

    g <-
      g + ggplot2::theme(panel.grid = element_blank())
  }

  if( drop.x.grids ){
    g <-
      g +
      ggplot2::theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank()
      )
  }

  if( drop.y.grids){
    g <-
      g +
      ggplot2::theme(panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank()
      )
  }








  g

}
