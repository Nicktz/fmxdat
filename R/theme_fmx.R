
#' @title theme_fmx
#' @description Plotting theme for ggplot: with boundary
#' @param FontType Check available fonts using: windowsFonts()
#' @return plot theme
#' @examples
#' g + theme_fmx(title.size = ggpts(35), axis.size.title = ggpts(35), grid_Col = "#FFFFFF") # Remove grids
#' g + theme_fmx(title.size = ggpts(35), axis.size.title = ggpts(35))
#'
#' # Custom Caption used as:
#' #   labs(caption= "First~line \n italic('and a second,')~bold('fancy one') \n 'also,'~integral(f(x)*dx, a, b)~'for good measure'")
#' # Bold and italics work from second line...
#' @export
#'

theme_fmx <- function(base.size = 15,
         title.size = 14,
         subtitle.size = 12,
         caption.size = 9,
         legend.size = 10,
         axis.size.title = 10,
         axis.size = 10,
         strip.size = 10,
         legend.pos = "bottom",
         grid_Col = "#F0EFEF",
         Strip_Col = "#DDE1EE",
         CustomCaption = FALSE,
         CaptionColor = "#606060",
         FontType = "sans") {

  safefontload <- purrr::safely(extrafont::loadfonts)
  x <- safefontload(device = "win", quiet = T)

  extrafont::loadfonts(device = "pdf", quiet = T)
  extrafont::loadfonts(device = "postscript", quiet = T)
  dkgray <- "gray27"

  if(CustomCaption){

    library(grid)
    element_custom <- function() {
      structure(list(), class = c("element_custom", "element_text"))
    }

    element_grob.element_custom <- function(element, label="", ...)  {
      disect <- strsplit(label, "\\n")[[1]]
      labels <- lapply(disect, function(x) tryCatch(parse(text=x),
                                                    error = function(e) x))
      hl <-  unit(rep(1, length(labels)), 'strheight', data=labels) + unit(0.1,"line")
      yl <- c(list(unit(0,"line")),
              lapply(seq_along(labels[-length(labels)]), function(ii) sum(hl[1:ii])))

      cl <- do.call(gList, Map(function(label, ii)
        textGrob(label, y = unit(1,"npc")-yl[[ii]], hjust=0, x=0, vjust=1),
        label = labels, ii = seq_along(labels)))

      gTree(children = cl, cl="sl", heights = hl, gp=gpar(col=CaptionColor,fontsize=caption.size))
    }

    heightDetails.sl <- function(x) sum(x$heights)


    .GlobalEnv$element_custom <- element_custom
    .GlobalEnv$element_grob.element_custom <- element_grob.element_custom
    .GlobalEnv$heightDetails.sl <- heightDetails.sl

    theme_satrix1 <-
      theme_grey() %+replace%

      ggplot2::theme(rect = element_rect(fill = "transparent",
                                         colour = NA, color = NA, size = 0, linetype = 0),
                     text = element_text(family = FontType, face = "plain",
                                         colour = "black", size = base.size, lineheight = 0.9,
                                         hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                                         debug = FALSE),
                     plot.title = ggplot2::element_text(size = title.size, face = "bold", color = "#222222",
                                                        # Left align
                                                        hjust = 0),
                     plot.subtitle = ggplot2::element_text(size = subtitle.size, margin = ggplot2::margin(9, 0, 9, 0),
                                                           # Left align
                                                           hjust = 0, colour = dkgray),
                     # plot.caption = ggplot2::element_text(size = caption.size),
                     #Legend
                     legend.position = legend.pos, legend.text.align = 0,
                     legend.background = ggplot2::element_blank(),
                     legend.title = ggplot2::element_blank(),
                     legend.key = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = legend.size,
                                                         color = "#222222"),
                     # Axes
                     # axis.title.x = ggplot2::element_blank(),
                     axis.text = ggplot2::element_text(size = axis.size,
                                                       color = "#222222"),
                     axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
                     axis.ticks = ggplot2::element_blank(),
                     axis.title = ggplot2::element_text(size = axis.size.title),
                     # Plot Border
                     # panel.border = element_blank(),
                     axis.line = element_line(colour = "black",
                                              size = rel(1)),
                     # Gridlines
                     panel.grid.major.y = ggplot2::element_line(color = grid_Col),
                     panel.grid.major.x = ggplot2::element_line(color = grid_Col),

                     # panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     # panel.grid.minor = element_line(size = rel(0.5)),

                     # Panels:
                     panel.background = ggplot2::element_blank(),
                     strip.background = element_rect(fill = Strip_Col),
                     strip.text = element_text(face = "bold", colour = "black", size = strip.size),
                     plot.caption = element_custom())


  } else {

    theme_satrix1 <-
      theme_grey() %+replace%

      ggplot2::theme(rect = element_rect(fill = "transparent",
                                         colour = NA, color = NA, size = 0, linetype = 0),
                     text = element_text(family = FontType, face = "plain",
                                         colour = "black", size = base.size, lineheight = 0.9,
                                         hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                                         debug = FALSE),
                     plot.title = ggplot2::element_text(size = title.size, face = "bold", color = "#222222",
                                                        # Left align
                                                        hjust = 0),
                     plot.subtitle = ggplot2::element_text(size = subtitle.size, margin = ggplot2::margin(9, 0, 9, 0),
                                                           # Left align
                                                           hjust = 0, colour = dkgray),
                     plot.caption = ggplot2::element_text(size = caption.size),
                     #Legend
                     legend.position = legend.pos, legend.text.align = 0,
                     legend.background = ggplot2::element_blank(),
                     legend.title = ggplot2::element_blank(),
                     legend.key = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = legend.size,
                                                         color = "#222222"),
                     # Axes
                     # axis.title.x = ggplot2::element_blank(),
                     axis.text = ggplot2::element_text(size = axis.size,
                                                       color = "#222222"),
                     axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
                     axis.ticks = ggplot2::element_blank(),
                     axis.title = ggplot2::element_text(size = axis.size.title),
                     # Plot Border
                     # panel.border = element_blank(),
                     axis.line = element_line(colour = "black",
                                              size = rel(1)),
                     # Gridlines
                     panel.grid.major.y = ggplot2::element_line(color = grid_Col),
                     panel.grid.major.x = ggplot2::element_line(color = grid_Col),

                     # panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     # panel.grid.minor = element_line(size = rel(0.5)),

                     # Panels:
                     panel.background = ggplot2::element_blank(),
                     strip.background = element_rect(fill = Strip_Col),
                     strip.text = element_text(face = "bold", colour = "black", size = strip.size))


  }

}
