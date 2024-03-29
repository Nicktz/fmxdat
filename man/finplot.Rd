% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finplot.R
\name{finplot}
\alias{finplot}
\title{finplot}
\usage{
finplot(
  g,
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
  drop.y.grids = FALSE
)
}
\arguments{
\item{x.comma.sep}{Make x axis not show scientific values (e.g. shows 25,000,000 as opposed to 2.5e+6)}

\item{y.comma.sep}{Make y axis not show scientific values (e.g. shows 25,000,000 as opposed to 2.5e+6)}

\item{x.pct}{Make x axis percentages}

\item{x.pct_acc}{Rounding of pct value.}

\item{y.pct}{Make y axis percentages}

\item{y.pct_acc}{Rounding of pct value.}

\item{x.vert}{flip x axis}

\item{y.vert}{flip y axis}

\item{x.ticks.rm}{Remove x ticks altogether}

\item{y.ticks.rm}{Remove x ticks altogether}

\item{legend_pos}{where to position legend. Defaults to bottom.}

\item{col.hue}{set color hues. E.g., set hue = 40 to see what happens}

\item{fill.hue}{set fill hues. E.g., set hue = 40 to see what happens}

\item{darkcol}{Use dark colors}

\item{x.date.type}{Of e.g. form '\%Y', '\%y_\%b'}

\item{x.date.dist}{Space between date points on x axis. E.g., "2 years", "10 week"}

\item{x.date.vector.sel}{Provide a vector of possible dates - this way weekends and holidays are e.g. removed... use bdscale here. E.g. use: x.date.vector.sel = unique(df$date)}

\item{legend.line.size}{Make legend lines larger}

\item{legend.text.size}{Adj legend text}

\item{legend.line.alpha}{Make legend lines lighter}

\item{log.y}{Apply a log transformation on the y-axis. This is a log base 10 transformation}

\item{title.center}{Put title in center of figure. Used with grids or facet wraps e.g.}
}
\value{
ggplot object
}
\description{
Plotting aide for Psec plotting functionalities.
}
\examples{
g <- ggplot() + geom_line(aes(date, returns, color = Funds)) + theme_fmx()
finplot(g, x.pct = TRUE)
}
