\name{sigHeat}
\alias{sigHeat}
\title{Produces a heatmap}
\usage{
sigHeat(xdat, ydat = xdat, fun = pseudocor, smallestprop = 0.75,
  showblocks = F, maxYlab = 95, matchreverse = F, ncoll = ncol(xdat),
  subtitle = "", maintitle = "")
}
\arguments{
  \item{xdat}{dataset}

  \item{ydat}{if given, a second dataset}

  \item{fun}{function e.g. correlation to relate the
  datasets}

  \item{ydat}{if given, a second dataset}

  \item{ydat}{if given, a second dataset}
}
\value{
A heatmap.2 plot
}
\description{
Produces a heatmap
}
\examples{
Here are some examples
}
\seealso{
Other experimental: \code{\link{loopr}};
\code{\link{pseudocor}}
}

