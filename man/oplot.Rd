\name{oplot}
\alias{oplot}
\title{Provide univariate or bivariate ggplot graphic according to the data type of the input variables.}
\usage{
oplot(xx, yy = NULL, zz = NULL, ll = NULL, simple = FALSE,
  histlabs = T, na.rm = F, fillcolour = RColorBrewer::brewer.pal(3,
  mypal)[2], mypal = "YlOrBr", sizefac = 15, reorder = F, xlabb = NULL,
  ylabb = NULL, xlablen = 30, ylablen = 30, xbreakswrap = 50,
  ybreakswrap = 50, sigLev = 0.01, xfilter = NULL, yfilter = NULL,
  usemultichoicelabs = F, ...)
}
\arguments{
  \item{xx}{A vector, dataframe, table or list.}

  \item{yy}{If provided, a second vector of the same length
  as xx.}

  \item{zz}{If provided, the statistic to be plotted.}

  \item{ll}{If provided, the label to be plotted.}

  \item{simple}{Whether to provide just a simple bar chart
  or line plot.}

  \item{histlabs}{Whether to provide Ns for bar charts.}

  \item{na.rm}{Whether to exclude missing values.}

  \item{fillcolour}{Fill colour for bar charts}

  \item{mypal}{RColourBrewer palette}

  \item{sizefac}{Font size multiplier for Ns on bar charts}

  \item{xlablen}{What line length to split for xlab}

  \item{ylablen}{What line length to split for ylab}

  \item{sigLev}{Graphs with associated p-values above this
  value will not be printed.}
}
\value{
A ggplot graphic, with additional information provided as
attributes.
}
\description{
Provide univariate or bivariate ggplot graphic according to
the data type of the input variables.
}
\examples{
Here are some examples
}
\seealso{
Other main omnivr functions: \code{\link{classer}};
\code{\link{omnivr}}, \code{\link{omnivr-package}};
\code{\link{otest}}; \code{\link{sankplot}}
}

