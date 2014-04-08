\name{qplotr}
\alias{qplotr}
\title{Provide univariate or bivariate ggplot graphic according to the data type of the input variables.}
\usage{
qplotr(xx, yy = NULL, simple = F, histlabs = T, na.rm = F,
  fillcolour = RColorBrewer::brewer.pal(3, mypal)[2], mypal = "YlOrBr",
  sizefac = 15, xlablen = 30, ylablen = 30, sigLev = 0.05)
}
\arguments{
  \item{xx}{A vector.}

  \item{yy}{If provided, a second vector of the same length
  as xx.}

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
Other main: \code{\link{quickplotr}},
\code{\link{quickplotr-package}}; \code{\link{quicktestr}}
}

