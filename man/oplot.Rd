\docType{package}
\name{oplot}
\alias{oplot}
\alias{oplot-package}
\title{Quick ggplots}
\usage{
oplot(xx, yy = NULL, simple = F, histlabs = T, na.rm = F,
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
ggplot2 is great for providing highly customisable plots.
oplot makes it just a little bit quicker to provide the
simplest plots - bar charts, scatterplots etc - with a
consistent appearance.

Provide univariate or bivariate ggplot graphic according to
the data type of the input variables.
}
\details{
The main use case for oplot is providing quick overview
reports from the results of a questionnaire survey:
providing tests of main variables against background, often
sociodemographic, variables which are usually of mixed data
type - nominal, ordinal etc. oplot provides a function
otest which conducts the statistical tests and provides a
p-value. By default, if oplot is provided with two
variables, and the corresponding oplot test for those two
variables is not significant, the plot is not produced,
though the returned empty string does contain informative
attributes.
}
\examples{
Here are some examples
}
\seealso{
Other main: \code{\link{otest}}

Other main: \code{\link{otest}}
}

