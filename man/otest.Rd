\name{otest}
\alias{otest}
\title{Provide a simple bivariate test for two variables depending on the types of the variables.
Also provides some basic statistics for a single variable}
\usage{
otest(xx, yy = NULL, level1 = "nom", level2 = "nom", spv = F, ...)
}
\arguments{
  \item{xx}{A vector.}

  \item{yy}{If provided, a second vector of the same length
  as xx.}

  \item{level1}{Force type of first variable}

  \item{level2}{Force type of second variable}

  \item{spv}{Whether to simulate p values for chi-squared
  tests. May take a while.}
}
\value{
The p-value, with additional information provided as
attributes.
}
\description{
Provide a simple bivariate test for two variables depending
on the types of the variables. Also provides some basic
statistics for a single variable
}
\examples{
Here are some examples
}
\seealso{
Other main: \code{\link{oplot}}, \code{\link{oplot}},
\code{\link{oplot-package}}
}

