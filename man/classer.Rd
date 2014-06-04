\name{classer}
\alias{classer}
\title{Returns data type of a vector}
\usage{
classer(x)
}
\arguments{
  \item{x}{A vector.}
}
\description{
A simple wrapper for \code{class()}.  The way
\code{class()} expresses the difference between an ordinal
an nominal variable is not convenient for our purposes.
Also allows for special data types to be set using the
variable attribute \code{setlevout}.
}
\note{
At the moment, integer is treated as continuous.
}
\examples{
class(1)
class(1:2)
classer(1)
classer(1:2)

x=factor(1:2);classer(x)
x=factor(1:2,ordered=T);classer(x)
x=1.25;classer(x)


}
\seealso{
Other main omnivr functions: \code{\link{omnivr}},
\code{\link{omnivr-package}}; \code{\link{oplot}};
\code{\link{otest}}; \code{\link{sankplot}};
\code{\link{worldmap}}
}

