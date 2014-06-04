\name{sankplot}
\alias{sankplot}
\title{Wrapper for riverplot}
\usage{
sankplot(mm, minedge = 1, strw = 12, pastel = T)
}
\arguments{
  \item{m}{an edge matrix with just two columns, each row
  is one case joining a and b. e.g. mtcars[,3:4] you could
  argue that to be consistent with oplot it should take x
  and y as input not a dataframe of them. but this way it
  is easier to generalise to more}
}
\description{
A simple wrapper for \code{riverplot()}. If same values are
found in each column, same colour will be used for each
}
\note{
At the moment, integer is treated as continuous.
}
\seealso{
Other main omnivr functions: \code{\link{classer}};
\code{\link{omnivr}}, \code{\link{omnivr-package}};
\code{\link{oplot}}; \code{\link{otest}};
\code{\link{worldmap}}
}

