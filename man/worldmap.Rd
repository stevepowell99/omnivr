\name{worldmap}
\alias{worldmap}
\title{Just a wrapper for rworldmap}
\usage{
worldmap(data, cutData = T, breaks = NULL, labels = NULL,
  legendLabel = "Category", mapTitle = "", xlim = NULL, ylim = NULL,
  ...)
}
\arguments{
  \item{data}{with xx= country names and yy value to be
  plotted}

  \item{title}{title}
}
\value{
A plot
}
\description{
Just a wrapper for rworldmap
}
\examples{
library(rworldmap)
data=(data.frame(xx=c("Germany","France","Brazil"),yy=1:3))
worldmap(data)
}
\seealso{
Other main omnivr functions: \code{\link{classer}};
\code{\link{omnivr}}, \code{\link{omnivr-package}};
\code{\link{oplot}}; \code{\link{otest}};
\code{\link{sankplot}}
}

