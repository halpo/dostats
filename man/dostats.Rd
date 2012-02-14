\name{dostats}
\alias{dostats}
\title{Convenient interface for computing statistics on a vector}
\usage{
  dostats(x, ..., .na.action = na.fail)
}
\arguments{
  \item{x}{the vector}

  \item{...}{statistics to compute, must take a vector and
  return a vector}

  \item{.na.action}{the action to take on NA values, for
  all statistics}
}
\value{
  A one row \code{data.frame} with columns named as in
  \code{...}
}
\description{
  Convenient interface for computing statistics on a vector
}
\examples{
data(mtcars)
library(plyr)
dostats(1:10, mean, median, sd, quantile, IQR)
ldply(mtcars, dostats, median, mean, sd, quantile, IQR)
}
\author{
  Andrew Redd
}
\seealso{
  \code{\link[plyr]{ldply}}
}
\keyword{misc}
\keyword{utilities,}

