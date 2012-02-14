\name{class.stats}
\alias{class.stats}
\alias{factor.stats}
\alias{integer.stats}
\alias{numeric.stats}
\title{Filter by class}
\usage{
  class.stats(.class)

  numeric.stats(x, ...)

  factor.stats(x, ...)

  integer.stats(x, ...)
}
\arguments{
  \item{x}{vector of any class}

  \item{.class}{string for class to filter by}

  \item{...}{passed to \code{\link{dostats}}}
}
\value{
  data frame of computed statistics if x is of class
  \code{.class} otherwise returns \code{NULL}.
}
\description{
  Filter by class
}
\seealso{
  \code{\link{dostats}}
}

