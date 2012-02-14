\name{wargs}
\alias{wargs}
\title{Call with arguments}
\usage{
  wargs(f, ...)
}
\arguments{
  \item{f}{a function}

  \item{...}{extra arguments}
}
\value{
  a function that takes 1 argument and calls f with the
  single argument and the additional \code{...} appended.
}
\description{
  Call with arguments
}
\examples{
mean2 <- wargs(mean, na.rm=T)
}
\keyword{misc}
\keyword{utilities,}

