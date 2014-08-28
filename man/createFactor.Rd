\name{createFactor}
\alias{createFactor}
\title{Function to create a factor}
\usage{
  createFactor(x, levels)
}
\arguments{
  \item{x}{a numeric vector that denotes the indexes of the
  factor}

  \item{levels}{character vector for the unique levels of
  the factor}
}
\value{
  a factor
}
\description{
  Function creates a factor when supplied with a numeric
  vector and character vector for levels
}
\examples{
createFactor(sample(1:3, 10, TRUE), LETTERS[1:3])
}

