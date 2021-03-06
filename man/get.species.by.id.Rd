% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jettel.r
\name{get.species.by.id}
\alias{get.species.by.id}
\title{Get JSON list of species by given ID}
\usage{
get.species.by.id(token = 0, id = 0)
}
\arguments{
\item{token}{User token, string, default null.}

\item{id}{ID of species, integer, default null.}
}
\value{
List of data of given species
}
\description{
This function return list of data of given species
}
\examples{
get.species.by.id("token", 000001)
}
\keyword{data}
\keyword{species}
