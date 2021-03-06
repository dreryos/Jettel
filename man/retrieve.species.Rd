% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jettel.r
\name{retrieve.species}
\alias{retrieve.species}
\title{Get dataframe of only one species selected form searched plants}
\usage{
retrieve.species(token = 0, species = "")
}
\arguments{
\item{token}{User token, string, default null.}

\item{species}{User query, string, default null.}
}
\value{
A dataframe of selected species, from list all plants which satisfy given search criteria
}
\description{
This function search plant which satisfies given query and lets user choose one plant.
}
\examples{
retrieve.species("token", "coconut")
}
\keyword{search}
\keyword{species}
