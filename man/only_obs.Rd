% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_data.R
\name{only_obs}
\alias{only_obs}
\title{Create functions to drop non observation records}
\usage{
only_obs(xpdb, .problem, quiet)
}
\arguments{
\item{xpdb}{An xpose database object.}

\item{.problem}{The $problem number to be used.}

\item{quiet}{Should messages be displayed to the console.}
}
\value{
A function
}
\description{
Create shortcut functions on the fly to remove records 
not associated with an observation.
}
\keyword{internal}
