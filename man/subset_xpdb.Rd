% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xpdb_edits.R
\name{subset_xpdb}
\alias{subset_xpdb}
\alias{filter.xpose_data}
\alias{slice.xpose_data}
\alias{distinct.xpose_data}
\title{Subset datasets in an xpdb}
\usage{
\method{filter}{xpose_data}(.data, ..., .problem, .source, .where)

\method{slice}{xpose_data}(.data, ..., .problem, .source, .where)

\method{distinct}{xpose_data}(.data, ..., .problem, .source, .where)
}
\arguments{
\item{.data}{An xpose database object.}

\item{...}{Name-value pairs of expressions. Use \code{NULL} to drop a variable.

These arguments are automatically quoted and evaluated in the 
context of the data frame. They support unquoting and splicing. 
See the dplyr vignette("programming") for an introduction to these concepts.}

\item{.problem}{The problem from which the data will be modified}

\item{.source}{The source of the data in the xpdb. Can either be 'data' or an output 
file extension e.g. 'phi'.}

\item{.where}{A vector of element names to be edited in special (e.g. 
\code{.where = c('vpc_dat', 'aggr_obs')} with vpc).}
}
\description{
Use \code{filter()} to select rows/cases where conditions are true. 
Unlike base subsetting, rows where the condition evaluates to NA are dropped.
Use \code{slice()} to select row/cases by their position
}
\examples{
# Subset by condition
xpdb_ex_pk \%>\% 
 filter(DV < 1, .problem = 1) \%>\% 
 dv_vs_ipred()
  
# Subset by positions
xpdb_ex_pk \%>\% 
 slice(1:100, .problem = 1) \%>\% 
 dv_vs_ipred()
 
# Deduplicate rows
xpdb_ex_pk \%>\% 
 distinct(TIME, .problem = 1) \%>\% 
 dv_vs_ipred()
}
