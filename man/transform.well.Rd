% Generated by roxygen2 (4.0.2): do not edit by hand
\name{transform.well}
\alias{transform.well}
\title{general data transformation function}
\usage{
\method{transform}{well}(x, methods, ...)
}
\arguments{
\item{x}{a \code{well} object}

\item{methods}{a character vector or list of functions}
}
\description{
This is mainly a wrapper for several other transformation functions.
This function allows many to be called at once using the
\code{methods} argument.
}
\details{
This \code{methods} argument may be a character vector
or a list of functions. The strings can be
\code{"tcenter"}, \code{"normalize"}, \code{"slice"},
or \code{"level"}.
}

