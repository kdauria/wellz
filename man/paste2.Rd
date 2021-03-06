% Generated by roxygen2 (4.0.2): do not edit by hand
\name{paste2}
\alias{paste2}
\title{Custom wrapper around \code{paste}}
\usage{
paste2(...)
}
\arguments{
\item{...}{like a call to \code{paste}}
}
\description{
Custom paste function that returns \code{NA}
if the input is \code{NULL} or if all inputs are \code{NA}.
Removes all arguments that are NA (though does not
remove the argument if it is a vector that is not all \code{NA}s).
Removes NULL arguments before the call to paste.
}

