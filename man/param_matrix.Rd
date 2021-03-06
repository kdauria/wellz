% Generated by roxygen2 (4.0.2): do not edit by hand
\name{param_matrix}
\alias{param_matrix}
\title{Parameter matrix of wells}
\usage{
param_matrix(x, ..., type = "start", ID = "last", compound = NULL,
  solvent = NULL)
}
\arguments{
\item{x}{a \code{wellList} object}

\item{...}{passed to the \code{by} argument in \code{group}}

\item{type}{passed to \code{group}}

\item{ID}{passed to \code{group}}

\item{compound}{passed to \code{group}}

\item{solvent}{passed to \code{group}}
}
\description{
Make a matrix of well characteristics/parameters where each
row is for one well.
}
\details{
The first two columns of the matrix will be the file
and location of the well, which is enough to uniquely identify
it. Any further columns are specified in the \code{...} arguments.
These names of these arguments can only be
\code{"color"}, \code{"linetype"}, \code{"size"}, \code{"alpha"},
\code{"shape"}, or {"fill"} (see \code{check_well_params()}).
The values of these arguments can only be
\code{"location"}, \code{"file"}, \code{"volume"}, \code{"compound"},
\code{"concentration"}, \code{"solvent"}, or \code{"solvent.percentages"}.
This is because the values are passed to \code{group()}.
The name of the column will be the name of the argument and the
values of the column will be set to the vector from \code{group}.
The output is actually a \code{data.table}, not a \code{matrix}.
}

