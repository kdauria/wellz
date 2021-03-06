% Generated by roxygen2 (4.0.2): do not edit by hand
\name{fsmoother_composite}
\alias{fsmoother_composite}
\title{Combination smoother}
\usage{
fsmoother_composite(x, y, sp = 1, min.dy = 0, window.width = NULL)
}
\arguments{
\item{x}{numeric vector of x values}

\item{y}{vector of y values}

\item{sp}{a \code{numeric}, tercentage of upper limit of sum of squared residuals (see details)}

\item{min.dy}{a \code{numeric} (see details)}

\item{window.width}{a \code{numeric} (see details)}
}
\description{
This is a very custom smoother that makes some assumptions
that won't work for every data set.
}
\details{
The idea here is to use the \code{Dierckx::curfit} function
to find a spline whose squared residuals are close to
some number, here called \emph{s}. How then do we choose
\emph{s}? Trial and error works OK, but this function tries to
guess at the upper limit of \emph{s} by assuming the residuals
shouldn't be much more than the differences between
one point and the next point. Then, using this upper limit
as a references, one can choose what percentage of \emph{s}
will be used for the actual call to \code{curfit} (see the \code{sp} argument).

Alternatively, one may wish to take a first guess at a
spline's residuals by fitting a very simple smoother or spline.
In this function, the moving average with width \code{window.width}
is used. If the window is small enough,
small variations due to noise will be removed so that a
"better" global spline can be fit. However, there is danger
that too large a window will smooth out the signal too much. Choosing
the window width is up to the user. This smoother approach is only used if
the \code{window.width} argument is not \code{NULL}.

This function also gives the option to set residuals
below some number (set by the \code{min.dy} argument) to
be set to zero before the global spline \code{curfit} call.
Theoretically, there is no obvious reason for this functionality, except
that sometimes it is a useful parameter to experiment with.
}

