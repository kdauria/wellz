#' Add smoother to well object
#' 
#' Calls \code{smoother} and adds the result to
#' the well object
#' 
#' @param x a \code{well} or \code{wellList} object
add_smoother = function(x, ...) UseMethod("add_smoother", x)
add_smoother.well = function(x, ...) {
  x$smoother = smoother(x, ...)
  x
}
add_smoother.wellList = function(x, ...) {
  x = lapply(x, add_smoother, ...)
  class(x) = c("wellList","list")
  x
}

#' Make smoother function from well data
#' 
#' This returns a function that will return
#' a "y-value" when given an "x-value". Several
#' smoother functions also have an 
#' \code{deriv} argument so that the derivative
#' will be returned.
#' 
#' @param x a \code{well} object
#' @param method smoother type, either \code{"smooth.spline"}
#'     \code{"lokerns"}, \code{"curfit"}, \code{"composite"}. These call
#'     \code{fsmoother_smooth.spline}, \code{fsmoother_lokerns}, 
#'     \code{fsmoother_curfit}, and \code{fsmoother_composite}, respectively.
#' @param ... passed to one of the fsmoother functions
smoother = function(x, ...) UseMethod("smoother", x)
smoother.well = function(x, method="smooth.spline", ...) {
  f = switch(method,
             smooth.spline=fsmoother_smooth.spline(tdata(x),vdata(x),...),
             lokerns=fsmoother_lokerns(tdata(x),vdata(x),...),
             curfit=fsmoother_curfit(tdata(x),vdata(x),...),
             composite=fsmoother_composite(tdata(x),vdata(x),...))
  return(f)
}


#' smooth.spline wrapper
#' 
#' Makes a smoother function that returns the y-value
#' given and x-value. Also allows the derivative to be 
#' returned using the \code{deriv} argument
#' 
#' @param x numeric vector of x values
#' @param y vector of y values
#' @param ... passed to \code{stats::smooth.spline}
fsmoother_smooth.spline = function( x, y, ... ) {
  object = smooth.spline( x, y, ...)
  function(x,deriv=0) predict(object,x,deriv)$y
}


#' lokerns smoother wrapper
#' 
#' Makes a smoother function that returns the y-value
#' given and x-value. Also allows the derivative to be 
#' returned using the \code{deriv} argument
#' 
#' @import lokern
#' @param x numeric vector of x values
#' @param y vector of y values
#' @param ... passed to \code{lokern::lokerns}
fsmoother_lokerns = function( x, y, ... ) {
  lfit = lokerns(x,y,...)
  lfit$trace.lev=0
  lfit$n = lfit$nobs
  function( x, deriv=0 ) predict(lfit,x,deriv)$y
}


#' DierckxSpline curfit wrapper
#' 
#' Makes a smoother function that returns the y-value
#' given and x-value. Also allows the derivative to be 
#' returned using the \code{deriv} argument.
#' 
#' @import DierckxSpline
#' @param x numeric vector of x values
#' @param y vector of y values
#' @param w weights on each data point
#' @param s the expected sum of squared weighted residuals for
#'            all data points (determines the amount of smoothing)
#' @param knots numeric vector setting location of knots
#' @param n the number of knots
#' @param ... passed to \code{DierckxSpline::curfit}
fsmoother_curfit = function( x, y, w=NULL, s=NULL, knots=NULL, n=NULL, ... ) {
  if( is.null(s) && is.null(knots) && is.null(n) ) n = min( 100, ceiling(length(x)/2))
  object = curfit(x,y,w,s,knots,n,...)
  function(x, deriv=0) deriv(object,x,order=deriv)
}


#' @title Combination smoother
#' 
#' @description
#' This is a very custom smoother that makes some assumptions
#' that won't work for every data set.
#' 
#' @details
#' The idea here is to use the \code{Dierckx::curfit} function
#' to find a spline whose squared residuals are close to
#' some number, here called \emph{s}. How then do we choose
#' \emph{s}? Trial and error works OK, but this function tries to
#' guess at the upper limit of \emph{s} by assuming the residuals
#' shouldn't be much more than the differences between
#' one point and the next point. Then, using this upper limit
#' as a references, one can choose what percentage of \emph{s}
#' will be used for the actual call to \code{curfit} (see the \code{sp} argument).
#' 
#' Alternatively, one may wish to take a first guess at a 
#' spline's residuals by fitting a very simple smoother or spline.
#' In this function, the moving average with width \code{window.width}
#' is used. If the window is small enough,
#' small variations due to noise will be removed so that a 
#' "better" global spline can be fit. However, there is danger
#' that too large a window will smooth out the signal too much. Choosing
#' the window width is up to the user. This smoother approach is only used if
#' the \code{window.width} argument is not \code{NULL}.
#' 
#' This function also gives the option to set residuals
#' below some number (set by the \code{min.dy} argument) to 
#' be set to zero before the global spline \code{curfit} call.
#' Theoretically, there is no obvious reason for this functionality, except
#' that sometimes it is a useful parameter to experiment with.
#' 
#' @import DierckxSpline
#' @param x numeric vector of x values
#' @param y vector of y values
#' @param sp a \code{numeric}, tercentage of upper limit of sum of squared residuals (see details)
#' @param min.dy a \code{numeric} (see details)
#' @param window.width a \code{numeric} (see details)
fsmoother_composite = function(x, y, sp=1, min.dy=0, window.width=NULL) {
  if( is.null(window.width) ) {
    resid = diff(y)
  } else {
    resid = y - smoother_maverage(x, y, window.width)
  }
  
  r = diff(range(y))
  resid[ resid<r*min.dy ] = 0
  
  upper.limit = sum(resid^2)
  message( "Upper limit of smoothing parameter s=", format(upper.limit, digits=3) )
  fsmoother_curfit( x, y, s=upper.limit*sp)
}


############################################################
#             Supporting smoother functions                #
############################################################


#' fast moving average
#' 
#' This function wraps an Rcpp function that calculates
#' the smoothing average very quickly
#' 
#' @param x the independent variable
#' @param y the dependent variable
#' @param window.width the width of the window in the moving average
smoother_maverage = function( x, y, window.width ) {
  out = rollmean_fc( x, y, x, width=window.width/2 )
  return(out)
}







