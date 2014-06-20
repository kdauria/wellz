#' Add a monotone Hermite spline to well
#' 
#' Calls \code{stats::splinefun} with \code{method="monoH.FC"} and
#' adds the resulting function to the \code{well} object.
#' Does not allow extrapolation; NA values will be returned.
#' 
#' @param x a \code{well} object
#' @param ... ignored
#' @export
add_spline = function(x,...) UseMethod("add_spline",x)
#' @export
add_spline.well = function(x, ...) {
  sfun = splinefun( x=tdata(x), y=vdata(x), method="monoH.FC" )
  bounds = range( tdata(x)[!is.na(vdata(x))] )
  
  # The function saved to x$spline
  # Note the sfun, bounds, and x will be saved in this
  # function's environment. (Lots of data in x)
  # so maybe later find a way to avoid saving x twice
  x$spline = function(xx, deriv=0) {
    
    # Set extrapolated values to NA
    xx[ xx < bounds[1] | xx > bounds[2] ] = NA
    
    # Only do interpolation on non-NA values since
    # some functions from splinefun fail when given NA values
    xx[!is.na(xx)] = sfun(na.omit(xx), deriv=deriv)
    xx
  }
  x
}
#' @export
add_spline.wellList = function(x, ...) {
  for( i in seq_along(x))
    x[[i]] = add_spline(x[[i]],...)
  x
}

#' Insert points with spline or smoother
#' 
#' Interpolates points in between the current data points using
#' the spline stored in the well object.
#' 
#' @param x a \code{well} or \code{wellList} object
#' @param ... passed to \code{insert_na_between} (see documentation for that function)
#' @param deriv which order derivative to return
#' @param a string, either \code{"spline"} or \code{"smoother"}
insert_n_between_spline = function(x,...) UseMethod("insert_n_between_spline",x)
#' @export
insert_n_between_spline.well = function( x, ..., deriv=0, type="spline" ) {
  new.i = insert_na_between( tdata(x), ... )
  new.t = na_interp( new.i )
  new.i[!is.na(new.i)] = idata(x)
  new.value = x[[type]]( new.t, deriv=deriv ) # the spline or smoother function stored in the well
  wdata(x) = data.frame( i = new.i, t = new.t, value = new.value )
  x
}
#' @export
insert_n_between_spline.wellList = function(x, ...) {
  x = lapply( x, insert_n_between_spline, ... )
  class(x) = c("wellList","list")
  x
}






