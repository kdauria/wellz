#' Add a monotone Hermite spline to well
#' 
#' Calls \code{stats::splinefun} with \code{method="monoH.FC"} and
#' adds the resulting function to the \code{well} object.
#' 
#' @param x a \code{well} object
#' @param ... ignored
add_spline = function(x,...) UseMethod("add_spline",x)
add_spline.well = function(x, ...) {
  x$spline = splinefun( x=x$data$t, y=x$data$value, method="monoH.FC" )
  x
}
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
insert_n_between_spline.well = function( x, ..., deriv=0, type="spline" ) {
  new.i = insert_na_between( tdata(x), ... )
  new.t = na_interp( new.i )
  new.i[!is.na(new.i)] = idata(x)
  new.value = x[[type]]( new.t, deriv=deriv ) # the spline or smoother function stored in the well
  wdata(x) = data.frame( i = new.i, t = new.t, value = new.value )
  x
}
insert_n_between_spline.wellList = function(x, ...) {
  x = lapply( x, insert_n_between_spline, ... )
  class(x) = c("wellList","list")
  x
}






