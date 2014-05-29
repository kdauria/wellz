# Add a spline to a well or wellList
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

# Insert n interpolated data points in between each data point
# The minimum space allowed between points is min.dx
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






