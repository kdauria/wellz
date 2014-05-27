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



