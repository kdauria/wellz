######################################################################
#         Smoothing and interpolating splines for wells              #
######################################################################

################## Smoothing splines #############

add_smoother = function(x,...) UseMethod("add_smoother",x)
add_smoother.wellList = function( wells, ... ) {
  x = lapply(wells,add_smoother, ... )
  class(x) = class(wells)
  x
}
add_smoother.Well = function( well, method="smooth.spline", nbw=0, min.diff=NULL, ... ) {
  wellint = spline_interpolate(well,nbw=nbw,min.diff=min.diff)
  x = timesdata(wellint)
  y = welldata(wellint)
  well$smoother = fsmoother(x,y,method,...)
  message("Smoother added")
  return(well)
}

################ Interpolating splines ###################

# Add interpolating function to a well
add_interpolating_spline = function(x,...) UseMethod("add_interpolating_spline",x)
add_interpolating_spline.wellList = function( wells, ... ) {
  x = lapply(wells,add_interpolating_spline,... )
  class(x) = class(wells)
  x
}
add_interpolating_spline.Well = function( well, ... ) {
  d = welldata(well)
  tm = timesdata(well)
  well$spline = splinefun(x=tm,y=d,method="monoH.FC",...)
  return(well)
}

