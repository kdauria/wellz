# A wrapper for approx that makes more sense to me
# pad=TRUE means that the NA's from extrapolation will be
# filled with the closest non-NA value
interpolate_linear = function(x,...) UseMethod("interpolate_linear",x)
interpolate_linear.default = function( x, y, xnew, pad=TRUE ) {
  ynew = approx(x,y,xnew)$y
  if(pad) {
    ynew[1] = Find(Negate(is.na),ynew)
    ynew = na.locf(ynew)
  }
  return(ynew)
}
interpolate_linear.data.table = function( dt, xnew, ... ) {  
  x = dt[[1]]
  y = dt[[2]]
  ynew = interpolate_linear( x, y, xnew, ... )
  out = data.table( x=xnew, y=ynew )
  setnames(out,colnames(out),colnames(dt))  
  return(out)
}