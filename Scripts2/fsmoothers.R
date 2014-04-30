# Wrappers to smoother_ functions. All the smoother_ functions 
# have a consistent output but different outputs. These functions take the
# input and convert them all the same output -- a function like "predict"
# that takes an x-value and gives a y-value. The function can also
# be asked to give the derivative.
fsmoother = function( x, y, method="smooth.spline", ... ) {
  f = switch(method,
         smooth.spline=fsmoother_smooth.spline(x,y,...),
         lokerns=fsmoother_lokerns(x,y,...),
         curfit=fsmoother_curfit(x,y,...),
         composite=fsmoother_composite(x,y,...))
  return(f)
}

fsmoother_lokerns = function( x, y, ... ) {
  object = smoother_lokerns( x, y, ... )
  function( x, deriv=0 ) {
    predict(object,x,deriv)$y
  }
}

fsmoother_curfit = function( x, y, ... ) {
  object = smoother_curfit( x, y, ... )
  function(x, deriv=0) {
    deriv(object,x,order=deriv)
  }
}

fsmoother_smooth.spline = function( x, y, ... ) {
  object = smoother_smooth.spline( x, y, ...)
  function(x,deriv=0) {
    predict(object,x,deriv)$y
  }
}

fsmoother_composite = function( x, y, ...) {
  object = smoother_composite( x, y, ...)
  function(x, deriv=0) {
    deriv(object,x,order=deriv)
  }
}


