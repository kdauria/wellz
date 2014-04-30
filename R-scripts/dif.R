# differentiate a vector
library(numDeriv)

# smooth the data, use Richardson extrapolation to calculate the derivative,
# then smooth the derivative, returns a 'smooth.spline' object
dif = function( x, y, df=15 ) {
  
  # Smooth the function and take the derivative
  fs = smooth.spline(x,y,df=df)
  f = splinefun(fs$x,fs$y)
  fp = grad(f,x,"Richardson")
  
  # Smooth the derivative
  fps = smooth.spline(x,fp,df=df)
  return(fps$y)
  #return(fp)
}
