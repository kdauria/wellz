# wrapper function to add a smoother to a well
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


# wrapper function to produce a smoother function
smoother = function(x, ...) UseMethod("smoother", x)
smoother.well = function(x, method="smooth.spline", ...) {
  f = switch(method,
             smooth.spline=fsmoother_smooth.spline(tdata(x),vdata(x),...),
             lokerns=fsmoother_lokerns(tdata(x),vdata(x),...),
             curfit=fsmoother_curfit(tdata(x),vdata(x),...),
             composite=fsmoother_composite(tdata(x),vdata(x),...))
  return(f)
}


# Smoothing with the stats package
fsmoother_smooth.spline = function( x, y, ... ) {
  object = smooth.spline( x, y, ...)
  function(x,deriv=0) predict(object,x,deriv)$y
}

# Different smoothing splines that can be applied to the data.
# The fsmoother_* functions create a function like splinefun that can be used for prediction
fsmoother_lokerns = function( x, y, ... ) {
  lfit = lokerns(x,y,...)
  lfit$trace.lev=0
  lfit$n = lfit$nobs
  function( x, deriv=0 ) predict(lfit,x,deriv)$y
}

# Smoothing with the Dierckx package
# w is the weights on data points
# s is a smoothing factor
# knots restricts the # knots
# n, I forgot what it does
fsmoother_curfit = function( x, y, w=NULL, s=NULL, knots=NULL, n=NULL, ... ) {
  if( is.null(s) && is.null(knots) && is.null(n) ) n = min( 100, ceiling(length(x)/2))
  object = curfit(x,y,w,s,knots,n,...)
  function(x, deriv=0) deriv(object,x,order=deriv)
}

aa = x[[1]]
x1 = tdata(aa)
y1 = vdata(aa)
sum(diff(y1)^2) # upper limit on the smoothing
fsmoother_composite(x1,y1,0.95)

# The Dierckx package attempts to find a curve 
# whose sum(residuals^2) is equal to a user-chosen number s
# The choice made here is that the first order derivative
# gives the upper limit of the residual, let's make this s
# Then sp is the percentage of s used as the smoothing parameter
# To improve the estimate of s, we may wish to subtract
# out noise. The problem is that noise is somewhat of a subjective term
# Here, the idea is to take a user-defined window and take the
# moving average over that window. Then, if the difference
# of the moving average and the actual data point is small, the
# residual at this point is assumed to be zero (due to noise). 
# Smaller windows set more stringent requirements on what constitutes noise

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

# take a well, bin it, and output a data.table
get_bins = function(x, nbins=NULL, width=NULL) {
  xrange = range(x)
  stopifnot( xor( is.null(nbins), is.null(width) ) )
  if(is.null(nbins)) nbins = ceiling(diff(xrange)/width)
  
  breaks = seq(xrange[1], xrange[2], length.out=nbins)
  .bincode( x, breaks )
}
bin_well_data = function(x, ...) {
  
  out = data.table(wdata(x))
  bins = get_bins(tdata(x), ...)
  bins[1] = 1
  
  out[, bin:=bins]
  out[,list(t,value=mean(value)),by=bin]
}


# A moving average smoother. Can do binning before rolling.
# Can also take the forward difference between rolling and smoothing
# to estimate the derivative.
smoother_maverage = function( x, y, window.width ) {
  out = rollmean_fc( x, y, x, width=window.width )
  return(out)
}

# A very fast rolling mean with "INTERVAL"
# zoo::rollmean finds the "neighborhood" for the local mean by
# finding the "k" closest points. This function finds all the
# points that are distance "width" away from the point of interest
cppFunction('
            NumericVector rollmean_fc( NumericVector x, NumericVector y, 
                                       NumericVector xout, double width) {
            double total=0;
            unsigned int n=x.size(), nout=xout.size(), i, ledge=0, redge=0;
            NumericVector out(nout);
            
            for( i=0; i<nout; i++ ) {
              while( x[ redge ] - xout[i] <= width && redge<n ) 
                total += y[redge++];
              while( xout[i] - x[ ledge ] > width && ledge<n ) 
                total -= y[ledge++];
              if( ledge==redge ) { out[i]=NAN; total=0; continue; }
              out[i] = total / (redge-ledge);
            }
            return out;
            }')





