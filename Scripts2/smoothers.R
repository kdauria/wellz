# Different smoothing splines that can be applied to the data.
# All smoother_* functions return the fit from many different
# smoothing functions. Therefore, the outputs from each 
# function are not consistent
smoother_lokerns = function(x, ...) UseMethod("smoother_lokerns", x)
smoother_lokerns.default = function(x,y,...) {
  lfit = lokerns(x,y,...)
  lfit$trace.lev=0
  lfit$n = lfit$nobs
  return(lfit)
}


# Smoothing with the stats package
smoother_smooth.spline = function(x, ...) UseMethod("smoother_smooth.spline", x)
smoother_smooth.spline.default = function(x,...) smooth.spline(x, ...)

# Smoothing with the Dierckx package
smoother_curfit = function(x, ...) UseMethod("smoother_curfit", x)
smoother_curfit.default = function(x, y=NULL, w=NULL, s=NULL, knots=NULL, n=NULL, ...) {
  if( is.null(s) && is.null(knots) && is.null(n) ) n = min( 100, ceiling(length(x)/2))
  curfit(x,y,w,s,knots,n,...)
}

# Find the midpoints between each element of the numeric vector
midpoint = function(x) UseMethod("midpoint",x)
midpoint.default = function(x) x[-length(x)] + diff(x)/2
midpoint.factor = function(x) {
  levels(x) = as.numeric(levels(x))
  midpoint(as.numeric(x))
}

# Not really a "smoother" in the typical sense. 
# Bins the data and takes the average y-value of each bin
# Will also take the forward difference of the bins (not the original
# data) if the derivative is requested
smoother_bin = function(x,...) UseMethod("smoother_bin",x)
smoother_bin.default = function(x,y,width=NULL,nbins=NULL,deriv=0) {
  
  # now do a derivative of the binned/average data points if requested
  if(deriv==1) {
    DT = condense(x, y, width=width, nbins=nbins )
    
    if(nrow(DT)==1) {
      slope = coef( lm(y~x) )["x"]
      w = abs( rep(slope,length(x)) )
      out = data.table(bins=1,x=mean(x),dydx=w)
    } else {
      out = DT[,list(x=midpoint(x), y=diff(y)/diff(x)), ]
    }
  } else {
    DT = condense(x, y, width=width, nbins=nbins )
    out = DT[,list(x,y)]
  }
  return(out)
}

# A moving average smoother. Can do binning before rolling.
# Can also take the forward difference between rolling and smoothing
# to estimate the derivative.
smoother_maverage = function(x,...) UseMethod("smoother_maverage",x)
smoother_maverage.default = function( x, y, window.width ) {
  out = rolltime( data.table(x,y), width=window.width )
  return(out)
}

# A fairly complex smoother which is a composite of several other smoothers
# Optimized for the scales of biological data (e.g., slow growth of cells
# which are then sharply affected by something such as drug treatment). 
# STEP 1: If the goal is to find the derivative of the curve (as it often is when
# calculating rates), then high-frequency, low-amplitude noise ("white noise") can cause big
# problems. The solution here tries to average out the white noise by binning the
# data into bins that are large enough to average out the noise.
# The width of these bins should be just large enough to get rid of noise. If
# it is set too large, then important trends in the data may be lost. 
# The width of the bins is set by "noise.scale".
# STEP 2: The sharp spikes in biological data are often the most intriguing
# pieces of the data. A global smoothing spline (e.g, typical cubic splines) with 
# a fixed bandwidth will most likely over-smooth the data around these spikes. One 
# solution for different amounts of smoothing at different points in the data is to
# change the "bandwidth" of a smoother at different points along the x-axis. There are various
# algorithms that try to best estimate how to modify the bandwidths given the data. 
# Some work well for a fairly general set of data. However, they do not work
# consistently on the data where the majority of the x-y relationship is smooth
# with a few sharp spikes. To try to solve this problem, the bandwidth of the 
# smoother will be estimated from the absolute value of the derivative. High
# derivatives will be translated to low bandwidths. The
# derivative is smoothed by a moving average so that points around the sharp spikes are 
# also given low bandwidths. The widht of the moving average window is set by "x.scale".
# "x.scale" should be chosen as about twice the distance away from
# a sharp spike that a low bandwidth should occur. If it's set too small,
# then other points far from the spikes will likely be overfit. This is probably the
# parameter that will need the most tuning by trial and error.
# STEP 3: The smoothed derivatives are then mapped to bandwidths using "deriv.cutoffs".
# The maximum bandwidth is set to "x.scale" and the minumum bandwidth is the
# distance between two adjacent points. The bandwidth for points with derivative above
# "deriv.cutoff" is set to the maximum bandwidth. Derivatives below "deriv.cutoffs"
# are set to the minimum bandwidth. Derivatives in between "deriv.cutoffs" are 
# set to bandwidths in between the maximum and minimum bandwidths.
# STEP 4: A local kernel regression is run on the data using the bandwidths (lokerns)
# STEP 5: Often, the local kernel regression is still jagged because of the jagged
# changes to the bandwidth introduced previously. However, the residuals of the 
# lokerns regression provides a decent estimate of error across all of the data.
# This error can be used for another algorithm that attempts to find the 
# best cubic spline that matches a desired amount of error (see the Dierckx library)
# Therefore, the output of this composite spline is actually just a simple cubic
# spline. However, the cubic spline usually fits the sharp spikes very closely (no
# oversmoothing). The other points away from the spikes have a relatively much larger
# amount of smoothing, which is often desired.
smoother_composite = function( x, y, x.scale, y.scale=max( max(y), diff(range(y)) ),
                               noise.scale=0, deriv.cutoffs=c(0.05,0.25)) {
  
  a = smoother_bin(x,y,noise.scale,deriv=1) # fine bin then take derivative
  b = smoother_maverage(a$x,abs(a$y),x.scale) # rolling average of absolute value of derivative
  w = interpolate_linear( b, x ) # weights: moving average "unbinned" to original data
  bw = weights_to_bw( w, yscale=y.scale, wrange=deriv.cutoffs, 
                      bwrange=c(0,0.95*x.scale) ) # map the weights to a bandwidth
  lfit = smoother_lokerns( x, y, bandwidth=bw, k=4 ) # Do a local kernel regression with the bandwidths
  fit = smoother_curfit(x=x,y=y,s= sum(resid(lfit)^2) ) # Do global fit using the errors from the local fit
  return(fit)
}

# A function which maps a weight to a bandwidth by linear mapping between two ranges
# wrange gives the minimum and maximum values of the weights
# bwrange gives the minimum and maximum values for the bandwidth
# yscale is simply a scaling factor for different data sets. Defaults to no scaling
weights_to_bw = function( df, yscale=1, wrange=c(0.05,0.25), bwrange=c(0,0.5) ) {
  
  x = df[[1]]
  w = df[[2]]
  
  # A simple linear mapping from weights to bandwidths
  wmin = wrange[1]
  wmax = wrange[2]
  bwmin = bwrange[1]
  bwmax = bwrange[2]
  w.points = c(min(w)-1,wmin,wmax,max(w)+1)
  bw.points = c(bwmax,bwmax,bwmin,bwmin)
  bw = approx(w.points,bw.points,w)$y/yscale
  
  # Make the bandwidth reach at least to an adjacent piont
  rdiff = c( diff(x), 0 )
  ldiff = c( 0, diff(x) )
  mindist = pmin(ldiff,rdiff)
  bw[ bw<mindist ] = mindist[ bw<mindist ]
  return(bw)
}






