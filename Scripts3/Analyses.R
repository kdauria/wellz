
# Find the maximum rate and the time at which it occurs
max.rate = function(x,...) UseMethod("max.rate",x)
max.rate.wellList = function( wells, ID, ... ) {
  out = lapply(wells,max.rate,ID=ID, ...)
  class(out) = class(wells)
  return(out)
}
max.rate.Well = function( well, ID, duration=Inf, nbw=10, min.diff=NULL, 
                          ylim=0.75, xlim=2, smoother=TRUE ) {
  
  # First get the derivative data
  dwell = spline_interpolate(well,deriv=1,nbw=nbw, smooth=smoother, min.diff=min.diff)
  times = timesdata(dwell)
  dm.deriv = datamat(dwell)
  
  # Get the regular data
  dwell = spline_interpolate(well,deriv=0, nbw=nbw, smooth=FALSE, min.diff=min.diff)
  vals = welldata(dwell)
  
  # get the starting time
  sweep = getrows.by.ID(dwell,ID)$t1
  idx = which(sweepdata(dwell)==sweep)
  tstart = times[idx]
  
  # the block of data to consider (based on the ID and duration)
  idx.range = times > tstart & times < (tstart + duration)
  
  # the block of data to consider (based on xlim and ylim )
  # data points with time<xlim && data>ylim won't be considered
  # this is to account for the small but sharp
  # data spikes that occur at the beginning of an action
  idx.spike = times < (tstart+xlim) & vals>ylim
  
  # subset the data according to the two restrictions above
  dm.deriv = dm.deriv[ idx.range & !idx.spike, ]
  
  # take the minimum rate
  id = which.min(dm.deriv$values)
  rate = dm.deriv[id,c("time","values")]
  colnames(rate) = c("time","value")
  
  well$metrics$max.rate = as.data.frame(as.list(rate))
  return(well)
}

# Function to calculate the area under the curve of a well
integrate = function(x,...) UseMethod("integrate",x)
integrate.default = function(x,...) stats::integrate(x,...)
integrate.wellList = function( wells, lower, upper, smoother=FALSE, baseline=FALSE ) {
  
  # get the area for each well
  out = lapply(wells, integrate, lower, upper, smoother)
  class(out) = class(wells)
  
  # subtract out controls as the baseline if requested
  if(baseline=="controls" || baseline=="controls.samefile") {
    
    controls.logical = findControls( out, y=rep(TRUE,length(out)), 
                                     samefile= baseline=="controls.samefile" )
    controls = which(controls.logical)
    controls.area = vapply( out[controls], function(x) x$metrics$integral$value, 1 )
    control.area = mean(controls.area)
    for( i in seq_along(out) ) {
      out[[i]]$metrics$integral$value = out[[i]]$metrics$integral$value - control.area
    }
  }
  
  return(out)
}
integrate.Well = function(well,lower,upper,smoother=FALSE) {
  
  fun = if(smoother) well$smoother else well$spline
  area = tryCatch( integrate(fun,lower,upper,subdivisions=5000)$value,
                   error = function(e) {
                     message(str_c("stats::integrate ",e$message) )
                     message("Will instead calculate the integral by the trapezoidal rule" )
                     nwell = slice(well,xlim=c(lower,upper))
                     x = timesdata(nwell)
                     y = welldata(nwell)
                     trapz(x,y)
                   } )
  well$metrics$integral = data.frame(lower=lower,upper=upper,value=area)
  return(well)
}


# Find when the data crosses some cutoff ('ylev') for 
# some duration of time ('tdelay') without crossing back
breakout = function(x, ...) UseMethod("breakout",x)
breakout.wellList = function(x, ylev=0.5, tdelay=1, ID=NA ) {
  out = lapply( x, breakout )
  class(out) = class(x)
  return(out)
}
breakout.Well = function(x, ylev=0.5, tdelay=1, ID=NA ) {
  
  # Only look at points after the "ID"
  if( !is.na(ID) ) {
    n = length(timesdata(x))
    sweep = getrows.by.ID(x,ID)$t1
    tstart = which( sweepdata(x)==sweep )
    times = timesdata(x)[tstart:n]
    values = welldata(x)[tstart:n]
  } else {
    times = timesdata(x)
    values = welldata(x)
  }
  
  # Identify the "switch" points when it goes above and below
  above.below = values < ylev
  switches = which( diff(above.below)!=0 )
  if(length(switches)==0) {
    warning("Data never crosses 'ylev'")
    x$metrics$breakout = data.frame( ylev=ylev, value=NA )
    return( x )
  }
  
  # Go through the switches until one is found
  for( s in switches ) {
    
    # use the interpolating spline to get an exact time
    well.spline = x$spline
    t1 = times[s]
    t2 = times[s+1]
    t.int = seq(t1,t2,length.out=1000)
    y.int = well.spline(t.int)
    s.int = which( diff( y.int < ylev ) != 0 )[1]
    switch.time = t.int[s.int]
    
    # Make sure the change lasts "tdelay"
    window = times < ( switch.time + tdelay ) & 
      times > switch.time
    if( all( values[window] < ylev ) ) {
      met = data.frame(ylev=ylev, value=switch.time )
      x$metrics$breakout = met
      return( x )
    }
    if( is.na(s) ) {
      warning("Data never drops below (or rises above) 'ylev' 
              for at least 'tdelay' time")
      x$metrics$breakout = data.frame( ylev=ylev, value=NA )
      return( x )
    }
    
  }
}





