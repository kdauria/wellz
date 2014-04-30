
################################################################################
#                             ggplot type plots                                #
################################################################################

# plot data in wellList
# ... are additional factors that can be made
plot.wellList = function( wells, ID="final", xvar="time",
                          showpoints=FALSE, manip=FALSE,
                          alpha=NULL, color=NULL, size=NULL, linetype=NULL,
                          shape=NULL, colour=NULL, 
                          spline=FALSE, nbw=3, min.diff=NULL, deriv=0,
                          xlim=NULL, diagnostic=NULL, replicates=FALSE,
                          smoother=FALSE, se=TRUE, ... ) {
  
  # figure out which groupings will need to be done, and melt data
  if( !is.null(color) & !is.null(colour) ) stop("color and colour defined")
  if( is.null(color) & !is.null(colour) ) color = colour
  groupings = c(alpha=alpha,colour=color,size=size,
                linetype=linetype,shape=shape)
  if(is.null(groupings)) groupings = c(colour="by.well",linetype="by.file")
  groupings = c(groupings, unlist(list(...))) # factors for other ggplot2 calls (eg facet_wrap)
  
  # build the base plot
  x = build.wellPlot( wells, groupings, xvar, nbw, deriv, 
                      showpoints, replicates, spline, ID, smoother, min.diff, xlim, se )
  p = x$plot
  lwells = x$lines
  
  # Make the plot diagnostic if requested
  if( !is.null(diagnostic) ) {
    
    # Force x-axis to be time
    if(!is.null(alpha)) stop("Cannot do diagnostic plot when alpha is specified")
    xvar = "time"
    p$mapping$x = as.symbol(xvar)
    for(i in seq_along(p$layers)) p$layers[[i]]$mapping$x = as.symbol(xvar)
    
    dwell = lwells[[diagnostic]]
    p = highlightWell( p, dwell, ID ) 
  }
  
  # add the data specific for this type of plot
  p$wellOptions = c( as.list(groupings), 
                     list( ID=ID, xvar=xvar,
                           showpoints=showpoints, manip=manip, 
                           deriv=deriv, nbw=nbw, spline=spline, xlim=xlim,
                           diagnostic=diagnostic, replicates=replicates, ID=ID,
                           smoother=smoother, min.diff=min.diff, se=se) )
  
  # the final calls showing the plot
  if(manip) {
    r = range(p$data[,xvar])
    n.panels = 1 + showpoints + replicates
    manipulate( { if(length(p$layers)>n.panels) p$layers = p$layers[1:n.panels]
                  p = p + xlim(xmin,xmax)
                  if(!is.null(diagnostic)) p = add.text.times(p,dwell)
                  p },
                xmax = slider(r[1],r[2], ifelse( is.null(xlim), r[2], xlim[2] )),
                xmin = slider(r[1],r[2], ifelse( is.null(xlim), r[1], xlim[1] )) )
    invisible(p)
  } else {
    if( !is.null(xlim) ) p = p + xlim(xlim)
    if(!is.null(diagnostic)) p = add.text.times(p,dwell)
    return(p)
  }
}

# a function to highlight a well by making all others more transparent
highlightWell = function( p, dwell, ID ) {
  
  # Find which well is to be highlighted in the diagnostic plot
  fwh = data.frame( file=getfiles(dwell), well=getlocations(dwell) )
  
  # change the alpha for each layer
  for( i in seq_along(p$layers) ) {
    l = p$layers[[i]]
    if( class(l$data) != "waiver" ) {
      fw = l$data[,c("file","well")]
      idx = fw$file==fwh$file & fw$well==fwh$well
      p$layers[[i]]$data$alpha = c(0.15,1)[ idx + 1 ]
      p$layers[[i]]$mapping$alpha = as.symbol("alpha")
    } else {
      fw = p$data[,c("file","well")]
      idx = fw$file==fwh$file & fw$well==fwh$well
      p$data$alpha = c(0.15,1)[ idx + 1 ]
      p$mapping$alpha = as.symbol("alpha")
    }
  }
  
  # add the title
  sumry = status(getrows.by.ID(dwell,ID))$status
  file = getfiles(dwell)
  loc = getlocations(dwell)
  p = p + ggtitle(str_c(file,": ",loc," -- ",sumry)) + scale_alpha(guide="none",limits=c(0,1))
  
  invisible(p)
}

# The core function of plotting data from wells. 
build.wellPlot = function( wells, groupings, xvar, nbw, deriv, 
                           showpoints, replicates, spline, ID,
                           smoother, min.diff, xlim, se ) {
  
  # setup of the base plot
  p = ggplot( data=data.frame(), aes_string(x=xvar)) + geom_line(aes_string(y="value"))
  
  # get data for line (or spline if the spline is requested). Derivative if requested too.
  if( smoother || spline || deriv>0 ) {
    wells.line = spline_interpolate( wells, nbw=nbw, deriv=deriv, smooth=smoother, min.diff=min.diff )
  } else {
    wells.line = wells
  }
  
  # add the data to the ggplot
  if( replicates ) wells.line = averageReplicates( wells.line, ID )
  
  # Subset the data if necessary
  if(!is.null(xlim)) {
    wells = transform(wells,"slice",xlim=xlim)
    wells.line = transform(wells.line,"slice",xlim=xlim)
  }
  
  # Add the data to the plot
  p$data = melt.wellList.wgroups( wells.line, groupings, ID )
  
  # show the points if requested
  if( showpoints ) {
    if( deriv > 0 ) {
      wells.points = spline_interpolate( wells, nbw=0, deriv=deriv, 
                                         smooth=smoother, min.diff=min.diff )
    } else {
      wells.points = wells
    }
    if( replicates ) wells.points = averageReplicates( wells.points, ID )
    y = melt.wellList.wgroups( wells.points, groupings, ID )
    p = p + geom_point(data=y,aes(y=value))
  } else {
    wells.points = NULL
  }
  
  if(replicates && se) {
      p = p + geom_ribbon(aes(ymin=value-sds,ymax=value+sds),alpha=0.2) 
  }
  
  # fix all of the aesthetics for each layer separately
  for( i in 1:length(p$layers) ) {
    p$layers[[i]]$mapping$y = as.symbol("value")
    p$layers[[i]]$mapping$group = substitute(interaction(well,file))
    for( nm in names(groupings) ) p$layers[[i]]$mapping[[nm]] = as.symbol(nm)
    if( p$layers[[i]]$geom$objname=="ribbon" & ("colour" %in% names(groupings)) ) {
      p$layers[[i]]$mapping$fill = as.symbol("colour")
      p$layers[[i]]$mapping$colour = NULL
    }
  }
  invisible(list(lines=wells.line, points=wells.points, plot=p))
}

# A very specialized function that adds the sweep numbers to the top of the plot corresponding
# to the times on the x-axis (the minor ticks of the x-axis)
add.text.times = function(p,dwell) {
  
  # the additional labels for the x-axis
  times = timesdata(dwell)
  sweeps = sweepdata(dwell)
  
  # find which sweep is closest to minor ticks
  aa = ggplot_build(p)
  majorx.times = aa$panel$ranges[[1]]$x.minor_source
  foo = function(x) which.min(abs(x - times))
  sweep.idxs = vapply(majorx.times,foo,1)
  
  # make sure that the sweep number isn't NA
  sn = na.omit(sweeps)
  for( i in seq_along(sweep.idxs) ) {
    id = sweep.idxs[i]
    sweepnum = sn[ which.min( abs(which(!is.na(sweeps))-id ) ) ]
    sweep.idxs[i] = which( sweeps==sweepnum )
  }
  
  # the sweep numbers as text on the plot
  xloc = times[sweep.idxs]
  max.y = aa$panel$y_scales[[1]]$range$range[2]
  textdf = data.frame(x=xloc,y=max.y,label=as.character(sweeps[sweep.idxs]))
  l1 = geom_text(mapping=aes(x=x,y=y,label=label),data=textdf,alpha=1)
  
  # vertial lines pointing to the sweep numbers
  yline = welldata(dwell)[sweep.idxs]
  linedf = data.frame(x=xloc,ymin=yline*1.02,ymax=max.y*0.98)
  l2 = geom_linerange(data=linedf,aes(x=x,ymin=ymin,ymax=ymax),alpha=1)
  
  return(p+l1+l2)
}

# plot a single well
plot.Well = function(x,...) {
  y = list(x)
  class(y) = c("wellList","list")
  plot(y,...)
}

# redo an old plot (all the old options) with new data
# e.g., oldplot %+d% newwellList
`%+d%` = function( p, newwells ) {
  
  # reset the x-limits if the ranges of old and new wells don't match
  newrange = range(unlist(timesdata(newwells)))
  oldrange = range(p$data$time)
  if(!is.null(p$wellOptions$xlim)) oldrange = p$wellOptions$xlim  
  
  if( newrange[1]>oldrange[1] && newrange[2]<oldrange[2] ) {
    p$wellOptions$xlim = NULL
    message("xlim reset because the data has been sliced")
  } else if( !identical(newrange,range(p$data$time))) {
    p$wellOptions$xlim = NULL
    message("xlim reset because the time data has been edited since the last plotting")
  }
  
  # recall the plot function with the new wells
  inputs = append( list(wells=newwells), p$wellOptions )
  do.call( plot, inputs )
}

# a hermite cubic "smoother" function for ggplot2.
# This is not really smoothing, it's for interpolation
# nbw is the number of points to put in between the x data points for
# better visualization of the spline function
smooth.pchip <- function(formula, data, weights, nbw=0, fun=NULL, ... ) {
  mat = model.frame(formula, data)
  out = list()
  if(is.function(fun)){
    out$fun = fun
  } else {
    out$fun = splinefun(x=mat[, 2],y=mat[, 1],method="monoH.FC")
  }
  out$nbw = nbw
  out$x = mat[, 2]
  class(out) = "smooth.pchip"
  out
}
predictdf.smooth.pchip <- function(model, xseq, se, level) {
  fun = model$fun
  newx = insert.nbw( model$x, model$nbw )
  data.frame(x = newx, y = fun(newx) )
}

################################################################################
#                       Reshape functions for ggplot                           #
################################################################################

# Melt data from multiple wells of a wellList into one matrix
melted.wellList = function( wells ) {
  
  datas = datamat(wells)
  x = melt(datas,measure.vars="values")
  x$well = getlocations(wells)[ x$L1 ]
  x$file = getfiles(wells)[ x$L1 ]
  x$variable = x$L1 = NULL
  if(is.null(x$sds)) x$sds = NA
  x = x[,c("file","well","time","sweep","value","sds")]
  return(x)
}

# melt a wellList, adding groupings using the groupWells function
melt.wellList.wgroups = function( wells, groupings, ID, ... ) {
  
  # combine all the data into one melted data matrix (think "reshape" package)
  x = melted.wellList(wells)
  id0 = rle(paste(x$file,x$well))
  idx = rep( 1:length(id0$lengths), times=id0$lengths )
  
  # make a matrix for the additional characteristics
  gf = matrix(NA,nrow=length(idx),ncol=length(groupings))
  colnames(gf) = names(groupings)
  
  # get the factors for each plot characteristic
  if( !is.null(groupings) ) {
    for( i in 1:length(groupings) ) {
      grouping = groupings[i]
      gf[,i] = groupWells( wells, grouping, ID, ... )[idx]
    }
  }
  
  return( data.frame(x,gf) )
}
