
# Possible aesthetics

# for line plot
# color
# linetype
# size (linewidth)
# alpha

# for point plot
# color
# fill
# alpha
# shape
# size

# Note error here is the gray values that are because of NA values
# Should throw an error here and remove them from the plot
# That's not really possible actually. It should be done at the select stage
x = select(wells,file="HCT8.txt")[1:4]
x = select(wells,file="HCT8.txt", ID="toxinAdd")


ggplot(x, color="concentration", diagnostic=1) + geom_line()

a = plot(x, color="concentration", diagnostic=1, ID="toxinAdd", type="all")
a = plot(x, diagnostic=3, xlim=c(40,60))

# The ... possibilities are:
#  type, ID, compound, solvent AND plot parameters
plot.wellList = function( x, ..., diagnostic=NULL, xlim=NULL, points=FALSE, lines=TRUE ) {
  
  args = list(...)
  args = highlight_well( args, diagnostic )
  args = arg_defaults( args )
  
  maes = do.call( well_aes, c(list(x),args) )
  maes$group = quote(interaction(file,location))
  
  data = do.call( melt_wellList_params, c(list(x),args) )
  if(!is.null(xlim)) data = data[ t<xlim[2] & t>xlim[1], ]

  base.plot = ggplot(data, maes) + geom_line()
  title = add_title( x, args, diagnostic )
  points = if(points) geom_point() else NULL
  lines = if(lines) geom_line() else NULL
  text = if(!is.null(diagnostic)) NULL
  
  return(base.plot + title + lines + points)
  
}

diagnostic_lines = function( p, diagnostic ) {
  
  # Find the location of ticks and the range of the plot
  g = ggplot_build(p)
  ylim = g$panel$ranges[[1]]$y.range
  xlim = g$panel$ranges[[1]]$x.range
  ticks = g$panel$ranges[[1]]$x.minor_source
  
  # Find which 'i' are closest to the ticks
  well.info = p$data[,list(file,location), by="file,location"][diagnostic]
  p$data = p$data[well.info]
  ticks.i = vapply( ticks, function(x) which.min(abs(x-p$data$t)), 1)
  subdata = p$data[ticks.i,]
  
  out1 = annotation_custom( textGrob( subdata$i, x=(subdata$t - xlim[1])/diff(xlim), y=1, vjust=1),
                            xmin=xlim[1], xmax=xlim[2], ymin=ylim[1], ymax=ylim[2]*1.01)
  #out1 = geom_text(data=subdat, aes(x=t,label=i), y=ylim[2], alpha=1)
  out2 = geom_linerange(data=subdata, aes(x=t,ymin=value+diff(ylim)*0.01), ymax=ylim[2]*0.95, alpha=1)
  list(out1, out2)
}



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




# Checks to see if a well should be highlighted
# If it should, the plot arguments are changed
# so that all other curves will have transparency
highlight_well = function( args, diagnostic ) {
  if( !is.null(diagnostic)) {
    if("alpha" %in% names(args)) {
      stop("diagnostic doesn't work when alpha is already defined")
    }
    args$alpha  = rep(0.2, length(x))
    args$alpha[diagnostic] = 1
  }
  args
}

# How to add the title
add_title = function( x, args, diagnostic ) {
  if( !is.null(diagnostic) || length(x)==1 ) {
    if(length(x)==1) diagnostic=1
    fname = filename(x[[diagnostic]])
    loc = code(x[[diagnostic]])
    conc = concentration(x[[diagnostic]], ID=args$ID, type=args$type)
    title = ggtitle( paste(fname, loc, conc) )
  } else {
    title = NULL
  }
  return(title)
}


# Fix the defaults for ID and type used in the
# plot.wellList function
arg_defaults = function(args) {
  if(is.null(args$ID)) args$ID = "last"
  if(is.null(args$type)) args$type = "start"
  args
}





