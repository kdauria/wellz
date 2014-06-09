

# diagnostic=NULL; xlim=NULL; points=FALSE; discrete=TRUE; replicates=FALSE; sd=replicates
# spline=FALSE; line=!spline; args=list(color="concentration")
# nbw=5; min.dx=NULL
plot.wellList = function( x, ..., diagnostic=NULL, xlim=NULL, 
                          points=FALSE, discrete=TRUE, replicates=FALSE, sd=replicates,
                          spline=FALSE, line=NULL, nbw=5, min.dx=NULL,
                          smoother=FALSE, deriv=0) {
  
  if( deriv>0 ) smoother = TRUE
  if( is.null(line) & (spline | smoother)) line = FALSE
  if( is.null(line) ) line = TRUE
  if(!is.null(xlim)) x = slice(x, xlim=xlim)
  if(replicates) x = average_replicates(x)
  
  args = list(...)
  args = highlight_well( x, args, diagnostic )
  args = arg_defaults( args )
  
  # Set up the aesthetics from possible groupins of wells
  maes = do.call( well_aes, c(list(x),args) )
  maes$group = quote(interaction(file,location))
  
  # Line, spline, and smoothers
  line = if(line) geom_line() else NULL
  spline = add_spline_to_plot( x, nbw=nbw, min.dx=min.dx, 
                               spline=spline, discrete=discrete, maes=maes, args=args, deriv=deriv)
  gg.smoother = add_smoother_to_plot( x, nbw=nbw, min.dx=min.dx, 
                                     smoother=smoother, discrete=discrete, maes=maes, args=args, deriv=deriv)
  
  # Melt the data into ggplot format
  if(deriv>0 && smoother) x = insert_n_between_spline(x=x,deriv=deriv,type="smoother",n=0)
  data = do.call( melt_wellList_params, c(list(x),args) )

  # Base plot and the data
  base.plot = ggplot(data, maes)
  if(discrete) make_discrete( data, maes )
  
  # Points if requested
  points = if(points) geom_point() else NULL
  
  # Decoration
  title = add_title( x, args, diagnostic )
  ribbon = sd_ribbon(maes, sd)
  
  # Add diagnostic annotations if requested
  base.plot$data = data
  text = diagnostic_lines(x, base.plot+ribbon, diagnostic)
    
  return(base.plot + title + points + text + ribbon + line + spline + gg.smoother)
}

plot.well = function(x, ...) {
  plot( structure( list(x), class=c("wellList","list") ), ...)
}


# Add a spline that interpolates values between points
add_spline_to_plot = function( x, nbw, min.dx=NULL, 
                               spline, discrete, maes, args, deriv) {
  if(!spline) return(NULL)
  newx = insert_n_between_spline( x, n=nbw, min.dx=min.dx, deriv=deriv)
  data.spline = do.call( melt_wellList_params, c(list(newx),args) )
  if(discrete) make_discrete( data.spline, maes)
  geom_line(data=data.spline)
}
add_smoother_to_plot = function( x, nbw, min.dx=NULL, 
                                 smoother, discrete, maes, args, deriv) {
  if(!smoother) return(NULL)
  newx = insert_n_between_spline( x, n=nbw, min.dx=min.dx, type="smoother", deriv=deriv)
  data.smoother = do.call( melt_wellList_params, c(list(newx),args) )
  if(discrete) make_discrete( data.smoother, maes)
  geom_line(data=data.smoother)
}

# Whether or not to show errors from the replicate wells
sd_ribbon = function(maes, sd) {
  if(!sd) return(NULL)
  
  ribbon.aes = aes(ymin=value-sd,ymax=value+sd)
  if( "colour" %in% names(maes) )
    ribbon.aes$fill = maes$colour
  
  geom_ribbon(ribbon.aes, alpha=0.2, colour=NA)
}

# Make some of the plot parameters discrete if they
# are continuous. This is so colors will be easier to distinguish
# instead of colors of the same hue but different intensity
make_discrete = function( x, ast, discrete.params = c("colour","fill","size")) {
  
  ast.str = as.character(ast)
  discrete.vars = ast.str[ names(ast.str) %in% discrete.params ]
  for( dvar in discrete.vars ) set(x, i=NULL, dvar, factor(x[[dvar]]))
  return(TRUE)
}

# Add lines and text showing the 'i' (index) of each
# data point from the original data
diagnostic_lines = function( x, p, diagnostic ) {
  
  if(is.null(diagnostic)) return(NULL)
  
  # Find the location of ticks and the range of the plot
  g = ggplot_build(p)
  g.ylim = g$panel$ranges[[1]]$y.range
  ticks = g$panel$ranges[[1]]$x.minor_source
  
  # Find which 'i' are closest to the ticks
  well.info = data.table( file=filename(x[[diagnostic]]), location=code(x[[diagnostic]]))
  p$data = p$data[well.info] # a data.table JOIN
  ticks.i = vapply( ticks, function(x) which.min(abs(x-p$data$t)), 1)
  subdata = p$data[ticks.i,]
  
  out1 = geom_text(data=subdata, aes(x=t,label=i), y=g.ylim[2], alpha=1, vjust=1, color="gray50")
  subdata[ , ymin:=value+diff(g.ylim)*0.01 ]
  out2 = geom_linerange(data=subdata, aes(x=t,ymin=ymin), 
                        ymax=g.ylim[2]*0.95, alpha=1, color="gray50")
  out3 = scale_alpha(guide = 'none')
  list(out1, out2, out3)
}

# Checks to see if a well should be highlighted
# If it should, the plot arguments are changed
# so that all other curves will have transparency
highlight_well = function( x, args, diagnostic ) {
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




