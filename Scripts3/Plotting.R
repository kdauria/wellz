
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
# x = select(wells,file="HCT8.txt")[1:4]
# x = select(wells,file="HCT8.txt", ID="toxinAdd")
# x = select(wells,"TcdA", filename="CecalCells.txt", controls=TRUE)
# a = plot(x, color="concentration", diagnostic=1, ID="toxinAdd", type="all")
# a = plot(x, diagnostic=3, xlim=c(34,38), points=FALSE, color="concentration")
# args = list(color="concentration")
# points=FALSE
# xlim = c(34,38)
# discrete=TRUE
# diagnostic=3

# The ... possibilities are:
#  type, ID, compound, solvent AND plot parameters
plot.wellList = function( x, ..., diagnostic=NULL, xlim=NULL, points=FALSE, discrete=TRUE ) {
  
  args = list(...)
  args = highlight_well( args, diagnostic )
  args = arg_defaults( args )
  
  maes = do.call( well_aes, c(list(x),args) )
  maes$group = quote(interaction(file,location))
  
  data = do.call( melt_wellList_params, c(list(x),args) )
  if(!is.null(xlim)) data = data[ t<xlim[2] & t>xlim[1], ]

  base.plot = ggplot(data, maes) + geom_line()
  if(discrete) make_discrete( base.plot, maes )
  title = add_title( x, args, diagnostic )
  points = if(points) geom_point() else NULL
  text = if(!is.null(diagnostic)) diagnostic_lines(base.plot, diagnostic) else NULL
  
  return(base.plot + title + points + text)
  #return(data)
}

# Make some of the plot parameters discrete if they
# are continuous. This is so colors will be easier to distinguish
# instead of colors of the same hue but different intensity
make_discrete = function( p, ast, discrete.params = c("colour","fill","size")) {
  
  ast.str = as.character(ast)
  discrete.vars = ast.str[ names(ast.str) %in% discrete.params ]
  for( dvar in discrete.vars ) set(p$data,i=NULL, dvar, factor(p$data[[dvar]]))
  return(TRUE)
}

# Add lines and text showing the 'i' (index) of each
# data point from the original data
diagnostic_lines = function( p, diagnostic ) {
  
  # Find the location of ticks and the range of the plot
  g = ggplot_build(p)
  g.ylim = g$panel$ranges[[1]]$y.range
  ticks = g$panel$ranges[[1]]$x.minor_source
  
  # Find which 'i' are closest to the ticks
  well.info = p$data[,list(file,location), by="file,location"][diagnostic]
  p$data = p$data[well.info]
  ticks.i = vapply( ticks, function(x) which.min(abs(x-p$data$t)), 1)
  subdata = p$data[ticks.i,]
  
  out1 = geom_text(data=subdata, aes(x=t,label=i), y=g.ylim[2], alpha=1, vjust=1, color="gray50")
  subdata[ , ymin:=value+diff(g.ylim)*0.01 ]
  out2 = geom_linerange(data=subdata, aes(x=t,ymin=ymin), 
                        ymax=g.ylim[2]*0.95, alpha=1, color="gray50")
  list(out1, out2)
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





