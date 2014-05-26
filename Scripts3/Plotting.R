
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

# The ... possibilities are:
#  type, ID, compound, solvent AND plot parameters
plot.wellList = function( x, ..., diagnostic=NULL ) {
  
  args = list(...)
  args = highlight_well( args, diagnostic )
  args = arg_defaults( args )
  
  maes = do.call( well_aes, c(list(x),args) )
  maes$group = quote(interaction(file,location))
  
  dat = do.call( melt_wellList_params, c(list(x),args) )
  
  base.plot = ggplot(dat, maes) + geom_line()
  
  title = add_title( x, args, diagnostic )

  return(base.plot + title)
  
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
    fname = filename(x[[diagnostic]])
    loc = code(x[[diagnostic]])
    conc = concentration(x[[diagnostic]], ID=args$ID, type=args$type)
    title = ggtitle( paste(fname, loc, conc) )
  } else {
    title = ggtitle()
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





