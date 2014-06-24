#' Plotting well objects
#' 
#' A plotting function for \code{wellList} objects.
#' 
#' This sets up a \code{ggplot} object with the data from 
#' well objects. Additional aesthetics can be set using the
#' \code{...} arguments. Acceptable aesthetics are
#' \code{"color"}, \code{"linetype"}, \code{"size"}, \code{"alpha"},
#' \code{"shape"}, or {"fill"}. Their values must be one 
#' \code{"location"}, \code{"file"}, \code{"volume"}, \code{"compound"},
#' \code{"concentration"}, \code{"solvent"}, or \code{"solvent.percentages"}.
#' These aesthetics will be the names of the \code{...} arguments.
#' The values of these arguments will be passed to \code{group()} which
#' will then group the wells based on different parameters (e.g., \code{color="concentration"}
#' will color data from wells depending on their concentrations).
#' 
#' Diagnostic plots are made by setting \code{diagnostic} to a number.
#' For example, \code{diagnostic=2} will make the second well in the 
#' \code{wellList} near opaque while all the other wells will be
#' made more transparent. Text annotation will also be added to the plot
#' to indicate the indices of the data points as well as the times that
#' are on the x-axis. The tile of the plot will give all the information
#' for the well of interest.
#' 
#' @import ggplot2
#' @param x a \code{wellList} object
#' @param ... aesthetics to add to plot
#' @param diagnostic a numeric indicated which well to highlight
#' @param xlim two-element numeric for what slice of the data to plot. 
#'          The default is to plot all data.
#' @param points add data points to the plot with \code{geom_point}
#' @param discrete aesthetics that could be continuous (e.g., concentration) are made discrete
#' @param replicates average wells with same solutions 
#'          with interpolation and their interpolating splines
#' @param sd show a ribbon plot with \code{geom_ribbon} of each replicate
#'           group's standard deviation
#' @param ID the action ID passed to all other annotation functions. Wells will
#'           be determined to be replicates based on the solutions in this action's
#'           solution
#' @param spline \code{logical} for if to show the smoother function (\code{TRUE}) in the well or
#'           just to connect the data points with straight lines (\code{FALSE}).
#' @param line \code{logical} for if to connect data points with lines
#' @param nbw the Number of points BetWeen each data point to plot so that the
#'           smoother functions can be seen
#' @param min.dx related to \code{nbw}. This is the minimum spaced allowed between
#'           points when adding interpolating points in between data points.
#' @param smoother \code{logical} if to show smoother
#' @param deriv \code{numeric} which order derivative to plot
#' @param compress \code{logical}, wheter to run \code{compress_data} or not
#' @export

# Code for quickly set all of the values when debugging
# diagnostic=NULL; xlim=NULL; points=FALSE; discrete=TRUE; replicates=FALSE; sd=replicates
# spline=FALSE; line=!spline; args=list(color="concentration")
# nbw=5; min.dx=NULL; deriv=0; smoother=FALSE; compress=TRUE
plot.wellList = function( x, ..., diagnostic=NULL, xlim=NULL, 
                          points=FALSE, discrete=TRUE, replicates=FALSE, sd=replicates,
                          ID="last",
                          spline=FALSE, line=NULL, nbw=5, min.dx=NULL,
                          smoother=FALSE, deriv=0, compress=TRUE) {
  
  if( deriv>0 ) smoother = TRUE
  if( is.null(line) & (spline | smoother)) line = FALSE
  if( is.null(line) ) line = TRUE
  if(!is.null(xlim)) x = slice(x, xlim=xlim)
  if(compress) x = compress_data(x)
  if(replicates) x = average_replicates(x, ID=ID)
  
  args = list(...)
  args = highlight_well( x, args, diagnostic )
  args = arg_defaults( args )
  args$ID = ID
  
  # set NA filenames to ""
  filename(x)[ is.na(filename(x)) ] = ""
  
  # Set up the aesthetics from possible groupings of wells
  maes = do.call( well_aes, c(list(x),args) )
  maes$group = quote(interaction(file,location))
  
  # Line, spline, and smoothers
  line = if(line) geom_line() else NULL
  spline = add_spline_to_plot( x, nbw=nbw, min.dx=min.dx, 
                               spline=spline, discrete=discrete, maes=maes, args=args, deriv=deriv)
  gg.smoother = add_smoother_to_plot( x, nbw=nbw, min.dx=min.dx, 
                                     smoother=smoother, discrete=discrete, maes=maes, args=args, deriv=deriv)
  
  # If the derivative is requested, then it wouldn't make sense to show
  # the original data. The data must be changed to the derivative
  if(deriv>0 && smoother) x = insert_n_between_spline(x=x,deriv=deriv,type="smoother",n=0)
  
  # Melt the data into ggplot format
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
  base.plot = base.plot + line + spline + gg.smoother + points + ribbon + title
  text = diagnostic_lines(x, base.plot, diagnostic)
    
  return(base.plot + text)
}


#' Plot one well object
#' 
#' To plot one well object, make it a wellList and call
#' \code{plot.wellList}.
#' 
#' @param x a \code{well} object
#' @param ... passed to \code{plot.wellList}
#' @export
plot.well = function(x, ...) {
  plot( structure( list(x), class=c("wellList","list") ), ...)
}


#' Add spline to plot
#' 
#' Add a spline to a plot of \code{well} objects with \code{geom_line} and the
#' spline functions saved within each well. Returns a \code{geom_line} layer
#' with it's own data.
#' 
#' @param x a \code{wellList} object
#' @param nbw the Number of points BetWeen data points to interpolate
#' @param min.dx related to nbw, the minimum space between points that are interpolated
#' @param spline \code{logical} if to interpolated data or \code{NULL}
#' @param discrete \code{logical} make continuous data discrete
#' @param maes the aes object from \code{ggplot2} to be used in the final plot
#' @param args the arguments from the parent \code{plot.wellList} function
#' @param deriv the order derivative to plot
add_spline_to_plot = function( x, nbw, min.dx=NULL, 
                               spline, discrete, maes, args, deriv) {
  if(!spline) return(NULL)
  newx = insert_n_between_spline( x, n=nbw, min.dx=min.dx, deriv=deriv)
  data.spline = do.call( melt_wellList_params, c(list(newx),args) )
  if(discrete) make_discrete( data.spline, maes)
  geom_line(data=data.spline)
}


#' Add smoother to plot
#' 
#' Add a smoother to a plot of \code{well} objects with \code{geom_line} and the
#' smoother functions saved within each well. Returns a \code{geom_line} layer
#' with it's own data.
#' 
#' @param x a \code{wellList} object
#' @param nbw the Number of points BetWeen data points to interpolate
#' @param min.dx related to nbw, the minimum space between points that are interpolated
#' @param smoother \code{logical} if to interpolated data or \code{NULL}
#' @param discrete \code{logical} make continuous data discrete
#' @param maes the aes object from \code{ggplot2} to be used in the final plot
#' @param args the arguments from the parent \code{plot.wellList} function
#' @param deriv the order derivative to plot
add_smoother_to_plot = function( x, nbw, min.dx=NULL, 
                                 smoother, discrete, maes, args, deriv) {
  if(!smoother) return(NULL)
  newx = insert_n_between_spline( x, n=nbw, min.dx=min.dx, type="smoother", deriv=deriv)
  data.smoother = do.call( melt_wellList_params, c(list(newx),args) )
  if(discrete) make_discrete( data.smoother, maes)
  geom_line(data=data.smoother)
}


#' Add standard deviation ribbon
#' 
#' Make a \code{geom_ribbon} layer for \code{plot.wellList}
#' 
#' @param maes the \code{aes} object made inside \code{plot.wellList}
#' @param a \code{logical} if a \code{geom_ribbon} should be returned or not
sd_ribbon = function(maes, sd) {
  if(!sd) return(NULL)
  
  ribbon.aes = aes(ymin=value-sd,ymax=value+sd)
  if( "colour" %in% names(maes) )
    ribbon.aes$fill = maes$colour
  
  geom_ribbon(ribbon.aes, alpha=0.2, colour=NA)
}


#' Make continous columns discrete
#' 
#' Make some columns of a \code{data.table} discrete
#' 
#' Note that this takes as input a \code{data.table}, not 
#' \code{data.frame}. Thus, the \code{data.table} is edited
#' in place and does not need to be returned as is usual in R.
#' 
#' @param x a \code{data.table}
#' @param ast an \code{aes} object from \code{ggplot2}
#' @param discrete.params the names of columns which are allowed to be made discrete
#' @import data.table
make_discrete = function( x, ast, discrete.params = c("colour","fill","size")) {
  
  ast.str = as.character(ast)
  discrete.vars = ast.str[ names(ast.str) %in% discrete.params ]
  for( dvar in discrete.vars ) set(x, i=NULL, dvar, factor(x[[dvar]]))
  return(TRUE)
}


#' Show data point indices on plot
#' 
#' Add vertical lines to a plot to show the index
#' of a handful of points. At the thop of these lines
#' a the index (a number) is displayed. The lines reach
#' from the top of the plot down to the data curve.
#' 
#' @param x a \code{wellList} object
#' @param p a \code{ggplot} object to which the lines will be added
#' @param diagnostic a \code{numeric} indicating which well in \code{x} will
#'            be used to make the indices and lines
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

#' Highlight one curve in a plot
#' 
#' Make one curve close to opaque and all other
#' curves more transparent. This simply changes teh
#' \code{args} function in \code{plot.wellList}. It 
#' is very much an internal function.
#' 
#' @param x a \code{wellList} object
#' @param args the list of arguments in the \code{args} variable in
#'           \code{plot.wellList}
#' @param diagnostic a \code{numeric} indicating which well is to be highlighted
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

#' Add title to well plot
#' 
#' Adds a title to a well plot if there is only one
#' well that is being plotted. Adds the filename, 
#' location, and concentration to the top. This is very
#' much an internal function.
#' 
#' @param x a \code{wellList} object
#' @param args the \code{args} variable in the \code{plot.wellList function}
#' @param diagnostic a \code{numeric} for which well to base the title on 
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


# Internal function for plot.wellList. This changes
# the arguments from the ... commands. This is pretty much
# a hack that would need to be fixed with some refactoring.
arg_defaults = function(args) {
  if(is.null(args$type)) args$type = "start"
  args
}





