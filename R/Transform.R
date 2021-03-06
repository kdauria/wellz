#' Average replicate wells
#' 
#' Wells are considered replicates if their solutions
#' in the action specified by \code{ID} are the same.
#' In other words, replicates will have the same volume,
#' compounds and concentrations, and solvent composition.
#' Replicates are merged by concatenating their location
#' strings with a "+". Their data are merged using one
#' of the well's spline functions. A new \code{wellList}
#' object is returned. For instance, if there are 6 wells
#' where the are three duplicate pairs, then the output
#' will contain three wells.
#' 
#' The standard deviation is saved as column \code{sd} in the data
#' matrix of the resulting wells. \code{NA} values are removed
#' before the means and standard deviations are calculated.
#' 
#' @import plyr
#' @import data.table
#' @param x a \code{wellList} object
#' @param ID which action to refer to
#' @export
average_replicates = function(x, ID="last") {
  # group the wells by their solutions to find repilcates
  welldescrip = interaction( concentration(x, type="all", ID=ID),
                             solvent_percentages(x, ID=ID),
                             volume(x,ID=ID) )
  group.ids = as.numeric(welldescrip)
  
  # Find the reference well for each group and make a new wellList
  num.rows = sapply(x, function(y) nrow(y$data)) # the statistic to choose with
  stats = data.frame( ids=1:length(x), stat=num.rows, group=group.ids )
  refwells = c(daply(stats,"group",function(x) x$ids[which.max(x$stat)]))
  new.x = x[refwells]
  
  # Now do interpolation of other wells onto the reference wells
  groups = split(1:length(group.ids), group.ids)
  for( i in seq_along(new.x) ) {
    
    other.wells = setdiff( groups[[i]], refwells[i])
    if(length(other.wells)==0) {
      new.x[[i]]$data$sd = rep(0,nrow(new.x[[i]]$data))
      next
    }
    
    value.list = vector(mode="list",length=length(groups[[i]]))
    value.list[[1]] = new.x[[i]]$data$value
    
    interp = function(x) {
      if( length(x$spline)==0 ) stop(well_key(x), " has no spline")
      x$spline( tdata(new.x[[i]]) )
    }
    value.list[2:length(groups[[i]])] = lapply( x[other.wells], interp )
    
    value.table = as.data.table(value.list)
    new.x[[i]]$data$value = rowMeans(value.table, na.rm=TRUE)
    new.x[[i]]$data$sd = rowSD(value.table, na.rm=TRUE)
    
    new.x[[i]]$code = paste( code(x[groups[[i]]]), collapse="+")
  }
  x = add_spline(x)
  new.x
}

#' general data transformation function
#' 
#' This is mainly a wrapper for several other transformation functions.
#' This function allows many to be called at once using the 
#' \code{methods} argument. 
#' 
#' This \code{methods} argument may be a character vector
#' or a list of functions. The strings can be 
#' \code{"tcenter"}, \code{"normalize"}, \code{"slice"},
#' or \code{"level"}.
#' 
#' @param x a \code{well} object
#' @param methods a character vector or list of functions
#' @export
transform.well = function(x, methods, ...) {
  if(length(methods)==1) return( do.call( methods, list(x,...) ) )
  for( m in methods ) x = do.call( m, list(x,...) )
  return(x)
}
#' @export
transform.wellList = transform.well

#' Time-center the data
#' 
#' Center the data at a point identified by the action
#' named \code{ID}. Return a well list with the time-transformed
#' data.
#' 
#' @param x a \code{well} or \code{wellList} object
#' @param ... the \code{ID} can be set
#' @export
tcenter = function(x,...) UseMethod("tcenter",x)
#' @export
tcenter.well = function(x, ID="last", ...) {
  if(!(ID %in% c("last",ID(x)))) {
    warning(paste("ID", ID,"not in well. Returning NULL"))
    return(NULL)
  }
  centered.time = ID_t(x, ID=ID, ...)
  tdata(x) = tdata(x) - centered.time
  x = add_spline(x)
  x
}
#' @export
tcenter.wellList = function(x, ...) {
  x = lapply(x, tcenter, ...)
  x = x[ !sapply(x,is.null) ]
  class(x) = c("wellList","list")
  x
}

#' Normalize well data
#' 
#' Normalize the data in a well to the time
#' of some action named \code{ID}.
#' 
#' @param x a \code{well} or \code{wellList} object
#' @param ... the \code{ID} can be set
#' @export
normalize = function(x, ...) UseMethod("normalize", x)
#' @export
normalize.well = function(x, ID="last", ...) {
  if( !(ID %in% ID(x)) ) {
    warning("ID",ID,"not in well. Returning NULL")
    return(NULL)
  }
  norm.val = ID_v(x, ID=ID)
  vdata(x) = vdata(x)/norm.val
  x = add_spline(x)
  x
}
#' @export
normalize.wellList = function(x, ...) {
  x = lapply(x, normalize, ...)
  x = x[ !sapply(x,is.null) ]
  class(x) = c("wellList","list")
  x
}


#' Slice in time of well data
#' 
#' Only keep data that is within the bounds
#' of \code{xlim}, a two-element numeric vector.
#' 
#' @param x a \code{well} or \code{wellList} object
#' @param ... ignored
#' @export
slice = function(x, ...) UseMethod("slice", x)
#' @export
slice.well = function(x, xlim, ...) {
  wdata(x) = wdata(x)[ tdata(x) > xlim[1] & tdata(x) < xlim[2], ]
  x = add_spline(x)
  x
}
#' @export
slice.wellList = function(x, ...) {
  x = lapply(x, slice, ...)
  x = x[ !sapply(x,is.null) ]
  class(x) = c("wellList","list")
  x
}

#' Level well data
#' 
#' "Level" the data in a well to the time
#' of some action named \code{ID}. In other words,
#' all other data values will be in reference to the difference
#' to that point in time.
#' 
#' @param x a \code{well} or \code{wellList} object
#' @param ... the \code{ID} can be set
#' @export
level = function(x, ...) UseMethod("level", x)
#' @export
level.well = function(x, ID="last", ...) {
  if( !(ID %in% ID(x)) ) {
    warning("ID",ID,"not in well. Returning NULL")
    return(NULL)
  }
  norm.val = ID_v(x, ID=ID)
  vdata(x) = vdata(x) - norm.val
  x = add_spline(x)
  x
}
#' @export
level.wellList = function(x, ...) {
  x = lapply(x, level, ...)
  x = x[ !sapply(x,is.null) ]
  class(x) = c("wellList","list")
  x
}








