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
#' @param x a \code{wellList} object
#' @param ID which action to refer to
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
    new.x[[i]]$data$value = rowMeans(value.table)
    new.x[[i]]$data$sd = rowSD(value.table)
    
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
transform.well = function(x, methods, ...) {
  if(length(methods)==1) return( do.call( methods, list(x,...) ) )
  for( m in methods ) x = do.call( m, list(x,...) )
  return(x)
}
transform.wellList = transform.well

#' Time-center the data
#' 
#' Center the data at a point identified by the action
#' named \code{ID}. Return a well list with the time-transformed
#' data.
#' 
#' @param x a \code{well} or \code{wellList} object
#' @param ... the \code{ID} can be set
tcenter = function(x,...) UseMethod("tcenter",x)
tcenter.well = function(x, ID="last", ...) {
  centered.time = ID_t(x, ID=ID, ...)
  tdata(x) = tdata(x) - centered.time
  x = add_spline(x)
  x
}
tcenter.wellList = function(x, ...) {
  x = lapply(x, tcenter, ...)
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
normalize = function(x, ...) UseMethod("normalize", x)
normalize.well = function(x, ID="last", ...) {
  norm.val = ID_v(x, ID=ID)
  vdata(x) = vdata(x)/norm.val
  x = add_spline(x)
  x
}
normalize.wellList = function(x, ...) {
  x = lapply(x, normalize, ...)
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
slice = function(x, ...) UseMethod("slice", x)
slice.well = function(x, xlim, ...) {
  wdata(x) = wdata(x)[ tdata(x) > xlim[1] & tdata(x) < xlim[2], ]
  x = add_spline(x)
  x
}
slice.wellList = function(x, ...) {
  x = lapply(x, slice, ...)
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
level = function(x, ...) UseMethod("level", x)
level.well = function(x, ID="last", ...) {
  norm.val = ID_v(x, ID=ID)
  vdata(x) = vdata(x) - norm.val
  x = add_spline(x)
  x
}
level.wellList = function(x, ...) {
  x = lapply(x, level, ...)
  class(x) = c("wellList","list")
  x
}








