
# Average wells with the same solutions as determined
# by their compound concentrations, solvent percentages,
# and volume.
# The averaged wells contains the means of all replicates
# interpolated to one the same time points. The
# standard deviation is also saved in the data frame
average_replicates = function(x) {
  # group the wells by their solutions to find repilcates
  welldescrip = interaction( concentration(x, type="all", ID="last"),
                             solvent_percentages(x, ID="last"),
                             volume(x,ID="last") )
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
    
    interp = function(x) x$spline( new.x[[i]]$data$t )
    value.list[2:length(groups[[i]])] = lapply( x[other.wells], interp )
    
    value.table = as.data.table(value.list)
    new.x[[i]]$data$value = rowMeans(value.table)
    new.x[[i]]$data$sd = rowSD(value.table)
    
    new.x[[i]]$code = paste( code(x[groups[[i]]]), collapse="+")
  }
  new.x
}


###### Finally create a generic function for the transformations
# methods is either a list of functions (e.g, created one created with `c()`)
# or a single function
transform.well = function(x, methods, ...) {
  if(length(methods)==1) return( methods(x,...) )
  for( m in methods ) x = m(x, ...)
  return(x)
}
transform.wellList = transform.well

######## Time centering
tcenter = function(x,...) UseMethod("tcenter",x)
tcenter.well = function(x, ID="last", ...) {
  centered.time = ID_t(x, ID=ID, ...)
  tdata(x) = tdata(x) - centered.time
  x
}
tcenter.wellList = function(x, ...) {
  x = lapply(x, tcenter, ...)
  class(x) = c("wellList","list")
  x
}

######## Normalize to a point in time
normalize = function(x, ...) UseMethod("normalize", x)
normalize.well = function(x, ID="last", ...) {
  norm.val = ID_v(x, ID=ID)
  vdata(x) = vdata(x)/norm.val
  x
}
normalize.wellList = function(x, ...) {
  x = lapply(x, normalize, ...)
  class(x) = c("wellList","list")
  x
}

####### Slice a piece of time
slice = function(x, ...) UseMethod("slice", x)
slice.well = function(x, xlim, ...) {
  wdata(x) = wdata(x)[ tdata(x) > xlim[1] & tdata(x) < xlim[2], ]
  x
}
slice.wellList = function(x, ...) {
  x = lapply(x, slice, ...)
  class(x) = c("wellList","list")
  x
}

###### Level to a point in time
level = function(x, ...) UseMethod("level", x)
level.well = function(x, ID="last", ...) {
  norm.val = ID_v(x, ID=ID)
  vdata(x) = vdata(x) - norm.val
  x
}
level.wellList = function(x, ...) {
  x = lapply(x, level, ...)
  class(x) = c("wellList","list")
  x
}








