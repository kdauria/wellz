
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
