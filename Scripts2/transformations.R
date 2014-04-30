################################################################################
#                      Data transformation functions                           #
################################################################################

# wrapper function for different types of transformations
transform.Well = function( x, method, ... ) {
  
  tdata = x
  for( i in seq_along(method)) {
    tdata = do.call( method[i], c( alist(tdata) , alist(...) ) )
    tdata = tdata[!vapply(tdata,is.null,TRUE)]
    class(tdata) = class(x)
  }
  out = add_interpolating_spline(tdata)
  return(out)
}
transform.wellList = transform.Well

# Normalize the data to a certain point in time by division
normalize = function(x,ID,...) UseMethod("normalize",x)
normalize.wellList = function(wells, ID, ...) {
  x = lapply(wells,normalize.Well,ID)
}
normalize.Well = function( well, ID, ... ) {
  
  if( !(ID %in% well$timeline$ID) ) {
    outst = str_c("normalize: Well ",getlocations(well),
                  " does not have a ",ID," ID. Well Removed.")
    message(outst)
    return(NULL)
  }
  row = getrows.by.ID(well,ID)
  sweep = row$t1
  data = datamat(well)
  if( !(sweep %in% data$sweep) ) {
    stop("The sweep for the ID is not in this data. Was data sliced?")
  }
  data$values = data$values / data$values[ data$sweep==sweep ]
  datamat(well) = data
  well
}

# Normalize the data to a certain point in time by subtraction
level = function(x,ID,...) UseMethod("level",x)
level.wellList = function(wells, ID=NULL, s=NULL, ...) {
  x = lapply(wells, level, ID, s)
}
level.Well = function( well, ID=NULL, s=NULL ) {
  
  stopifnot( xor( is.null(ID), is.null(s)) )
  
  if(!is.null(ID)) {
    if( !(ID %in% well$timeline$ID) ) {
      outst = str_c("normalize: Well ",getlocations(well),
                    " does not have a ",ID," ID. Well Removed.")
      message(outst)
      return(NULL)
    }
    row = getrows.by.ID(well,ID)
    sweep = row$t1
  } else {
    sweep=s
  }
  data = datamat(well)
  if( !(sweep %in% data$sweep) ) {
    stop("The sweep for the ID is not in this data. Was data sliced?")
  }
  data$values = data$values - data$values[ data$sweep==sweep ]
  datamat(well) = data
  well
}

# center the time axis at a certain action ID
tcenter = function(x,ID,...) UseMethod("tcenter",x)
tcenter.wellList = function(wells, ID, ...) {
  x = lapply(wells,tcenter,ID)
}
tcenter.Well = function( well, ID, ... ) {
  
  if( !(ID %in% well$timeline$ID) ) {
    outst = str_c("tcenter: Well ",getlocations(well)," does not have a ",ID," ID. Well Removed.")
    message(outst)
    return(NULL)
  }
  sweep = getrows.by.ID(well,ID)$t1
  idx = which(sweepdata(well)==sweep)
  times = timesdata(well)
  timesdata(well) = times - times[idx]
  well
}

# use the spline (or smoother) in each well to get data 
# The "nbw" argument means that "N Points Between" points will
# be inserted between each of teh existing x-values. The derivative
# can be requested iwth the "deriv" argument
spline_interpolate = function(x,...) UseMethod("spline_interpolate",x)
spline_interpolate.wellList = function(x,...) {
  out = lapply(x,spline_interpolate,...)
  class(out) = class(x)
  return(out)
}
spline_interpolate.Well = function(x,nbw=0,deriv=0,smoother=FALSE,min.diff=NULL) {
  dd = welldata(x)
  tt = timesdata(x)
  ss = sweepdata(x)
  tt.new = insert.nbw(tt,nbw,min.diff)
  if( smoother ) {
    dd.new = x$smoother( tt.new, deriv=deriv )
  } else {
    dd.new = x$spline(tt.new,deriv=deriv)
  }
  ss.new = rep(NA_real_,length(tt.new))
  ss.new[ tt.new %in% tt ] = ss
  datamat(x) = data.frame(sweep=ss.new,time=tt.new,values=dd.new)
  x = add_interpolating_spline(x)
  return(x)
}

# Extract a slice of the data. This will speed up other analyses because
# of the smaller data set
slice = function(x,...) UseMethod("slice",x)
slice.wellList = function( wells, xlim=c(0,1), ... ) {
  out = lapply( wells, slice, xlim=xlim )
}
slice.Well = function( well, xlim=c(0,1), ... ) {
  
  times = timesdata(well)
  yn = times > xlim[1] & times < xlim[2]
  datamat(well) = datamat(well)[yn,]
  return(well)
}

# Find replicates according to their solution at the given ID
# Average them into single wells
averageReplicates = function( wells, ID="final" ) {
  
  # group the wells by their final solutions to find replicates
  solns = getsolutions(wells, ID)
  yn = duplicated(solns)
  groups = yn
  groups[!yn] = 1:sum(!yn)
  for( i in which(yn) ) {
    matched = mapply(identical,solns,solns[i])
    matched[i] = FALSE
    groups[i] = groups[ which(matched)[1] ]
  }
  
  # make a list of replicate groups
  g2 = split(1:length(groups),groups)
  new.wells = vector(mode="list",length=length(g2))
  
  # make new wells, averaging replicates when applicable
  for( i in seq_along(g2) ) {
    ids = g2[[i]]
    if(length(ids)==1) {
      new.wells[[i]] = wells[[ids]]
    } else {
      temp.wells = wells[ids]
      datas = datamat(temp.wells)
            refwell = temp.wells[[ which.max(vapply(datas,nrow,1)) ]]
      reftimes = timesdata(refwell)
      
      # Interpolating Spline Don't allow extrapolation
      foo = function(x) {
        out = x$spline(reftimes)
        times = timesdata(x)
        out[ reftimes > max(times) | reftimes < min(times) ] = NA
        out
      }
      vals.df = t(laply(temp.wells,foo))
      welldata(refwell) = rowMeans(vals.df,na.rm=TRUE)
      refwell$data$sds = apply(vals.df,1,sd)
      refwell$location = str_c( getlocations(temp.wells), collapse="+" )
      new.wells[[i]] = refwell
    }
  }
  class(new.wells) = class(wells)
  new.wells = add_interpolating_spline(new.wells)
  return(new.wells)
}
