# Melts a wellList with parameters
# See melt_wellList and param_matrix for more info
# melt_wellList_params(x, color="concentration", 
#                      size=1:7, shape="file", linetype="file")
melt_wellList_params = function(x, ...) {
  dt = melt_wellList(x)
  p = param_matrix(x, ...)
  dt[p] # a join
}

# Take the data from all the wells in a well list
# and melt it so that one measurement is on
# every row. The well and file for the measurement
# are indicated in the loc and file columns
melt_wellList = function(x) {
  l = lapply(x, "[[", "data")
  dt = rbindlist(l)
  dat.sizes = sapply(x,function(x) nrow(x$data))
  ff = rep(filename(x),times=dat.sizes)
  loc = rep(code(x),times=dat.sizes)
  dt[, file:=ff ]
  dt[, location:=loc ]
  setkey(dt,file,location)
  dt
}

# Expands plot arguments that are meant for group.wellList
# These will be the arguments that are characters of length one
# The output is a two-element list
# The first elemen (aes) is the aesthetic that will be used
# in ggplot2
# The second output is a data.table that contains the
# aesthetic information for each well (one row per well)
# This data.table will be joined with a large data matrix
# to make a final data.table that will be input into ggplot
# param_matrix(x, color="concentration", size=1:7, shape="file", linetype="file")
param_matrix = function( x, ..., type="start", ID="last", compound=NULL, solvent=NULL ) {

  check_well_params( x, ...)
  args = list(...)
  
  # the file and location are already going to be generated
  yn = sapply(args, function(y) is_char_len1(y) && y %in% c("file","location") )
  args[yn] = NULL
  is.auto.param = vapply( args, is_char_len1, TRUE )
  auto.params = unlist( args[is.auto.param] )
  names(args)[is.auto.param] = auto.params
  
  # Use group.wellList to expand the character of length one to a vector
  expanded.auto.params = lapply( auto.params, function(p, ...) group(x,p, ...), 
                                  type=type, ID=ID, compound=compound, solvent=solvent)
  args[is.auto.param] = expanded.auto.params
  
  # make a matrix mapping wells (ID'd by file and location) to parameters
  if( length(args)==0 ) {
    params = data.table( roster(x) )
  } else {
    params = data.table( cbind( roster(x), data.frame(args)) )
  }
  setkey(params, file, location)
  params
}

# Make an aesthetic for ggplot2 from the wellList object
# and optional parameters (see param_expand and check_well_params)
# well_aes(x, color="concentration", size=1:7, shape="file", linetype="file")
well_aes = function(x, ..., type="start", ID="last", compound=NULL, solvent=NULL) {
  args = list(...)
  check_well_params( x, ...)
  
  # keep track of argument names and those from group.wellList
  is.auto.param = vapply( args, is_char_len1, TRUE )
  variable = aesthetic = names(args)
  auto.params = unlist( args[is.auto.param] )
  variable[is.auto.param] = auto.params
  
  # make an aesthetic (aes) for ggplot2
  aes.list = as.list(variable)
  names(aes.list) = aesthetic
  if(length(aes.list)==0) aes.list = list(color="location")
  aes.list = c(aes.list, x="t", y="value")
  aes.out = do.call(aes_string, aes.list)
  aes.out
}

# Check if the inputs for the plotting function are allowed
# Automatic parameters will eventually be input into group.wellList
check_well_params = function( x, ... ) {
  
  args = list(...)
  allowable.param.names = c("color","linetype","size","alpha","shape","fill")
  allowable.params = c("location","file","volume","compound",
                       "concentration","solvent","solvent.percentages")
  
  # Make sure every argument has a distinct name
  nms = names(args)
  if( length(nms) != length(unique(nms)) )
    stop("Every argument name must be distinct")
  
  # check that plot parameters match to plottable aesthetics
  if( !all(names(args) %in% allowable.param.names) )
    stop( "plot argument names must be one of ", paste(allowable.param.names,collapse=" "))
  
  # parameters to describe the wells for the group.wellList function
  is.auto.param = vapply( args, is_char_len1, TRUE )
  auto.params = unlist( args[is.auto.param] )
  is.allowed = auto.params %in% allowable.params
  if(!all(is.allowed)) 
    stop(paste("plot parameters of length 1 must be one of", paste(allowable.params,collapse=" ")))
  
  # manual parameters 
  manual.params = args[!is.auto.param]
  if( length(manual.params) ) {  
    is.correct.length = unlist(lapply(manual.params, function(y) length(y)==length(x)))
    if(!all(is.correct.length))
      stop( "argument '", names(manual.params)[!is.correct.length][1], "' is ",
            "not a character of length 1 or a vector the length of the wells")
  }
  return(TRUE)
}








