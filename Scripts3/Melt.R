# Melts a wellList with parameters
# See melt_wellList and param_matrix for more info
# melt_wellList_params(x, "volume", "file", "concentration", 
#                      manual.params=list(aa=1:7,bb=2:8), ID="toxinAdd")
melt_wellList_params = function(x, ...) {
  dt = melt_wellList(x)
  p = param_matrix(x, ...)
  dt[p]
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
  dt[, loc:=loc ]
  setkey(dt,file,loc)
  dt
}

# Make a data.table of parameters that describe wells
# Each row is one well
# param_matrix(x, "volume", "file", "concentration", 
#              manual.params=list(aa=1:7,bb=2:8), ID="toxinAdd")
param_matrix = function(x, ..., auto.params=NULL, manual.params=NULL,
                        type="start", ID="last", compound=NULL, solvent=NULL) {
  
  params = check_well_params(x, ..., auto.params=auto.params, man.params=manual.params)
  
  # get the well groups with group.wellList
  params$auto = setdiff(params$auto,c("file","location"))
  auto.params = lapply( params$auto, function(p, ...) group(x,p, ...), 
                        type=type, ID=ID, compound=compound, solvent=solvent)
  names(auto.params) = params$auto
  
  out = data.frame( file=filename(x), loc=code(x), auto.params, params$manual )
  out = data.table(out)
  setkey(out, file, loc)
  out
}


# Check the parameters requested to descibre each of the wells
# If the input is a character of length one, it will eventually
# be used by group.wellList and so must be one of the allowable 
# inputs of group.wellList
# Otherwise, one can define their own vector that groups the wells
# The vector must be the same length as the length of the wellList
# Additional parameters for group.wellList can also be given as a vector
# of characters in the auto.params argument
# Additional manual vectors can be given in the man.params argument

# check_well_params(x, "a", "b", 
#                   rep(c("a","b"),times=c(3,4)),
#                   rep(c("a","b"),times=c(1,6))) # error!
# 
# check_well_params(x, "volume", "file", 
#                  a=rep(c("a","b"),times=c(3,5)),
#                  b=rep(c("a","b"),times=c(1,6))) # error!
# check_well_params(x, "volume","file", a=1:7, 
#                   man.params=list(b=1:7,cc=2:8), 
#                   auto.params=c("concentration","volume","compound"))
check_well_params = function(x, ..., auto.params=NULL, man.params=NULL) {
  
  args = list(...)
  is.char.len1 = unlist(lapply( args, function(x) length(x)==1 && is.character(x)))
  
  # parameters to describe the wells from the group.wellList function
  allowable.params = c("location","file","volume","compound",
                       "concentration","solvent","solvent.percentages")
  group.params = unlist( args[is.char.len1] )
  group.params = union(auto.params, group.params)
  is.allowed = group.params %in% allowable.params
  if(!all(is.allowed)) 
    stop(paste(group.params[!is.allowed], "is not an allowed parameter"))
  
  # manual parameters 
  manual.params = args[!is.char.len1]
  manual.params = c(manual.params, man.params)
  if( length(manual.params) ) {
    
    nms = names(manual.params)
    has.no.name = nms == ""
    if( is.null(nms) || any(has.no.name) )
      stop("Every manual parameter must be a named argument")
    if( length(nms) != length(unique(nms)) )
      stop("Every manual parameter must have a distinct name")
    if( any(nms %in% group.params) )
      stop("Manual parameter name cannot be the same as an automatic parameter")
    
    is.correct.length = unlist(lapply(manual.params, function(y) length(y)==length(x)))
    if(!all(is.correct.length))
      stop(paste( "argument", names(manual.params)[!is.correct.length]), " is ",
           "not a character of length 1 or the length of the wells")
  }
  list( auto=group.params, manual=manual.params )
}


args = list( color="concentration", size=1:7 )

is.auto.param = sapply( args, is_char_len1 )
args[is.auto.param]






