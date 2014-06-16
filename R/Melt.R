#' Melt a wellList with parameters
#' 
#' First, the wells are melted with \code{melt_wellList}. Additional
#' ID variables are added to the melted \code{data.table} from
#' the \code{...} arguments that are passed to \code{param_matrix} (which
#' are then eventually passed to \code{group})
#' 
#' @param x a \code{wellList} object
#' @param ... passed to \code{param_matrix(x, ...)}
#' @import data.table
#' @export
melt_wellList_params = function(x, ...) {
  dt = melt_wellList(x)
  p = param_matrix(x, ...)
  dt[p] # a join
}

#' Melt well data
#' 
#' Melt the data of many wells in a wellList to one
#' \code{data.table} where the ID variables are the
#' file and location of the well. The measured variables
#' are the times and data values for the wells.
#' 
#' @param x a \code{wellList object}
#' @import data.table
#' @export
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

#' Parameter matrix of wells
#' 
#' Make a matrix of well characteristics/parameters where each
#' row is for one well.
#' 
#' The first two columns of the matrix will be the file
#' and location of the well, which is enough to uniquely identify
#' it. Any further columns are specified in the \code{...} arguments.
#' These names of these arguments can only be 
#' \code{"color"}, \code{"linetype"}, \code{"size"}, \code{"alpha"},
#' \code{"shape"}, or {"fill"} (see \code{check_well_params()}).
#' The values of these arguments can only be 
#' \code{"location"}, \code{"file"}, \code{"volume"}, \code{"compound"},
#' \code{"concentration"}, \code{"solvent"}, or \code{"solvent.percentages"}.
#' This is because the values are passed to \code{group()}.
#' The name of the column will be the name of the argument and the
#' values of the column will be set to the vector from \code{group}.
#' The output is actually a \code{data.table}, not a \code{matrix}.
#' 
#' @import data.table
#' @param x a \code{wellList} object
#' @param ... passed to the \code{by} argument in \code{group}
#' @param type passed to \code{group}
#' @param ID passed to \code{group}
#' @param compound passed to \code{group}
#' @param solvent passed to \code{group}
#' @export
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


#' aes object for wellList
#' 
#' Make an aes object to be used by \code{ggplot2} from the
#' named arguments in \code{...}.
#' 
#' The aesthetics are limited to certain types (see 
#' \code{check_well_params}).
#' 
#' @param x a \code{wellList} object
#' @param ... the aesthetics
#' @param type ignored
#' @param ID ignored
#' @param compound ignored
#' @param solvent ignored
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


#' Check well parameters
#' 
#' This is principally an argument validation function
#' called by other functions.
#' It checks that the parameters that describe a well and the parameters that
#' will eventually be used for a plot are allowed within this framework.
#' 
#' Named arguments must be one of 
#' \code{"color"}, \code{"linetype"}, \code{"size"}, \code{"alpha"},
#' \code{"shape"}, or {"fill"}. Their values must be one 
#' \code{"location"}, \code{"file"}, \code{"volume"}, \code{"compound"},
#' \code{"concentration"}, \code{"solvent"}, or \code{"solvent.percentages"}.
#' Alternatively, the values can be an atomic vector the same length
#' as the number of wells in the \code{wellList}. The function aises an error
#' if the arguments are not valid. Otherwise, it returns \code{TRUE}.
#' 
#' @param x a \code{wellList} object
#' @param ... parameters and aesthetics
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








