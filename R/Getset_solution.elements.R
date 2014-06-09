

#' Solvent names
#' 
#' This gets the names of the solvents. By default, the last
#' action of well is used to get the solution (ID="last"). If
#' ID is given but an action with that ID doesn't exist, NA is returned.
#' For a wellList, there are four possible types of return values
#' based on the unique and collapse arguments. unique=TRUE indicates
#' that all the solvent names will be condensed into a vector of only
#' unique solvent names. If unique=FALSE, then a list is returned (the length equal to
#' length(x) ). collapse=TRUE indicates that the solvent names will be concatenated
#' together into a single string. If collapse=FALSE, a character vector is returned.
#' 
#' @param x Solution, well, or wellList
#' @param ... ID passed to solution, unique, and collapse described below
#' @export
solvent_names = function(x,...) UseMethod("solvent_names",x)
solvent_names.default = function(x,...) return(NA)
solvent_names.Solution = function(x, ...) x$solvent$name
solvent_names.well = function(x, ID="last", ... ) solvent_names(solution(x, ID="last", ...))
solvent_names.wellList = function(x, unique=TRUE, collapse=TRUE, ...) {
  nms = lapply(x, solvent_names, ...)
  out = list_concat_str( nms, unique=unique, collapse=collapse )
  return(out)
}

######## "compound_names" of compounds in a solution, well, or wellList
# NA is returned if an action with ID (see compound_names.well and solution)
# does not exist
compound_names = function(x,...) UseMethod("compound_names",x)
compound_names.default = function(x,...) return(NA)
compound_names.Solution = function(x, type="all" ) {
  if( type=="all" ) {
    return( x$compound$name )
  } else {
    return( x$compounds$name[ x$compounds$type==type ] )
  }
}
compound_names.well = function(x, type="start", ID="last", ...) {
  compound_names( solution(x, ID=ID, ...), type=type )
}
compound_names.wellList = function(x, unique=TRUE, collapse=TRUE, ...) {
  nms = lapply(x, compound_names, ...)
  out = list_concat_str(nms, unique=unique, collapse=collapse)
  return(out)
}


######## "concentration" of a well, wellList
concentration = function(x,...) UseMethod("concentration", x)
concentration.default = function(x, ...) return(NA)
concentration.Solution = function( x, compound=NULL, type="start" ) {
  
  # if the concentration for a single compound is requested, return a numeric
  if( !is.null(compound) && (class(compound)!="character" || length(compound)!=1))
    stop( "compound argument must be character class of length 1" )
  if( !is.null(compound) ) {
    out = x$compounds[ x$compounds$name==compound, "conc" ]
    if(length(out)==0) out = 0
    return(out)
  }
  
  # Otherwise get compound names to return
  compound = compound_names( x, type=type )
  comp.rows = x$compounds[ x$compounds$name %in% compound, ]
  out = paste(comp.rows$name, format1(comp.rows$conc), sep="-", collapse=", ")
  return(out)
}
concentration.well = function(x, compound=NULL, ID="last", type="start", ... ) {
  concentration(solution(x, ID=ID, ... ), type=type, compound=compound )
}
concentration.wellList = function(x, compound=NULL, type="start", ... ) {
  
  # make sure a numeric is returned if there is only one compound in all wells
  if(is.null(compound)) {
    nms = compound_names(x, collapse=FALSE, type=type)
    if( length(unique(nms))==1 ) compound = nms
  }
  sapply( x, concentration, compound=compound, type=type, ... )
}

######## "solvent_percentages" of a well, wellList
solvent_percentages = function(x, ...) UseMethod("solvent_percentages",x)
solvent_percentages.default = function(x, ...) return(NA)
solvent_percentages.Solution = function(x, solvent=NULL, ...) {
  
  # if the percentage for a single solvent is requested, return a numeric
  if( !is.null(solvent) && (class(solvent)!="character" || length(solvent)!=1))
    stop( "solvent argument must be character class of length 1" )
  if( !is.null(solvent) ) {
    out = x$solvent[ x$solvent$name==solvent, "perc" ]
    if(length(out)==0) out = 0
    return(out)
  }
  
  out = paste( x$solvent$name, format1(x$solvent$perc), sep="-", collapse=", ")
  return(out)
}
solvent_percentages.well = function(x, solvent=NULL, ID="last", ... ) {
  solvent_percentages( solution(x, ID=ID, ...), solvent=solvent )
}
solvent_percentages.wellList = function(x, solvent=NULL, ... ) {
  
  # make sure a numeric is returned if there is only one solvent in all wells
  if(is.null(solvent)) {
    nms = solvent_names(x, collapse=FALSE)
    if( length(unique(nms))==1 ) solvent = nms
  }
  sapply( x, solvent_percentages, solvent=solvent, ... )
}


