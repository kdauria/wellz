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
#' @param ... ID passed to solution; unique, and collapse described below
#' @export
solvent_names = function(x,...) UseMethod("solvent_names",x)
#' @export
solvent_names.default = function(x,...) return(NA)
#' @export
solvent_names.Solution = function(x, ...) x$solvent$name
#' @export
solvent_names.well = function(x, ID="last", ... ) solvent_names(get_solution(x, ID="last", ...))
#' @export
solvent_names.wellList = function(x, unique=TRUE, collapse=TRUE, ...) {
  nms = lapply(x, solvent_names, ...)
  out = list_concat_str( nms, unique=unique, collapse=collapse )
  return(out)
}


#' Compound names
#' 
#' This gets the names of the compounds By default, the last
#' action of well is used to get the solution (ID="last"). If
#' ID is given but an action with that ID doesn't exist, NA is returned.
#' For a wellList, there are four possible types of return values
#' based on the unique and collapse arguments. unique=TRUE indicates
#' that all the compound names will be condensed into a vector of only
#' unique compound names. If unique=FALSE, then a list is returned (the length equal to
#' length(x) ). collapse=TRUE indicates that the compound names will be concatenated
#' together into a single string. If collapse=FALSE, a character vector is returned.
#' 
#' @param x Solution, well, or wellList
#' @param ... ID passed to solution; unique, and collapse described below
#' @export
compound_names = function(x,...) UseMethod("compound_names",x)
#' @export
compound_names.default = function(x,...) return(NA)
#' @export
compound_names.Solution = function(x, type="all" ) {
  if( type=="all" ) {
    return( x$compound$name )
  } else {
    return( x$compounds$name[ x$compounds$type==type ] )
  }
}
#' @export
compound_names.well = function(x, type="start", ID="last", ...) {
  compound_names( get_solution(x, ID=ID, ...), type=type )
}
#' @export
compound_names.wellList = function(x, unique=TRUE, collapse=TRUE, ...) {
  nms = lapply(x, compound_names, ...)
  out = list_concat_str(nms, unique=unique, collapse=collapse)
  return(out)
}


#' @title Concentration of well(s)
#' 
#' @description
#' Returns the contents of wells depending on requested parameters (e.g., ID
#' or compound name).
#' 
#' @details
#' For Solution objects, a compound can be requested with the
#' compound argument. This argument must be a character variable of length
#' one. If compound=NULL (the default), the concentrations
#' are concatenated into a string of the form 
#' "compoundName1-12.34, compoundName2-56.7, compoundName3-8.9".
#' If compound does not exist in the Solution, 0 is returned.
#' The \code{type} argument can specify to search for compounds
#' of type "start" or "total" or to search "all" compounds.
#' 
#' For well objects, the default is to use the solution
#' of the last action of the well (\code{ID="last"}). \code{ID} can be specified in
#' to choose another solution. If \code{ID} does not specify an action,
#' \code{NA} is returned. \code{compound} and \code{type} can also be specified.
#' 
#' For wellList objects, if all the wells contain the same one compound
#' at different concentrations, then a numeric vector is returned.
#' A numeric vector is also returned if the \code{compound} is specified
#' as an argument. Otherwise, a vector of strings is returned as described
#' above for Solution objects. The last solution is used unless \code{ID}
#' is specified.
#' 
#' @param x Solution, well, or wellList
#' @param ... \code{ID} passed to solution; \code{compound} described below
#' @export
concentration = function(x,...) UseMethod("concentration", x)
#' @export
concentration.default = function(x, ...) return(NA)
#' @export
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
#' @export
concentration.well = function(x, compound=NULL, ID="last", type="start", ... ) {
  concentration(get_solution(x, ID=ID, ... ), type=type, compound=compound )
}
#' @export
concentration.wellList = function(x, compound=NULL, type="start", ... ) {
  
  # make sure a numeric is returned if there is only one compound in all wells
  if(is.null(compound)) {
    nms = compound_names(x, collapse=FALSE, type=type)
    if( length(unique(nms))==1 ) compound = nms
  }
  sapply( x, concentration, compound=compound, type=type, ... )
}




#' @title Solvent composition of well(s)
#' 
#' @description
#' Returns the solvent percentages of wells depending on requested parameters (e.g., ID
#' or solvent name).
#' 
#' @details
#' For Solution objects, a solvent can be requested with the
#' \code{solvent} argument. This argument must be a character variable of length
#' one. If \code{solvent=NULL} (the default), the concentrations
#' are concatenated into a string of the form 
#' "solventName1-25.0, solventName2-30.0, solventName3-45.0.
#' If \code{solvent} does not exist in the Solution, 0 is returned.
#' 
#' For well objects, the default is to use the solution
#' of the last action of the well (\code{ID="last"}). \code{ID} can be specified in
#' to choose another solution. If \code{ID} does not specify an action,
#' \code{NA} is returned. \code{solvent} can also be specified.
#' 
#' For wellList objects, if all the wells contain the same one solvent
#' at different concentrations, then a numeric vector is returned.
#' A numeric vector is also returned if the \code{solvent} is specified
#' as an argument. Otherwise, a vector of strings is returned as described
#' above for \code{Solution} objects. The last solution is used unless \code{ID}
#' is specified.
#' 
#' @param x Solution, well, or wellList
#' @param ... \code{ID} passed to solution; \code{solvent} described below
#' @export
solvent_percentages = function(x, ...) UseMethod("solvent_percentages",x)
#' @export
solvent_percentages.default = function(x, ...) return(NA)
#' @export
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
#' @export
solvent_percentages.well = function(x, solvent=NULL, ID="last", ... ) {
  solvent_percentages( get_solution(x, ID=ID, ...), solvent=solvent )
}
#' @export
solvent_percentages.wellList = function(x, solvent=NULL, ... ) {
  
  # make sure a numeric is returned if there is only one solvent in all wells
  if(is.null(solvent)) {
    nms = solvent_names(x, collapse=FALSE)
    if( length(unique(nms))==1 ) solvent = nms
  }
  sapply( x, solvent_percentages, solvent=solvent, ... )
}


