#' Subset a wellList
#' 
#' Custom function so that subsetting does not lose class
#' 
#' @param x a wellList
#' @param i range to select
`[.wellList` = function( x, i ) {
  r = NextMethod("[")
  mostattributes(r) = attributes(x)
  r
}

#' Concatenate wellLists
#' 
#' Custom function so that concatenation does not lose class
#' 
#' @param x a wellList
#' @param ... more wellLists
c.wellList = function(x,...) {
  r = NextMethod("c")
  mostattributes(r) = attributes(x)
  r
}

#' Subset an actionList
#' 
#' Custom function so that subsetting does not lose class
#' 
#' @param x an actionList
#' @param i range to select
`[.actionList` = function( x, i ) {
  r = NextMethod("[")
  mostattributes(r) = attributes(x)
  r
}

#' Concatenate actionLists
#' 
#' Custom function so that concatenation does not lose class
#' 
#' @param x an actionList
#' @param ... more actionLists
c.actionList= function(x,...) {
  r = NextMethod("c")
  mostattributes(r) = attributes(x)
  r
}

#' A roster of wells
#' 
#' data frame with columns that are enough to uniquely
#' identify each row. A convenience function
#' 
#' @param wells wellList object
roster = function(wells) {
  data.frame( file=filename(wells), location=code(wells) ) 
}

#' Unique key of a well
#' 
#' Combines the filename and code of a well to give a unique key
#' 
#' @param x a well object
well_key.well = function(x) paste(filename(x),code(x))

#' Get the unique identifier(s) of one or more wells
#' 
#' This function gives the "code" or the string
#' that gives the location or identifier of a well
#' No two wells can have the same "code".
#' 
#' @importFrom Rcpp cppFunction
#' @param x well or wellList
#' @export
code = function(x) UseMethod("code",x)
code.well = function(x) x[["code"]]
code.wellList = function(x) code_rcpp(x)
cppFunction('
  CharacterVector code_rcpp( List x ) {
            List temp;
            unsigned int n=x.size(), i;
            CharacterVector out(n);
            for( i = 0; i<n ; i++ ) {
            temp = as<List>(x[i]);
            out[i] = as<std::string>(temp["code"]);
            }
            return out;  }')

#' Set the unique identifier(s) of one or more wells
#' 
#' This function sets the "code" or the string
#' that gives the location or identifier of a well
#' No two wells can have the same "code".
#' 
#' @param x well or wellList
#' @param value a character vector
"code<-" = function(x,value) UseMethod("code<-",x)
"code<-.well" = function(x,value) x[["code"]] = value
"code<-.wellList" = function(x,value) {
  for( i in seq_along(x)) {
    x[[i]]$code = value[i]
  }
  x
}

#' Data filenames of wells
#' 
#' Returns the name of the file(s) containing
#' the data for each well in the input
#' 
#' @importFrom Rcpp cppFunction
#' @param x well or wellList
#' @export
filename = function(x) UseMethod("filename",x)
filename.well = function(x) x[["file"]]
filename.wellList = function(x) filename_rcpp(x)
cppFunction('
  CharacterVector filename_rcpp( List x ) {
            List temp;
            unsigned int n=x.size(), i;
            CharacterVector out(n);
            for( i = 0; i<n ; i++ ) {
            temp = as<List>(x[i]);
            out[i] = as<std::string>(temp["file"]);
            }
            return out;  }')

#' Data filenames of wells
#' 
#' Sets the name of the file(s) containing
#' the data for each well in the input
#' 
#' @param x well or wellList
#' @param value a character vector
"filename<-" = function(x,value) UseMethod("filename<-",x)
"filename<-.well" = function(x,value) x[["file"]] = value
"filename<-.wellList" = function(x,value) {
for( i in seq_along(x)) {
    x[[i]]$file = value[i]
  }
  x
}

#' Index of an action
#' 
#' Gives the index or indices of actions or actions
#' in a action or actionList object
#' 
#' @param x action or actionList object
#' @importFrom Rcpp cppFunction
#' @export
index = function(x) UseMethod("index",x)
index.action = function(x) x[["i"]]
index.actionList = function(x) index_actionList_rcpp(x)
cppFunction('
  NumericVector index_actionList_rcpp( List x ) {
            List temp;
            unsigned int n=x.size(), i;
            NumericVector out(n);
            for( i = 0; i<n ; i++ ) {
              temp = as<List>(x[i]);
              out[i] = as<double>(temp["i"]);
            }
            return out;
            }')


#' Index of an action
#' 
#' Sets the index or indices of actions or actions
#' in a action or actionList object
#' 
#' @param x action or actionList object
"index<-" = function(x,value) UseMethod("i<-",x)
"index<-.action" = function(x,value) x[["i"]] = value
"index<-.actionList" = function(x,value) { for(i in seq_along(x)) x[[i]][["i"]] = value[i]; x }


#' ID/name of action(s)
#' 
#' Returns the names of the actions for an action, actionList, well, or wellList
#' 
#' @param x action, actionList, well, or wellList object
#' @export
ID = function(x) UseMethod("ID",x)
ID.action = function(x) x[["ID"]]
ID.actionList = function(x) sapply(x, function(y) y$ID)
ID.well = function(x) sapply(x$actions, ID)
ID.wellList = function(x) lapply(x,ID)


#' ID/name of action(s)
#' 
#' Sets the names of the actions for an action, actionList, well, or wellList
#' 
#' @param x action, actionList, well, or wellList object
"ID<-" = function(x,value) UseMethod("ID<-",x)
"ID<-.action" = function(x,value) x[["ID"]] = value
"ID<-.actionList" = function(x,value) { 
  for(i in seq_along(x)) x[[i]][["ID"]] = value[i]
  names(x) = value
  x
}

#' Volume of a solution
#' 
#' Returns the volume of a Solution object. If given a well
#' or wellList, it defaults to the last action. Otherwise, different
#' solutions can be specified by passing arguments to the solution
#' function (the ... arguments)
#' 
#' @param x a well object
#' @param ... passed to solution
volume = function(x,...) UseMethod("volume",x)
volume.Solution = function(x) x$volume
volume.well = function(x, ...) get_solution(x, ...)$volume
volume.wellList = function(x, ...) sapply(x, volume, ...)


#' Get solution(s) of a well
#' 
#' For actionList and well objects, the default is to return all solutions (ID=NA)
#' The default for wellList objects is to return the solution of the last action
#' If ID is a character of length 1, then return solution for the action with that ID.
#' If an action with that ID doesn't exist, return NA as the solution.
#' Do not allow length(ID)>1.
#' 
#' @param x action, actionList, well, or wellList
#' @param ... ID argument optional, eventually passed to action
get_solution = function(x,...) UseMethod("get_solution",x)
get_solution.default = function(x, ...) return(NA)
get_solution.action = function(x) x$solution
get_solution.actionList = function(x,ID=NA) {
  if( is.na(ID) ) return( lapply(x, get_solution) )
  return( get_solution(get_action(x, ID)) )
}
get_solution.well = function(x, ...) get_solution( get_actionList(x), ...)
get_solution.wellList = function(x, ID="last") lapply(x, get_solution, ID)


#' Change solution(s) of a well
#' 
#' For actionList and well objects, the replacement value must
#' be a list of solutions, one for each action.
#' 
#' @param x action, actionList, or well object
#' @param value solution objects
"solution<-" = function(x,value) UseMethod("solution<-",x)
"solution<-.action" = function(x,value) x[["solution"]] = value
"solution<-.actionList" = function(x,value) { for(i in seq_along(x)) x[[i]][["solution"]] = value[[i]]; x }
"solution<-.well" = function(x,value) { for(i in seq_along(x$actions)) x$actions[[i]][["solution"]] = value[[i]]; x }


#' Get actions(s) of a well or actionList
#' 
#' The default is to return the last action (ID="last")
#' If ID is a character of length 1, then return action that ID.
#' If an action with that ID doesn't exist, return NA.
#' Do not allow length(ID)>1.
#' 
#' @param x action, actionList, well, or wellList
#' @param ... ID argument optional
get_action = function(x, ...) UseMethod("get_action", x)
get_action.action = function(x, ID=NA, ...) {
  if(is.na(ID) || ID==x$ID ) return(x)
  return(NA)
}
get_action.actionList = function(x, ID="last", ...) {
  if( ID=="last" ) return( x[[length(x)]] )
  if( length(ID)!=1 ) stop("ID argument must be of length 1")
  if( !(ID %in% ID(x)) ) return(NA)
  return( x[[ID]] )
}
get_action.well = function(x, ...) get_action( get_actionList(x), ... )
get_action.wellList = function(x, ... ) lapply( x, get_action, ... )

#' Get actionsLists from well(s)
#' 
#' The default is to return the full actionList (ID=NULL)
#' Alternatively, an actionList containing only the last
#' action can be returned (ID="last")
#' For a wellList object, a list of actionLists are returned.
#' 
#' @param x actionList, well, or wellList
#' @param ... ID argument optional
get_actionList = function(x, ...) UseMethod("get_actionList",x)
get_actionList.actionList = function(x, ID=NULL, ...) {
  if(is.null(ID)) return(x)
  if(ID=="last") return(x[length(x)])
  matched.ids = ID[ ID %in% ID(x) ]
  if(length(matched.ids)==0) return(NA)
  x[matched.ids]
}
get_actionList.well = function(x, ...) get_actionList(x$actions, ...)
get_actionList.wellList = function(x, ...) lapply(x,get_actionList, ...)


#' Change actionsLists of well(s)
#' 
#' The only method of this S3 function is to replace the
#' actionList in each well in a wellList by supplying
#' a list of actionLists.
#' 
#' @param x actionList, well, or wellList
#' @param ... ID argument optional
"actionList<-" = function(x,value) UseMethod("actionList<-",x)
"actionList<-.wellList" = function(x,value) { for(i in seq_along(x)) x[[i]][["actions"]] = value[[i]]; x }

