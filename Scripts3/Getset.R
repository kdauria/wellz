
# select wells based off of the search.wellList function
select = function(x, ...) UseMethod("select",x)
select.wellList = function(wells, ...) {
  yn = search(wells, ...)
  wells[which(yn)]
}
"select<-" = function(x, ...) UseMethod("select<-",x)
"select<-.wellList" = function(wells, value, ...) {
  yn = search(wells, ...)
  wells[which(yn)] = value
  wells
}

roster = function(wells) {
  data.frame( file=filename(wells), code=code(wells) ) 
}

# Don't lose class when subsetting
`[.wellList` = function( x, i ) {
  r = NextMethod("[")
  mostattributes(r) = attributes(x)
  r
}
c.wellList = function(x,...) {
  r = NextMethod("c")
  mostattributes(r) = attributes(x)
  r
}


#############################################################
#         Simple, fast accessors of well information        #
#############################################################

## Different types of accessor functions:
# code
# filename
# index
# solution
# actionList
# ID
# volume


##### "code" or "location"
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
"code<-" = function(x,value) UseMethod("code<-",x)
"code<-.well" = function(x,value) x[["code"]] = value
"code<-.wellList" = function(x,value) code_replace_rcpp(x,value)
cppFunction('
  List code_replace_rcpp( List x, CharacterVector value) {
            List temp;
            unsigned int n=x.size(), i;
            List out(n);
            for( i = 0; i<n ; i++ ) {
              temp = as<List>(x[i]);
              temp["code"] = as<std::string>(value[i]);
              out[i] = temp;
            }
            out.attr("class") = x.attr("class");
            return out;  }')

######### "file" or "filename"
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
"filename<-" = function(x,value) UseMethod("filename<-",x)
"filename<-.well" = function(x,value) x[["file"]] = value
"filename<-.wellList" = function(x,value) filename_replace_rcpp(x,value)
cppFunction('
  List filename_replace_rcpp( List x, CharacterVector value) {
            List temp;
            unsigned int n=x.size(), i;
            List out(n);
            for( i = 0; i<n ; i++ ) {
            temp = as<List>(x[i]);
            temp["file"] = as<std::string>(value[i]);
            out[i] = temp;
            }
            out.attr("class") = x.attr("class");
            return out;  }')

####### "index# or "time" of an action
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
"index<-" = function(x,value) UseMethod("i<-",x)
"index<-.action" = function(x,value) x[["i"]] = value
"index<-.actionList" = function(x,value) { for(i in seq_along(x)) x[[i]][["i"]] = value[i]; x }
cppFunction('
  List index_actionList_replace_rcpp( List x, NumericVector value) {
            List temp;
            unsigned int n=x.size(), i;
            List out(n);
            for( i = 0; i<n ; i++ ) {
              temp = as<List>(x[i]);
              temp["i"] = value[i];
              out[i] = temp;
            }
            out.attr("class") = x.attr("class");
            return out;  }')

######### "solution"
solution = function(x,...) UseMethod("solution",x)
solution.action = function(x) x[["solution"]]
solution.actionList = function(x,ID=NA) {
  if(!is.na(ID)) {
    if(ID=="last") {
      out = x[[length(x)]]$solution
    } else {
      out = x[[ID]]$solution # returns NULL if ID isn't in action list
    }
  } else {
    out = lapply(x,"[[","solution")
  }
  return(out)
}
solution.well = function(x, ID="last") solution( actionList(x), ID)
solution.wellList = function(x, ...) lapply(x,solution, ...)
"solution<-" = function(x,value) UseMethod("solution<-",x)
"solution<-.action" = function(x,value) x[["solution"]] = value
"solution<-.actionList" = function(x,value) { for(i in seq_along(x)) x[[i]][["solution"]] = value[[i]]; x }
"solution<-.well" = function(x,value) { for(i in seq_along(x$actions)) x$actions[[i]][["solution"]] = value[[i]]; x }

########## "actionList"
actionList = function(x) UseMethod("actionList",x)
actionList.well = function(x) x$actions
actionList.wellList = function(x) lapply(x,"[[","actions")
"actionList<-" = function(x,value) UseMethod("actionList<-",x)
"actionList<-.wellList" = function(x,value) { for(i in seq_along(x)) x[[i]][["actions"]] = value[[i]]; x }


######### "ID" of actions
ID = function(x) UseMethod("ID",x)
ID.action = function(x) x[["ID"]]
ID.actionList = function(x) vapply(x,"[[","","ID")
ID.well = function(x) sapply(x$actions, ID)
"ID<-" = function(x,value) UseMethod("ID<-",x)
"ID<-.action" = function(x,value) x[["ID"]] = value
"ID<-.actionList" = function(x,value) { 
  for(i in seq_along(x)) x[[i]][["code"]] = value[i]
  names(x) = value
  x
}

######## "volume"
volume = function(x,...) UseMethod("volume",x)
volume.Solution = function(x) x$volume
volume.well = function(x, ...) solution(x, ...)$volume
volume.wellList = function(x, ...) sapply(x, volume, ...)


###############################################################
#                   More complex functions                    #
###############################################################

##### "solvent_names"
solvent_names = function(x,...) UseMethod("solvent_names",x)
solvent_names.default = function(x,...) return(NA)
solvent_names.Solution = function(x, ...) x$solvent$name
solvent_names.well = function(x, ... ) solvent_names(solution(x, ...))
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
compound_names.well = function(x, type="start", ...) {
  compound_names( solution(x, ...), type=type )
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
concentration.well = function(x, compound=NULL, type="start", ... ) {
  concentration(solution(x, ... ), type=type, compound=compound )
}
concentration.wellList = function(x, compound=NULL, type="start", ... ) {
  
  # make sure a numeric is returned if there is only one compound in all wells
  if(is.null(compound)) {
    nms = compound_names(x, collapse=FALSE, type=type)
    if( length(unique(nms))==1 ) compound = nms
  }
  sapply( x, concentration, compound=compound, type=type, ... )
}


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
solvent_percentages.well = function(x, solvent=NULL, ... ) {
  solvent_percentages( solution(x,...), solvent=solvent )
}
solvent_percentages.wellList = function(x, solvent=NULL, ... ) {
  
  # make sure a numeric is returned if there is only one solvent in all wells
  if(is.null(solvent)) {
    nms = compound_names(x, collapse=FALSE, type=type)
    if( length(unique(nms))==1 ) solvent = nms
  }
  sapply( x, solvent_percentages, solvent=solvent, ... )
}






















