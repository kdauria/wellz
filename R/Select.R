#' @title Search a wellList for certain wells
#' 
#' @description
#' This function takes several parameters (see arguments) and 
#' returns wells that match \emph{all} of the arguments.
#' 
#' @details
#' The \code{filename} and \code{code} arguments are the most straightforward.
#' Wells with the matching filenames or codes/locations are found.
#' \code{controls=TRUE} also finds wells with no compounds, only solvent.
#' 
#' The \code{compstr} argument (the COMPound STRing) is the most flexible.
#' It parses a string describing one or more concentrations and returns
#' wells that meet those requirements. This is best described by example.
#' For instance, the string \code{"CompoundA"} will return all
#' wells that have "CompoundA". Next, the concentration of compounds
#' can be set. For instance, \code{"CompoundA [10]"} will find wells
#' with CompoundA at a concentration of 10. \code{"CompoundA [10-1000]"}
#' will find all wells with CompoundA at or above a concentration of 10 and at or
#' below a concentration of 1000. The logical operaturs \code{&}, \code{|}, and
#' \code{!} can be used in front of compound names. For instance,
#' \code{"CompoundA [10-1000] & !CompoundB"} will find all wells with
#' CompoundA between the specificed concnetrations that also do
#' not have CompoundB.
#' 
#' \code{ID} specifies the name of the action (and so the Solution)
#'  to look for in each well. If an action with the name does not exist,
#'  \code{NA} is the result.
#'
#' The final output from matching all of these compounds is a
#' logical vector the same length as the number of wells in 
#' the \code{wellList} object. The \code{TRUE} elements indicate
#' which well met the required parameters
#' 
#' @param x a \code{wellList} object
#' @param compstr a string used to search for compounds
#' @param filename a string
#' @param code a string
#' @param ID a string
#' @param controls a \code{logical}
search = function(x,...) UseMethod("search",x)
search.wellList = function(x,compstr=NULL,filename=NULL,code=NULL,ID="last",controls=FALSE) {
  
  # filename & code
  yn = rep(TRUE,length(x))
  if(!is.null(filename)) yn = yn & filename(x) %in% filename
  if(!is.null(code)) yn = yn & code(x) %in% code
  if(ID!="last") yn = yn & sapply( ID(x), function(x) ID %in% x )
  if(!is.null(compstr)) {
    bounds = parse_comp_str( compstr )
    yn = yn & sapply(x, match_well_string, compstr, bounds, ID)
  }
  if(controls) {
    files = filename(x)
    concs = concentration(x,type="start",ID=ID)
    yn[ concs %in% "" & files %in% unique(files[yn]) ] = TRUE
  }
  yn
}


#' Select wells given parameters
#' 
#' Returns the subset of wells matching the given
#' parameters. The parameters are specified in the
#' \code{...} arguments which are passed to \code{search}.
#' 
#' @param x a \code{wellList} object
#' @param ... passed to \code{search}
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


#' Parses string for finding compounds
#' 
#' See \code{search} for a description of what the
#' "compound string" (\code{comp.str} here) can be.
#' The output here is a 3-column data.frame.
#' First column is the name of the compound; second
#' is the lower bound of the concnetration; third is the
#' upper bound. These names and bounds are then used to search
#' for wells that match these requirements.
#' 
#' @param comp.str see \code{search} for description of the string format
parse_comp_str = function( comp.string ) {
  # get string between each boolean operator
  sp = "[[:space:]]*"
  pattern = paste0(sp,"[[:alnum:]\\-\\.\\_]+",sp,
                   "\\[*",sp,"[[:digit:][:space:]\\.-]*","\\]*",sp)
  matches = str_match_all(comp.string, pattern)[[1]]
  comps = gsub("[[:space:]]","",matches)
  
  # split the names and concentrations of each string
  comp = lapply( comps, function(x) strsplit(x,"\\s*\\[\\s*")[[1]])
  
  # get the compound name in each string
  nms = sapply( comp, "[", 1)
  
  # get the range of concs for each string
  ranges = sapply( comp, function(x) sub("\\]","", x[2]) )            
  range_to_bounds = function(x) as.numeric( strsplit(x,"\\s*-\\s*")[[1]] )
  bounds = lapply( ranges, range_to_bounds )
  bounds = lapply(bounds, function(x) 
    if( length(x)==1 ) { if(!is.na(x)) rep(x,2) else rep(NA_real_,2) } else x )
  
  # put the names and bounds in an easy data.frame
  bounds.df = data.frame(name=nms,
                         left=sapply(bounds,"[",1),
                         right=sapply(bounds,"[",2),
                         stringsAsFactors=FALSE)
  bounds.df$left[is.na(bounds.df$left)] = -Inf
  bounds.df$right[is.na(bounds.df$right)] = Inf
  
  return( bounds.df )
}

#' Find wells from string
#' 
#' This determines if a \code{well} object matches the parameters
#' in a string describing desired compounds
#' and concentrations. See \code{search} for a description of the 
#' required string format. The \code{ID} specifies which action and Solution
#' that will be used to define the well. The bounds of the concentrations
#' and compounds are parsed into a data frame with \code{parse_comp_str}.
#' See that function's documentation for more details. This function additionally
#' takes care of any logical operators in the string. It does this by evaluating
#' each compound/concentration parameter separately to \code{TRUE} or \code{FALSE}.
#' These logicals are then used to replace the corresponding part of the string
#' and the string is then evaluated as if it were regular R syntax. Because of the
#' regular expressions used here, one should be very careful using any
#' R functions. This function has only been tested with \code{|}, code{&}, and \code{!}.
#' 
#' @param well a \code{well} object
#' @param s string describing desired compounds
#'            and concentrations (see \code{search})
#' @param bounds an output of the same form as the output of \code{parse_comp_str(s)}
#' @param ID which action/Solution to use
match_well_string = function( well, s, bounds = parse_comp_str(s), ID="last" ) {

  out = logical(nrow(bounds))
  sn = solution(well,ID=ID)
  if( length(sn)==1 && is.na(sn) ) return(FALSE)
  
  # figure out which part of the bounds are satisfied
  for( i in 1:nrow(bounds) ) {
    nm = bounds$name[i]
    if( nm %in% sn$compounds$name ) {
      conc = sn$compounds[ sn$compounds$name==nm, "conc" ]
      lbound = bounds$left[i]
      rbound = bounds$right[i]
      out[i] = conc >= lbound & conc <= rbound
    } else {
      out[i] = FALSE
    }
  }
  
  # Replace the compounds with the TRUE/FALSE if they matched the well
  sp = "[[:space:]]*"
  pattern = paste0(sp,"[[:alnum:]\\-\\.\\_]+",sp,
                   "\\[*",sp,"[[:digit:][:space:]\\.-]*","\\]*",sp)
  notmatched = strsplit(s,pattern)[[1]]
  if( length(notmatched)==length(out) ) {
    eval.string = paste0(notmatched,out,collapse="")
  } else if( length(notmatched)==length(out)+1) {
    eval.string = paste0(notmatched[1],paste0(out,notmatched[-1],collapse=""),collapse="")
  }
  eval(parse(text=eval.string))
}






