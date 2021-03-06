#' Print \code{wellList} object
#' 
#' Print the basic information of a \code{wellList} object.
#' If there are less than 32 wells, than a data.frame is printed
#' with a summary of compounds for each well
#' If there are more, than the number of wells in each data file
#' is displayed
#' 
#' @param wells a \code{wellList} object
#' @param printall print concentration of all wells even if there are >32 wells
#' @param ID the action and solution in each well with which to summarize the action
#' @export
print.wellList = function( wells, printall=FALSE, ID="last" ) {
  
  files = filename(wells)
  if(length(wells)==0) {
    cat("Empty wellList\n")
    return()
  }
  
  if( length(unique(files))==1 || length(wells)<33 || printall ) {
    
    # get information for every single well in a data.frame
    out = list()
    out$file = files
    out$loc = code(wells)
    solns = lapply(wells, function(x) get_solution(x,ID=ID))
    out$compounds = sapply(solns, compound_string, type="start")
    out$compounds[ sapply(solns,is.null) ] = paste0("No ", ID, " ID")
    out$total = sapply(solns, compound_string, type="total", wConc=FALSE)
    out = as.data.frame(out, stringsAsFactors=FALSE)
    
    # Add "ditto"s for repeated rows
    if(nrow(out)>1) {
      for( i in nrow(out):2 ) {
        repeated.cols = out[i,] == out[i-1,] | (is.na(out[i,]) & is.na(out[i-1,]))
        out[ i, repeated.cols ] = "-"
      }
    }
    print(out)
  } else {
    r = data.frame( roster(wells), w=1:length(wells) )
    rs = split( r$w, r$file )
  
    ranges = sapply(rs,text_ranges)
    comps = sapply( rs, function(x) paste(compound_names(wells[x]),collapse=", ") )
    cat("A 'wellList' object \n")
    print(data.frame(i=ranges,compounds=comps))
  }
}

#' Print well object
#' 
#' Print the file, location, summary of actions, and
#' summary of the last action
#' 
#' @param well a \code{well} object
#' @export
print.well = function( well ) {
  
  cat("File:", well$file, "\n")
  cat("Location:", well$code, "\n")
  cat(length(well$actions), "actions")
  #print(well$actions)
  
  cat("\n\n")
  cat("Final solution:\n")
  print( get_solution(well,ID="last") )
  
  if(length(well$data)!=0) {
    cat("\n\n")
    print_well_data(well$data)
    #print(well$data)
  }
}

#' Print Solution object
#' 
#' Print volume, compounds, solvents, and concentrations of 
#'   a solution
#'   
#' @param soln a \code{Solution} object
#' @export
print.Solution = function( soln ) {
  cat("Volume: ", soln$volume, "\n")  
  if( length(compound_names(soln)) ) cat( compound_string(soln), "\n" )
  cat( solvent_string(soln) )
}

#' Print an actionList
#' 
#' Print an actionList
#' 
#' @export
print.actionList = function( actionlist ) {
  
  out = list()
  out$ID = ID(actionlist)
  out$i = index(actionlist)
  out$rmVol = sapply(actionlist,"[[","rmVol")
  
  # EDIT!!! Solutions are no longer subtracted from another
  # Instead, a different actionList, the $states will be saved as
  # part of the wellList
  solns = get_solution(actionlist)  
  
  out$adVol = sapply(solns,"[[","volume")
  out$compounds = vapply( solns, compound_string, "" )
  out$solvent = vapply( solns, solvent_string, "" )
  if( length(unique(out$solvent))==1 ) out$solvent=NULL
  if( all(out$rmVol==0 | is.na(out$rmVol)) ) out$rmVol=NULL
  
  out = as.data.frame(out)
  print(out,row.names=FALSE)
}



####### Helper functions 


#' Summarize Solution compounds in one string
#' 
#' Concatenates the names and concentrations of all compounds
#' 
#' @param soln a \code{Solution} object
#' @param type the "type" of concentration, either \code{"start"}, 
#'           \code{"total"}, or \code{"all"}
compound_string = function(soln, type="all", wConc=TRUE ) {
  
  yn = switch(type,
              all = rep(TRUE,nrow(soln$compounds)),
              total = soln$compounds$type == "total",
              start = soln$compounds$type == "start" )
  
  nms =  soln$compounds$name[yn]
  if( wConc && length(nms) ) {
    concs = format1( soln$compounds$conc[yn] )
    out = paste0( nms, " [", concs, "]", collapse=", " )
  } else if( length(nms) ) {
    out = paste0( nms, collapse=", " )
  } else {
    out = ""
  }
  out
}


#' Summarize Solution solvents in one string
#' 
#' Concatenates the names and percentages of all solvents
#' 
#' @param soln a \code{Solution} object
solvent_string = function(soln) {
  if( nrow(soln$solvent)==0 ) return ("")
  paste0( soln$solvent$perc, "% ", soln$solvent$name, collapse=", " )
}


#' Print abbreviated view of well data
#' 
#' This shows the first and last 5 rows of data so
#'   that the screen doesn't fill with numbers in
#'   other print functions. This was inspired by
#'   \code{print.data.table}.
#'   
#' @param data a \code{data.frame}
print_well_data = function(data) {
  if( nrow(data) > 10 ) {
    top = data[1:5,]
    bottom = tail(data,5)
    cat( nrow(data), "data points\n" )
    print( rbind(top,"---",bottom), row.names=FALSE)
  } else {
    print(data, row.names=FALSE )
  }
}

