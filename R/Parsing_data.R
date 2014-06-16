#' Add data to a well or wells
#' 
#' A wrapper that moves data from a file and parsing function
#' to well objects
#' 
#' \code{parse_fun} must take one input, the filepath of a
#' data file. It must return a data frame where the first
#' column is named "t" and contains the times at which data
#' were recorded. The other columns must have the same names
#' as the locations of the well. These names are used to assign
#' that column's data to the appropriate well in a \code{wellList} object.
#' 
#' @param wells wellList object
#' @param data.dir the root directory for the data files
#' @param parse_fun a function that parses the data files
add_data = function(x,...) UseMethod("add_data",x)
add_data.wellList = function( wells, data.dir, parse_fun ) {

  allfiles = filename(wells)
  allcodes = code(wells)
  codes = split( allcodes, allfiles )
  files = names(codes)
  file.paths = file.path(data.dir,names(codes))
  r = roster(wells)
  
  for( i in seq_along(file.paths)) {
    
    codes.i = codes[[i]]
    message(paste("Parsing",files[i]))
    dat = parse_fun(file.paths[i])
    
    for( j in seq_along(codes.i) ) {
      if( !codes.i[j] %in% colnames(dat) ) {
        warning(paste("Well",codes.i[j],"not in file",files[i]))
        next
      }
      well.ij = which( files[i] == r$file & codes.i[j] == r$location )
      wells[[well.ij]]$data = dat[ , c("i","t",codes.i[j]) ]
      colnames(wells[[well.ij]]$data) = c("i","t","value")
    }
  }
  wells
}

#' Add data from data frame to wells
#' 
#' This function assumes that the column names of the data
#' frame are the locations of the wells. If \code{names=FALSE},
#' then the columns are assigned to wells in order (e.g., the third
#' column is assigned to the third well). The first column is assumed
#' to be the time. The column name "i" is reserved for the
#' "index" of the data points. If it is not already in the provided
#' data frame, then it is created as 1:nrow(df).
#' 
#' 
#' @param wells a \code{wellList} object
#' @param df the data frame holding the data
#' @param names a \code{logical}
add_data_data.frame = function( wells, df, names=FALSE) {
  
  # The index of the data points
  if( !("i" %in% n) )
    df$i = 1:nrow(df)
  
  if(names) {
    if( unique(code(wells))!=length(wells) ) {
      stop("Every well ID must be unique")
    }
    codes = code(wells)
    for( code in codes ) {
      if( !(code %in% colnames(dat)) ) {
        warning(paste("Well",code,"not in data frame"))
        next
      }
      well.i = code == codes
      wells[[well.i]]$data = df[ , c("i","t",codes) ]
      colnames(wells[[well.i]]$data) = c("i","t","value")
    }
  } else {
    for( j in seq_along(wells) ) {
      columns = match( c("i","t"), colnames(df) )
      columns = c( columns, setdiff( 1:ncol(df), columns )[j] )
      wells[[j]]$data = df[ , columns ]
      colnames(wells[[j]]$data) = c("i","t","value")
    }
  }
  return(wells)
}


#' Parse RTCA data
#' 
#' A parsing function to be used by \code{add_data}. The
#' type of file it parses is from right clicking on the plot
#' in the typical RTCA xCelligence plot and saving in List format
#' 
#' @param filepath filepath of csv file exported from RTCA software
parse_rtca = function(filepath) {
  dat = fread(filepath)
  dat[,"Time-Interval":=NULL,with=FALSE]
  dat[,i:=1:nrow(dat)]
  cn = colnames(dat)
  for( ii in 2:(ncol(dat)-1) )
    cn[ii] = paste0( substr(cn[ii],4,5) )
  cn[1] = "t"
  setnames(dat,cn)
  class(dat) = "data.frame"
  dat
}



