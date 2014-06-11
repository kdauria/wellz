#' Expand ranges of letters
#' 
#' Expand ranges of letters. For example
#' \code{"A-C, D, F-H"} becomes
#' \code{"A","B","C","D","F","G","H"}
#' 
#' @param a string with text ranges separated by commas
expand_letters = function(x) {
  if( grepl("-",x,fixed=TRUE) ) {
    lets = strsplit(x,"-")[[1]]
    bounds = match(lets,LETTERS)
    return( LETTERS[ bounds[1]:bounds[2] ] )
  } else {
    return(x)
  }
}

#' Expand ranges of numbers
#' 
#' Expand ranges of numbers For example
#' \code{"1-4, 8, 11-13"} becomes
#' \code{1, 2, 3, 4, 8, 11, 12, 13)}.
#' 
#' @param a string with number ranges separated by commas
expand_numbers = function(x) {
  if( grepl("-",x,fixed=TRUE) ) {
    bounds = as.numeric( strsplit(x,"-")[[1]] )
    x = as.character( bounds[1]:bounds[2] )
  }
  x
}

#' Expand well format notation
#' 
#' On a typical plate, wells are indicated where the rows
#' are letters and the columns are numbers. This code expands
#' shorthand. For instance, \code{"A-C1, D-E3-4, F6"} becomes
#' \code{c("A1","B1","C1","D3","D4","E3","E4","F6")}
#' 
#' @param a string with well location shorthand
expand_code = function(code) {
  
  codes = strsplit(code,"[ ]*,[ ]*")[[1]]
  
  letter.parts = sub(".*?([A-Z][-]?[A-Z]?).*", "\\1", codes)
  code.lets = lapply(letter.parts,expand_letters)
  
  number.parts = sub(".*?([0-9]+\\-?[0-9]*).*", "\\1", codes)
  code.nums = lapply(number.parts,expand_numbers)
  
  c(unlist( mapply(function(x,y) outer(x,y,FUN=paste0), 
                   code.lets, code.nums) ))
}

