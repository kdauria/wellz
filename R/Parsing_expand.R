# expand the letters
expand_letters = function(x) {
  if( grepl("-",x,fixed=TRUE) ) {
    lets = strsplit(x,"-")[[1]]
    bounds = match(lets,LETTERS)
    return( LETTERS[ bounds[1]:bounds[2] ] )
  } else {
    return(x)
  }
}

# expand the numbers
expand_numbers = function(x) {
  if( grepl("-",x,fixed=TRUE) ) {
    bounds = as.numeric( strsplit(x,"-")[[1]] )
    x = as.character( bounds[1]:bounds[2] )
  }
  x
}

# combine the letters and numbers to get all wells
expand_code = function(code) {
  
  codes = strsplit(code,"[ ]*,[ ]*")[[1]]
  
  letter.parts = sub(".*?([A-Z][-]?[A-Z]?).*", "\\1", codes)
  code.lets = lapply(letter.parts,expand_letters)
  
  number.parts = sub(".*?([0-9]+\\-?[0-9]*).*", "\\1", codes)
  code.nums = lapply(number.parts,expand_numbers)
  
  c(unlist( mapply(function(x,y) outer(x,y,FUN=paste0), 
                   code.lets, code.nums) ))
}

