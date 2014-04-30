# expand the letters
expand_letters = function(x) {
  if(str_detect(x,"-")) {
    lets = str_extract_all(x, "[A-Z]")[[1]]
    bounds = match(lets,LETTERS)
    return( LETTERS[ bounds[1]:bounds[2] ] )
  } else {
    return(x)
  }
}

# expand the numbers
expand_numbers = function(x) {
  if(str_detect(x,"-")) {
    bounds = sort(as.numeric(str_extract_all(x, "[0-9]")[[1]]))
    x = as.character( bounds[1]:bounds[2] )
  }
  str_pad( x, width=2, pad="0" )
}

# combine the letters and numbers to get all wells
expand_code = function(code) {
  codes = str_trim( str_split(code,",")[[1]])
  
  letter.parts = str_extract(codes,"[A-Z]-*[A-Z]*")
  code.lets = lapply(letter.parts,expand_letters)
  
  number.parts = str_extract(codes,"[0-9]+-*[0-9]*")
  code.nums = lapply(number.parts,expand_numbers)
  
  codes = c(unlist( mapply( function(x,y) 
    as.character(interaction(x,y,sep="")), 
    code.lets, code.nums ) ))
}