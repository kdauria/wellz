# Make the names of a list the elements
# and the elements the names
invert_list = function(x) {
  split( rep(names(x),times=sapply(x,length)),
         unlist(x) )
}

# Take a vector of numbers and make a string
# describing them. For instance, the numbers
# 1,2,3,4,10,11,15,17,18 become
# 1-4, 10-11, 15, 17-18
text_ranges = function(x) {
  right = c( which( diff(x)!=1 ), length(x) )
  left = c(1, right[-length(right)]+1)
  right = x[right]
  left = x[left]
  
  ranges = mapply( function(x,y) ifelse(x==y, x, paste(x,y,sep="-")),
               left, right )
  paste(ranges,collapse=", ")
}

# Custom number format for numbers above/below 1
# If >1, require two decimal places
# If <1, require two significant digits
format1 = function(x) {
  .local = function(x) {
    if( is.na(x) ) {
      out = NA_real_
    } else if( x < 1 ) {
      out = format(x,digits=2)
    } else {
      out = format(x,digits=floor(log10(x))+3)
    }
  }
  sapply(x,.local)
}
