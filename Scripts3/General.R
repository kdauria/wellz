invert_list = function(x) {
  split( rep(names(x),times=sapply(x,length)),
         unlist(x) )
}
text_ranges = function(x) {
  right = c( which( diff(x)!=1 ), length(x) )
  left = c(1, right[-length(right)]+1)
  paste( x[left], x[right], sep="-", collapse=", " )
}