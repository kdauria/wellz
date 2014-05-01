comma_split = function(x) str_trim(unlist(str_split(x,",")))
invert_list = function(x) {
  split( rep(names(x),times=sapply(x,length)),
         unlist(x) )
}