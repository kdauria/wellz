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

# Custom paste function that returns NA
# if the input is NULL or if all inputs are NA
# Removes all arguments that are NA (though doesn't
# remove the argument if it is a vector that is not all NA's)
# Also removes NULL arguments before the call to paste
paste2 = function(...) {
  args = list(...)
  #print(args)
  sep = args$sep
  collapse = args$collapse
  args$sep = NULL
  args$collapse = NULL
  
  if( is.null(unlist(args)) ) {
    return(NA)
  } else {
    args = lapply(args,function(x) if(!is.null(x) && length(x) && all(is.na(x))) NULL else x )
    args = args[ !sapply(args,is.null) ]
    
    if( is.null(unlist(args)) ) return(NA)
    newargs = c( args, sep=sep, collapse=collapse)
    #print(newargs)
    return( do.call(paste, newargs ))
  }
}

# A very specific function, but one I use more than once
# This takes a list of character vectors
# unique=TRUE means that only the unique characters are returned
# unique=FALSE means that the characters for each list element are returned in a vector
#    the same length as the list
# collapse=TRUE means that 2+-character list elements are concatenated into one string
# collapse=FASE means that a list of character vectors is returned
list_concat_str = function( l, collapse=TRUE, unique=TRUE) {
  if( !collapse && !unique ) {
    out = l
  } else if( !collapse && unique ) {
    out = unique(unlist(l))
  } else if (collapse && !unique) {
    out = sapply( l, paste2, collapse="+" ) # NOTE paste2, not paste
  } else if (collapse && unique) {
    out = paste2( na.omit(unique(unlist(l))), collapse=", " )
  }
  return(out)
}

# Checks if something is a character of length 1
is_char_len1 = function(x) is.character(x) && length(x)==1

# A special row function meant to be fast
rowSD = function(x) {
  mat = as.matrix(x)
  m = nrow(mat)
  n = ncol(mat)
  .rowSums( (mat - .rowMeans(mat,m,n))^2, m, n )^2 / (ncol(mat)-1)
}


# insert n linearly interpolated numbers into a vector
# that can only be as close as min.dx
insert_na_between = function(x, n, min.dx=NULL ) {
  if( !is.null(min.dx)) {
    n_bw = pmax(0, pmin( floor(diff(x)/min.dx)-1, n ) )
    out = rep(NA_real_, length(x) + sum(n_bw) )
    out[ c(1, cumsum(n_bw) + 1:length(n_bw) + 1) ] = x
  } else {
    out = rep(NA_real_, (length(x)-1)*(n+1)+1 )
    out[ 1:length(x)*(n+1) - n] = x
  }
  return(out)
}
na_interp = function(x) {
  return( approx(x, xout=1:length(x))$y )
}





