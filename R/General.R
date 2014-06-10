#' Invert list names and elements
#' 
#' Consider a list as a map. 
#' The "keys" of a list (actually the \code{names()}) are mapped
#' to values. This function reverses that mapping, making a
#' list where the \code{names()} become the values and the values
#' become the \code{names()}
#' 
#' @param x a \code{list} object
invert_list = function(x) {
  split( rep(names(x),times=sapply(x,length)),
         unlist(x) )
}

#' String of number ranges
#' 
#' Take a vector of numbers and make a string
#' describing them. For instance, the numbers
#' \code{c(1,2,3,4,10,11,15,17,18)} becomes
#' \code{"1-4, 10-11, 15, 17-18"}.
#' 
#' @param x a numeric vector
text_ranges = function(x) {
  right = c( which( diff(x)!=1 ), length(x) )
  left = c(1, right[-length(right)]+1)
  right = x[right]
  left = x[left]
  
  ranges = mapply( function(x,y) ifelse(x==y, x, paste(x,y,sep="-")),
               left, right )
  paste(ranges,collapse=", ")
}

#' Format numbers above/below 1 differently
#' 
#' If >=1, require two decimal places.
#' If <1, require two significant digits
#' 
#' @param x a numeric vector
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

#' Custom wrapper around \code{paste}
#' 
#' Custom paste function that returns \code{NA}
#' if the input is \code{NULL} or if all inputs are \code{NA}.
#' Removes all arguments that are NA (though does not
#' remove the argument if it is a vector that is not all \code{NA}s).
#' Removes NULL arguments before the call to paste.
#' 
#' @param ... like a call to \code{paste}
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


#' summarizing a list of character vectors
#' 
#' A very specific function, but one I use more than once
#' This takes a list of character vectors
#' \code{unique=TRUE} means that only the unique characters are returned
#' \code{unique=FALSE} means that the characters for each list element are returned in a vector
#'    the same length as the list.
#' \code{collapse=TRUE} means that 2+-character list elements are concatenated into one string.
#' \code{collapse=FASE} means that a list of character vectors is returned
#' 
#' @param l a \code{list} of character vectors
#' @param collapse a \code{logical}
#' @param unique a \code{logical}
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

#' is character length one
#' 
#' is character length one
#' 
#' @param x a \code{character}
is_char_len1 = function(x) is.character(x) && length(x)==1

#' standard deviation of rows
#' 
#' This is much faster than using an \code{apply} type function.
#' 
#' @param x a matrix
rowSD = function(x) {
  mat = as.matrix(x)
  m = nrow(mat)
  n = ncol(mat)
  .rowSums( (mat - .rowMeans(mat,m,n))^2, m, n )^2 / (ncol(mat)-1)
}


#' Insert NA values
#' 
#' Insert n NA value between each element of a numeric
#' vector.
#' 
#' @param x numeric vector
#' @param n number of NA values in between each element
#' @param the smallest spacing allowed between values if
#'           the \code{NA}s were linearly interpolated
insert_na_between = function(x, n, min.dx=NULL, ... ) {
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

#' Interpolate \code{NA} values
#' 
#' Fill in \code{NA} values by linear interpolation
#' 
#' @param x numeric vector
na_interp = function(x) {
  return( approx(x, xout=1:length(x))$y )
}





