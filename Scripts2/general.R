################################################################################
#                          Other functions                                     #
################################################################################

# Remove empty rows and columns from a matrix of strings
rm.empty.rows.columns = function( x ) {
  filled = x != ""
  rr = apply(filled,1,any)
  cc = apply(filled,2,any)
  y = x[rr,cc]
  return(y)
}

# moves one row of a matrix/data.frame to the header (column names)
row.as.header = function( x, row=1 ) {
  colnames(x) = as.character(x[row,])
  return(x[-row,])
}

# returns TRUE if string is NA, of length zero, or "" (bs = Blank String)
is.bs = function(x) {
  foo = function(x) length(x)==0 || is.null(x) || is.na(x) || x==""
  out = vapply(x,foo,TRUE)
  if(length(out)==0) {
    out = foo(x)
  }
  return(out)
}

# cat with sep=""
cat0 = function(...) cat( ..., sep="" )

# insert n=nbw evenly spaced numbers in between the elements of vector x
insert.nbw = function( x, nbw, min.diff=NULL ) {
  
  if( !is.null(min.diff) ) {
    dx = diff(x)
    num.NAs = c( (dx>=min.diff)*nbw, 0 )
    out = rep(NA_real_,length=length(x)+sum(num.NAs))
    
    temp = num.NAs + 1
    temp = c(1, temp[-length(temp)] )
    out[cumsum(temp)] = x
  } else {
    n = length(x)
    out = rep(NA_real_, (n-1)*nbw + 1 )
    out[ 1:n*(nbw+1)-nbw ] = as.numeric(x)
  }
  return( approx(out, xout=1:length(out))$y )
}



# make a function that does a rolling mean given a time window
rolltime = function(x,...) UseMethod("rolltime",x)
rolltime.default = function(x,y,width,fun=mean) {
  
  if(identical(fun,mean)) return( rollmean_fc(x, y, x, width) )
  out = x*NA
  for( i in seq_along(x) ) {
    window = x >= (x[i]-width/2) & x <= (x[i]+width/2)
    out[i] = fun( y[window] )
  }
  return(out)
}
rolltime.data.table = function( dt, width, fun=mean ) {
  
  stopifnot(ncol(dt)==2)
  x = dt[,1,with=FALSE][[1]]
  y = dt[,2,with=FALSE][[1]]
  ynew = rolltime(x,y,width,fun)
  out = dt[,list(x=x,y=ynew)]
  return(out)
}

# A very fast rolling mean with "INTERVAL"
# zoo::rollmean finds the "neighborhood" for the local mean by
# finding the "k" closest points. This function finds all the
# points that are distance "width" away from the point of interest
cppFunction('
  NumericVector rollmean_fc( NumericVector x, NumericVector y, 
                             NumericVector xout, double width) {
    double total=0;
    unsigned int n=x.size(), nout=xout.size(), i, ledge=0, redge=0;
    NumericVector out(nout);

    for( i=0; i<nout; i++ ) {
      while( x[ redge ] - xout[i] <= width && redge<n ) 
        total += y[redge++];
      while( xout[i] - x[ ledge ] > width && ledge<n ) 
        total -= y[ledge++];
      if( ledge==redge ) { out[i]=NAN; total=0; continue; }
      out[i] = total / (redge-ledge);
    }
    return out;
  }')






