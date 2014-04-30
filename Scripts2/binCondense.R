# Functions to bin data given the number of bins
# Note that setting "width" does not set the bandwidth to
# exactly "width". It makes several equi-width
# bins with width closest to "width"
bin = function(x,...) UseMethod("bin",x)
bin.default = function( x, nbins=NULL, width=NULL ) {
  
  # checking inputs
  stopifnot( xor( is.null(nbins), is.null(width) ) )
  if(is.null(nbins)) nbins = ceiling(diff(range(x))/width)
  
  # return essentially a data table of the input if bins are
  # not requested (i.e. width=0). Else, do the binning
  if( !is.null(width) && width==0 ) {
    out = data.table(x,bins=1:length(x),key="x,bins")
  } else {
    breaks = seq( from=x[1], to=last(x), length.out=nbins+1 )
    bins = cut(x,breaks,include.lowest=TRUE,labels=FALSE)
    out = data.table(x,bins,key="x,bins")
  }
  class(out) = c("binned",class(out))
  return(out)
}
bin.factor = function( x, ... ) {
  levels(x) = as.numeric(levels(x))
  x = as.numeric(x)
  bin( x, ... )
}
bin.data.table = function( dt, binvar, ... ) {
  setkeyv(dt,binvar)
  bins = bin( dt[,binvar,with=FALSE][[1]], ... )
  out = dt[bins]
  class(out) = c("binned",class(out))
  return(out)
}
bin.data.frame = function( df, binvar, ... ) {
  dt = data.table(df,key=binvar)
  bin( dt, binvar, ... )
}

# Functions to condense data.tables with a "bins" column
# Also call the bin function if the data is not already binned
condense = function(x, ...) UseMethod("condense",x)
condense.binned = function( dt, fun=mean ) {
  if(identical(fun,mean)) fun = function(x) .Internal(mean(x))
  dt[,lapply(.SD,fun),by=bins]
}
condense.default = function( x, y, fun=mean, ... ) {
  data = data.table(x=x,y)
  condense( bin(data, binvar="x", ...), fun )
}
condense.data.frame = function( df, binvar="bins", fun=mean, ... ) {
  condense( bin( df, binvar, ... ), fun )
}



