#' Compress well data for plotting
#' 
#' Removes data points that couldn't possibly be displayed
#' because of overlap and limited screen space.
#' 
#' This function loops throught the data, removing points
#' that are so close to an adjacent point that they coudn't be
#' seen anyways. The width of the plot is found from
#' \code{dev.size()}. This function is fairly slow because
#' it uses loops in R. It wouldn't be too difficult to speed this
#' up with Rcpp.
#' 
#' @param x a \code{well} or \code{wellList} object
#' @export
compress_data = function(x,...) UseMethod("compress_data",x)
#' @export
compress_data.well = function(well) {
  
  image.dim.px = dev.size(units="px")
  data.dim = c( x=diff(range(tdata(well))),
                y=diff(range(vdata(well))))
  pix.size = data.dim / image.dim.px
  
  x = tdata(well)
  y = vdata(well)
  idx = idata(well)
  
  newx = numeric(length(x))
  newy = numeric(length(y))
  newidx = numeric(length(y))
  newx[1] = x[1]
  newy[1] = y[1]
  new.i = 1
  
  for( i in 1:length(x) ) {
    xdiff = x[i] - newx[new.i]
    ydiff = y[i] - newy[new.i]
    if( xdiff>pix.size[1] | ydiff>pix.size[2] ) {
      new.i = new.i + 1
      newx[new.i] = x[i]
      newy[new.i] = y[i]
      newidx[new.i] = idx[i]
    } 
  }
  wdata(well) = data.frame(i=newidx[1:new.i],
                           t=newx[1:new.i],
                           value=newy[1:new.i])
  well
}
#' @export
compress_data.wellList = function(x) {
  for( i in seq_along(x) )
    x[[i]] = compress_data(x[[i]])
  x
}