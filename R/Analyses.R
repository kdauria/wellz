#' Find maximum rate of curve
#' 
#' This finds the maximum derivative within boundaries
#' that can be defined
#' 
#' \code{xlim} and \code{ylim} define a bounding box
#' in which the highest derivative will be found.
#' \code{pgon} is a data frame where each row
#' represents the (x,y) coordinates of a polygon's contiguous
#' vertices. The derivative must be found from the data
#' within this polygon. If \code{direction="any"}, the largest
#' rate is returned regardless if it is positive or negative.
#' The larges positive and negative rates can be returned instead
#' by changin the \code{direction} argument.
#' 
#' @param well a \code{wellList} object
#' @param xlim a 2-element \code{numeric}
#' @param ylim a 2-element \code{numeric}
#' @param pgon a 2-column \code{data.frame}
#' @param direction either \code{"any"}, \code{"positive"}, or \code{"negative"}
#' @export
max_rate = function( x, ...) UseMethod("max_rate",x)
#' @export
max_rate.well = function( well, xlim=NULL, ylim=NULL, pgon=NULL, direction="any" ) {
  well.deriv = insert_n_between_spline(well, type="smoother", deriv=1, n=5)
  well = insert_n_between_spline(well, n=5)
  yn = is_in_space(well, xlim=xlim, ylim=ylim, pgon=pgon)
  
  row.num = switch(direction,
         any=which.max( abs(vdata(well.deriv)[yn]) ),
         positive=which.max( vdata(well.deriv)[yn] ),
         negative=which.min( vdata(well.deriv)[yn] ))
  max.row = wdata(well)[yn,][row.num,]
  max.row$rate = vdata(well.deriv)[yn][row.num]
  max.row$file = filename(well.deriv)
  max.row$location = code(well.deriv)
  max.row = max.row[,c( -1:0+ncol(max.row), 1:(ncol(max.row)-2)) ] # move columns around
  max.row
}
#' @export
# I need to work out this groups and rbindlist thing. It's a little confusing
# right now. A period is appended to each argument name here because
# the argument will be eventually called by another function.
# For another example of this error, see http://stackoverflow.com/questions/4357101/
# Since R automatically matches arguments, this should have no
# practical effect
max_rate.wellList = function( x, groups=NULL,  xlim.=NULL, 
                              ylim.=NULL, pgon.=NULL, direction.="any",  ...) {
  
  rates = vector(length=length(x), mode="list")
  rates = lapply(x,max_rate, xlim=xlim., ylim=ylim., pgon=pgon., direction=direction.)
  rates = rbindlist( rates )
  for( g in groups ) rates[[g]] = group(x, g, ...)
  rates
}

#' Find area under curve
#' 
#' Finds the area under a curve given upper and lower bounds of the x-axis.
#' 
#' @import pracma
#' @param x a \code{well} or \code{wellList} object
#' @param lower a \code{numeric} vector
#' @param upper a \code{numeric} vector
#' @export
area_under_curve = function(x, ...) UseMethod("area_under_curve", x)
#' @export
area_under_curve.well = function(x, lower, upper ) {
  fun = x$spline
  area = tryCatch( integrate(fun,lower,upper,subdivisions=5000)$value,
                   error = function(e) {
                     message(str_c("stats::integrate ",e$message) )
                     message("Will instead calculate the integral by the trapezoidal rule" )
                     x.s = slice(x,xlim=c(lower,upper))
                     trapz( tdata(x.s), vdata(x.s))
                   } )
  data.frame( file=filename(x), location=code(x), lower=lower, upper=upper, area=area)
}
#' @export
area_under_curve.wellList = function(x, lower, upper, groups=NULL, ...) {
  areas = rbindlist( lapply( x, area_under_curve, lower=lower, upper=upper))
  for( g in groups ) areas[[g]] = group(x, g, ...)
  areas
}


#############################################################
#           Functions to support analyses                   #
#############################################################

# points is a two-column matrix of 2D points
# pgon is a two-column matrix of a polygon's vertices
# xlim and ylim are two-element vectors
# outside.pgon=TRUE means that the data must be outside of the polygon
is_in_space = function(x, ...) UseMethod("is_in_space",x)
is_in_space.default = function(points, xlim=NULL, ylim=NULL, pgon=NULL, outside.pgon=FALSE) {
  yn = rep(TRUE, nrow(points))
  if(!is.null(ylim))
    yn = yn & points[,2] < ylim[2] & points[,2] > ylim[1]
  if(!is.null(xlim))
    yn = yn & points[,1] < xlim[2] & points[,1] > xlim[1]
  if(!is.null(pgon) & !outside.pgon )
    yn = yn & is_in_polygon( points, pgon)
  if(!is.null(pgon) & outside.pgon )
    yn = yn & !is_in_polygon( points, pgon)
  yn 
}
is_in_space.well = function( well, ...) {
  points = cbind(tdata(well),vdata(well))
  is_in_space(points, ...)
}

# pgon is a two-column matrix of a polygon's vertices
# points is a two-column matrix of 2D points
is_in_polygon = function(x,...) UseMethod( "is_in_polygon", x)
is_in_polygon.default = function(points, pgon) {
  colnames(pgon) = colnames(pgon) = c("x","y")
  points = SpatialPoints(points)
  pgon = SpatialPolygons(list(Polygons(list(Polygon(pgon)),1)))
  over(points, pgon)
}
is_in_polygon.well = function(well, ...) {
  points = cbind(tdata(well),vdata(well))
  is_in_polygon( points, ... )
}

# Add a polygon to a ggplot
# this function is necessary because I don't want
# this layer to inherit aesthetics from the parent plot
# Note all the NA values
# plot(well) + gg_polygon(pgon)
gg_polygon = function( pgon, color="black", alpha=0.2 ) {
  colnames(pgon) = c("x","y")
  geom_polygon(data=pgon, aes(x=x,y=y, group=NA, size=NA, 
                              color=NA, linetype=NA, shape=NA), 
               fill=color, alpha=alpha)
}















