#' Get a well's data
#' 
#' Extract the data matrix from a well. The matrix contains
#' three columns: i, t, and value indicating the indices, times, and values
#' 
#' @param x well object
#' @export
wdata = function(x, ...) UseMethod("wdata",x)
#' @export
wdata.well = function(x) x$data


#' Change a well's data
#' 
#' Change the data for a well. The matrix contains
#' three columns: i, t, and value indicating the indices, times, and values
#' 
#' @param x well object
#' @export
"wdata<-" = function(x, ...) UseMethod("wdata<-",x)
#' @export
"wdata<-.well" = function(x, value) { x$data = value ; x }


#' Get the times for a well's data points
#' 
#' Extract a numeric vector of the times from well's data.
#' 
#' @param x well object
#' @export
tdata = function(x, ...) UseMethod("tdata", x)
#' @export
tdata.well = function(x) wdata(x)$t


#' Change the times for a well's data points
#' 
#' Given a numeric vector of the times from well's data, update
#' the data. This is usefull for time transformations.
#' 
#' @param x well object
#' @export
"tdata<-" = function(x, ...) UseMethod("tdata<-",x)
#' @export
"tdata<-.well" = function(x, value) { wdata(x) = `$<-`( wdata(x), "t", value); x}


#' Get the data values for a well
#' 
#' Extract a numeric vector of the values from a well's data.
#' The times and indices aren't given.
#' 
#' @param x well object
#' @export
vdata = function(x, ...) UseMethod("vdata", x)
#' @export
vdata.well = function(x) wdata(x)$value


#' Set the data values for a well
#' 
#' Use a numeric vector to set the values of a well
#' 
#' @param x well object
#' @export
"vdata<-" = function(x, ...) UseMethod("vdata<-",x)
#' @export
"vdata<-.well" = function(x, value) { wdata(x) = `$<-`( wdata(x), "value", value); x}


#' Get the indices for a well's data
#' 
#' Extract a numeric vector of the indices from a well's data.
#' 
#' @param x well object
#' @export
idata = function(x, ...) UseMethod("idata", x)
#' @export
idata.well = function(x) wdata(x)$i


#' Set the indices for a well's data
#' 
#' Use a numeric vector to change the indices of a well's data.
#' 
#' @param x well object
#' @export
"idata<-" = function(x, ...) UseMethod("idata<-",x)
#' @export
"idata<-.well" = function(x, value) { wdata(x) = `$<-`( wdata(x), "i", value); x}


#' Get time given index
#' 
#' Given a well and the i'th data point (the index), give the
#' corresponding time
#' 
#' @param x well object
#' @param i the index, an integer
i_t = function(x, ...) UseMethod("i_t", x)
i_t.default = function(x, ...) return(NA)
i_t.well = function(x, i) wdata(x)$t[ match(i, wdata(x)$i) ]


#' Get value given index
#' 
#' Given a well and the i'th data point (the index), give the
#' corresponding value
#' 
#' @param x well object
#' @param i the index, an integer
i_v = function(x, ...) UseMethod("i_v", x)
i_v.default = function(x, ...) return(NA)
i_v.well = function(x, i) wdata(x)$value[ match(i, wdata(x)$i) ]


#' Get index from action name
#' 
#' Given the name of an action and a well, return the index
#' at which the action occurred (or right before the action occurred).
#' The ID of the action is passed to action or actionList with the
#' ... arguments. If the ID isn't given, 
#' then a named numeric vector is returned where the names are the
#' names of the actions and the values are the indices.
#' 
#' @param x action, actionList, well, or wellList
#' @param ... arguments passed to action or actionList
ID_i = function(x,...) UseMethod("ID_i", x)
ID_i.default = function(x, ...) return(NA)
ID_i.action = function(x, ...) get_action(x, ...)$i
ID_i.actionList = function(x, ...) sapply( get_actionList(x, ...), ID_i )
ID_i.well = function(x, ...) sapply( get_actionList(x, ...), ID_i )
ID_i.wellList = function( x, ... ) sapply(x, ID_i, ... )


#' Get times from action name
#' 
#' Given the name of an action and a well, return the time
#' at which the action occurred (or right before the action occurred).
#' The ID of the action is passed to action or actionList with the
#' ... arguments. If the ID isn't given, 
#' then a named numeric vector is returned where the names are the
#' names of the actions and the values are the times.
#' 
#' @param x action, actionList, well, or wellList
#' @param ... arguments passed to ID_i
ID_t = function(x,...) UseMethod("ID_t", x)
ID_t.default = function(x, ...) return(NA)
ID_t.well = function(x, ...) i_t(x, ID_i(x, ...))
ID_t.wellList = function( x, ... ) sapply(x, ID_t, ... )


#' Get values from action name
#' 
#' Given the name of an action and a well, return the data value for the time
#' at which the action occurred (or right before the action occurred).
#' The ID of the action is passed to action or actionList with the
#' ... arguments. If the ID isn't given, 
#' then a named numeric vector is returned where the names are the
#' names of the actions and the values are the data values.
#' 
#' @param x action, actionList, well, or wellList
#' @param ... arguments passed to ID_i
ID_v = function(x,...) UseMethod("ID_v", x)
ID_v.default = function(x, ...) return(NA)
ID_v.well = function(x, ...) i_v(x, ID_i(x, ...))
ID_v.wellList = function( x, ... ) sapply(x, ID_v, ... )