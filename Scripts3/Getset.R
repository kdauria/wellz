
########### Search wells based on parameters
select = function(x, ...) UseMethod("select",x)
select.well = function(well, ...) {
  yn = search(well, ...)
  well[ yn ,]
}
"select<-" = function(x, ...) UseMethod("select<-",x)
"select<-.well" = function(well, value, ...) {
  yn = search(well, ...)
  well[yn,] = value
}

search = function(x,...) UseMethod("search",x)
search.well = function(well,filename=NULL,code=NULL) {
  
  # filename & code
  yn = rep(TRUE,nrow(well))
  if(!is.null(filename)) yn = yn & well$file %in% filename
  if(!is.null(code)) yn = yn & well$code %in% code
  
  yn
}

####### access and set different elements of wells and wellLists
code = function(x) UseMethod("code",x)
code.well = function(x) x[["code"]]
code.wellList = function(x) vapply(x,"[[","","code")
"code<-" = function(x,value) UseMethod("code<-",x)
"code<-.well" = function(x,value) x[["code"]] = value
"code<-.wellList" = function(x,value) { for(i in seq_along(x)) x[[i]][["code"]] = value[i]; x }

filename = function(x) UseMethod("filename",x)
filename.well = function(x) x[["file"]]
filename.wellList = function(x) vapply(x,"[[","","file")
"filename<-" = function(x,value) UseMethod("filename<-",x)
"filename<-.well" = function(x,value) x[["file"]] = value
"filename<-.wellList" = function(x,value) { for(i in seq_along(x)) x[[i]][["file"]] = value[i]; x }


####### Custom selector functions
custom.df.selector = function(x, i=NULL, j=NULL, class) {
  class(x) = "data.frame"
  if( !is.null(i) & is.null(j) ) {
    y = x[i,]
    class(y) = c(class,"data.frame")
  } else if( is.null(i) & !is.null(j) ) {
    y = x[,j]
  } else {   
    y = x[i,j]
    if( (is.numeric(j) && length(j)==ncol(x)) ||
          (is.logical(j) && sum(j)==ncol(x)) ) {
      class(y) = c(class,"data.frame")
    }
  }
  y
}
`[.well` = function( x, i=NULL, j=NULL ) {
  custom.df.selector(x, i, j, "well")
}
# `[.Action` = function( x, i=NULL, j=NULL ) {
#   custom.df.selector(x, i, j, "Action")
# }
# `$.Action` = function( x, nm ) {
#   class(x) = "data.frame"
#   if(nrow(x)==1) {
#     return( x[[nm]][[1]] )
#   } else {
#     return( x[[nm]] )
#   }
# }
# `$<-.Action` = function( x, nm, value ) {
#   class(x) = "data.frame"
#   if(nrow(x)==1 && nm=="solution") {
#     x[[nm]] = I(list(value))
#   } else {
#     x[[nm]] = value
#   }
#   class(x) = c("Action","data.frame")
#   x
# }


