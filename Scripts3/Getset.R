
########### Search wells based on parameters
select = function(x, ...) UseMethod("select",x)
select.wellList = function(wells, ...) {
  yn = search(wells, ...)
  wells[[which(yn)]]
}
"select<-" = function(x, ...) UseMethod("select<-",x)
"select<-.wellList" = function(wells, value, ...) {
  yn = search(wells, ...)
  wells[[which(yn)]] = value
  wells
}

search = function(x,...) UseMethod("search",x)
search.wellList = function(wells,filename=NULL,code=NULL) {
  
  # filename & code
  yn = rep(TRUE,length(wells))
  if(!is.null(filename)) yn = yn & filename(wells) %in% filename
  if(!is.null(code)) yn = yn & code(wells) %in% code
  
  yn
}

####### access and set different elements of wells, wellLists, actions, and actionLists
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

index = function(x) UseMethod("index",x)
index.action = function(x) x[["i"]]
index.actionList = function(x) vapply(x,"[[",1,"i")
"index<-" = function(x,value) UseMethod("i<-",x)
"index<-.action" = function(x,value) x[["i"]] = value
"index<-.actionList" = function(x,value) { for(i in seq_along(x)) x[[i]][["i"]] = value[i]; x }

solution = function(x) UseMethod("solution",x)
solution.action = function(x) x[["solution"]]
solution.actionList = function(x) lapply(x,"[[","solution")
solution.well = function(x) solution(x$actions)
"solution<-" = function(x,value) UseMethod("solution<-",x)
"solution<-.action" = function(x,value) x[["solution"]] = value
"solution<-.actionList" = function(x,value) { for(i in seq_along(x)) x[[i]][["solution"]] = value[[i]]; x }
"solution<-.well" = function(x,value) { for(i in seq_along(x$actions)) x$actions[[i]][["solution"]] = value[[i]]; x }

actionList = function(x) UseMethod("actionList",x)
actionList.wellList = function(x) vapply(x,"[[","","actions")
"actionList<-" = function(x,value) UseMethod("actionList<-",x)
"actionList<-.wellList" = function(x,value) { for(i in seq_along(x)) x[[i]][["actions"]] = value[[i]]; x }

filename = function(x) UseMethod("filename",x)
filename.well = function(x) x[["file"]]
filename.wellList = function(x) vapply(x,"[[","","file")
"filename<-" = function(x,value) UseMethod("filename<-",x)
"filename<-.well" = function(x,value) x[["file"]] = value
"filename<-.wellList" = function(x,value) { for(i in seq_along(x)) x[[i]][["file"]] = value[i]; x }

ID = function(x) UseMethod("ID",x)
ID.action = function(x) x[["ID"]]
ID.actionList = function(x) vapply(x,"[[","","ID")
"ID<-" = function(x,value) UseMethod("ID<-",x)
"ID<-.action" = function(x,value) x[["ID"]] = value
"ID<-.actionList" = function(x,value) { 
  for(i in seq_along(x)) x[[i]][["code"]] = value[i]
  names(x) = value
  x
}



####### Custom selector functions
# custom.df.selector = function(x, i=NULL, j=NULL, class) {
#   class(x) = "data.frame"
#   if( !is.null(i) & is.null(j) ) {
#     y = x[i,]
#     class(y) = c(class,"data.frame")
#   } else if( is.null(i) & !is.null(j) ) {
#     y = x[,j]
#   } else {   
#     y = x[i,j]
#     if( (is.numeric(j) && length(j)==ncol(x)) ||
#           (is.logical(j) && sum(j)==ncol(x)) ) {
#       class(y) = c(class,"data.frame")
#     }
#   }
#   y
# }
# `[.well` = function( x, i=NULL, j=NULL ) {
#   custom.df.selector(x, i, j, "well")
# }
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


