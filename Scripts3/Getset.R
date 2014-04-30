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