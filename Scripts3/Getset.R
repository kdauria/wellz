
########### Search wells based on parameters
select = function(x, ...) UseMethod("select",x)
select.wellList = function(wells, ...) {
  yn = search(wells, ...)
  wells[which(yn)]
}
"select<-" = function(x, ...) UseMethod("select<-",x)
"select<-.wellList" = function(wells, value, ...) {
  yn = search(wells, ...)
  wells[which(yn)] = value
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

roster = function(wells) {
  data.frame( file=filename(wells), code=code(wells) ) 
}

# Don't lose class when subsetting
`[.wellList` = function( x, i ) {
  r = NextMethod("[")
  mostattributes(r) = attributes(x)
  r
}
c.wellList = function(x,...) {
  r = NextMethod("c")
  mostattributes(r) = attributes(x)
  r
}


####### access and set different elements of wells, wellLists, actions, and actionLists
code = function(x) UseMethod("code",x)
code.well = function(x) x[["code"]]
code.wellList = function(x) code_rcpp(x)
cppFunction('
  CharacterVector code_rcpp( List x ) {
            List temp;
            unsigned int n=x.size(), i;
            CharacterVector out(n);
            for( i = 0; i<n ; i++ ) {
            temp = as<List>(x[i]);
            out[i] = as<std::string>(temp["code"]);
            }
            return out;  }')
"code<-" = function(x,value) UseMethod("code<-",x)
"code<-.well" = function(x,value) x[["code"]] = value
"code<-.wellList" = function(x,value) code_replace_rcpp(x,value)
cppFunction('
  List code_replace_rcpp( List x, CharacterVector value) {
            List temp;
            unsigned int n=x.size(), i;
            List out(n);
            for( i = 0; i<n ; i++ ) {
              temp = as<List>(x[i]);
              temp["code"] = as<std::string>(value[i]);
              out[i] = temp;
            }
            out.attr("class") = x.attr("class");
            return out;  }')


filename = function(x) UseMethod("filename",x)
filename.well = function(x) x[["file"]]
filename.wellList = function(x) filename_rcpp(x)
cppFunction('
  CharacterVector filename_rcpp( List x ) {
            List temp;
            unsigned int n=x.size(), i;
            CharacterVector out(n);
            for( i = 0; i<n ; i++ ) {
            temp = as<List>(x[i]);
            out[i] = as<std::string>(temp["file"]);
            }
            return out;  }')
"filename<-" = function(x,value) UseMethod("filename<-",x)
"filename<-.well" = function(x,value) x[["file"]] = value
"filename<-.wellList" = function(x,value) filename_replace_rcpp(x,value)
cppFunction('
  List filename_replace_rcpp( List x, CharacterVector value) {
            List temp;
            unsigned int n=x.size(), i;
            List out(n);
            for( i = 0; i<n ; i++ ) {
            temp = as<List>(x[i]);
            temp["file"] = as<std::string>(value[i]);
            out[i] = temp;
            }
            out.attr("class") = x.attr("class");
            return out;  }')


index = function(x) UseMethod("index",x)
index.action = function(x) x[["i"]]
index.actionList = function(x) index_actionList_rcpp(x)
cppFunction('
  NumericVector index_actionList_rcpp( List x ) {
            List temp;
            unsigned int n=x.size(), i;
            NumericVector out(n);
            for( i = 0; i<n ; i++ ) {
              temp = as<List>(x[i]);
              out[i] = as<double>(temp["i"]);
            }
            return out;
            }')
"index<-" = function(x,value) UseMethod("i<-",x)
"index<-.action" = function(x,value) x[["i"]] = value
"index<-.actionList" = function(x,value) { for(i in seq_along(x)) x[[i]][["i"]] = value[i]; x }
cppFunction('
  List index_actionList_replace_rcpp( List x, NumericVector value) {
            List temp;
            unsigned int n=x.size(), i;
            List out(n);
            for( i = 0; i<n ; i++ ) {
              temp = as<List>(x[i]);
              temp["i"] = value[i];
              out[i] = temp;
            }
            out.attr("class") = x.attr("class");
            return out;  }')


solution = function(x) UseMethod("solution",x)
solution.action = function(x) x[["solution"]]
solution.actionList = function(x) lapply(x,"[[","solution")
solution.well = function(x) solution(x$actions)
"solution<-" = function(x,value) UseMethod("solution<-",x)
"solution<-.action" = function(x,value) x[["solution"]] = value
"solution<-.actionList" = function(x,value) { for(i in seq_along(x)) x[[i]][["solution"]] = value[[i]]; x }
"solution<-.well" = function(x,value) { for(i in seq_along(x$actions)) x$actions[[i]][["solution"]] = value[[i]]; x }

actionList = function(x) UseMethod("actionList",x)
actionList.well = function(x) x$actions
actionList.wellList = function(x) lapply(x,"[[","actions")
"actionList<-" = function(x,value) UseMethod("actionList<-",x)
"actionList<-.wellList" = function(x,value) { for(i in seq_along(x)) x[[i]][["actions"]] = value[[i]]; x }

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

compound_names = function(x,...) UseMethod("compound_names",x)
compound_names.Solution = function(x) x$compounds$name
compound_names.well = function(x,ID=length(x$actions),types="start") {
  x$actions[[ID]]$solution$compounds$name[x$actions[[ID]]$solution$compounds$type %in% types]
}
compound_names.wellList = function(x,...) {
  unique(unlist(lapply(x,compound_names,...)))
}








