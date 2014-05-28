#### Find wells that match the requested parameters
search = function(x,...) UseMethod("search",x)
search.wellList = function(x,compstr=NULL,filename=NULL,code=NULL,ID="last",controls=FALSE) {
  
  # filename & code
  yn = rep(TRUE,length(x))
  if(!is.null(filename)) yn = yn & filename(x) %in% filename
  if(!is.null(code)) yn = yn & code(x) %in% code
  if(ID!="last") yn = yn & sapply( ID(x), function(x) ID %in% x )
  if(!is.null(compstr)) {
    bounds = parse_comp_str( compstr )
    yn = yn & sapply(x, match_well_string, compstr, bounds, ID)
  }
  if(controls) {
    files = filename(x)
    concs = concentration(x,type="start",ID=ID)
    yn[ concs %in% "" & files %in% unique(files[yn]) ] = TRUE
  }
  yn
}


# Tests
# t1 = "TcdA"
# t2 = "TcdA OR TcdB"
# t3 = "TcdA AND TcdB"
# t4 = "TcdA [1-100]"
# t5 = "TcdA[1-100]"
# t6 = "TcdA[1]"
# t7 = "TcdA [1-100] OR TcdB [1-10]"
# t8 = "TcdA AND TcdB [1-10]"
# t9 = "TcdA AND TcdB OR IL8"

# Parses a concentration string and returns a list
# with the names and bounds of each compound.
parse_comp_str = function( comp.string ) {
  # get string between each boolean operator
  sp = "[[:space:]]*"
  pattern = paste0(sp,"[[:alnum:]\\-\\.\\_]+",sp,
                   "\\[*",sp,"[[:digit:][:space:]\\.-]*","\\]*",sp)
  matches = str_match_all(comp.string, pattern)[[1]]
  comps = gsub("[[:space:]]","",matches)
  
  # split the names and concenctrations of each string
  comp = lapply( comps, function(x) strsplit(x,"\\s*\\[\\s*")[[1]])
  
  # get the compound name in each string
  nms = sapply( comp, "[", 1)
  
  # get the range of concs for each string
  ranges = sapply( comp, function(x) sub("\\]","", x[2]) )            
  range_to_bounds = function(x) as.numeric( strsplit(x,"\\s*-\\s*")[[1]] )
  bounds = lapply( ranges, range_to_bounds )
  bounds = lapply(bounds, function(x) 
    if( length(x)==1 ) { if(!is.na(x)) rep(x,2) else rep(NA_real_,2) } else x )
  
  # put the names and bounds in an easy data.frame
  bounds.df = data.frame(name=nms,
                         left=sapply(bounds,"[",1),
                         right=sapply(bounds,"[",2),
                         stringsAsFactors=FALSE)
  bounds.df$left[is.na(bounds.df$left)] = -Inf
  bounds.df$right[is.na(bounds.df$right)] = Inf
  
  return( bounds.df )
}

# Take a concentration string (s) and the results
# from parse_comp_str (bounds) to to see if the string
# is selecting the well
match_well_string = function( well, s, bounds = parse_comp_str(s), ID="last" ) {

  out = logical(nrow(bounds))
  sn = solution(well,ID=ID)
  if( length(sn)==1 && is.na(sn) ) return(FALSE)
  
  # figure out which part of the bounds are satisfied
  for( i in 1:nrow(bounds) ) {
    nm = bounds$name[i]
    if( nm %in% sn$compounds$name ) {
      conc = sn$compounds[ sn$compounds$name==nm, "conc" ]
      lbound = bounds$left[i]
      rbound = bounds$right[i]
      out[i] = conc >= lbound & conc <= rbound
    } else {
      out[i] = FALSE
    }
  }
  
  # Replace the compounds with the TRUE/FALSE if they matched the well
  sp = "[[:space:]]*"
  pattern = paste0(sp,"[[:alnum:]\\-\\.\\_]+",sp,
                   "\\[*",sp,"[[:digit:][:space:]\\.-]*","\\]*",sp)
  notmatched = strsplit(s,pattern)[[1]]
  if( length(notmatched)==length(out) ) {
    eval.string = paste0(notmatched,out,collapse="")
  } else if( length(notmatched)==length(out)+1) {
    eval.string = paste0(notmatched[1],paste0(out,notmatched[-1],collapse=""),collapse="")
  }
  eval(parse(text=eval.string))
}






