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
# with the names and bounds of each compound. They
# are linked with booleans which are also returned
parse_comp_str = function( comp.string ) {
  # get string between each boolean operator
  comps = strsplit( comp.string, "\\s*(\\||\\&)\\s*" )[[1]]
  
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
  
  # get the booleans separating compound strings
  matches = gregexpr( "(\\||\\&)", comp.string )[[1]]
  match.length = attributes(matches)$match.length
  bools = sapply(match.length, function(x) if(x==3) "AND" else if(x==2) "OR" else NA_character_ )
  
  return( list( bounds=bounds.df, bools=bools) )
}

IDw = "last"

subset = select(wells,file="HCT8-t3.txt")
s = "TcdA[1-10] | TcdB[1] | gdTcdB[10-100]"
limits = parse_comp_str(s)
bounds = limits$bounds

out = logical(nrow(bounds))

well = subset[[1]]
sn = solution(well,ID=IDw)

# figure out which part of the bounds are satisfied
# in the solution
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

# do the boolean logic with the matched parts
bools = limits$bools
for( i in 1:length(bools) ) {
  if( bools[i] == "OR" ) {
    out[i+1] = out[i] | out[i+1]
  } else if( bools[i] == "AND" ) {
    
  } else {
    stop("unknown or no boolean operator")
  }
  out[i]
}



# select wells based off of the search.wellList function
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