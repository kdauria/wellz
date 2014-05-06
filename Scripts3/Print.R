print.wellList = function( wells ) {
  r = data.frame( roster(wells), w=1:length(wells) )
  rs = split( r$w, r$file )
  
  allfiles = filename(wells)
  files = unique(allfiles)
  right = cumsum(table(allfiles))
  ranges = paste(right-right[1]+1, right, sep="-")
  comps = vapply( rs, function(x) paste(compound_names(wells[x]),collapse=", "), "" )
  cat("A 'wellList' object \n")
  print(data.frame(i=ranges,compounds=comps))
}

# print.well = function( well ) {
#   
#   
#   
# }


