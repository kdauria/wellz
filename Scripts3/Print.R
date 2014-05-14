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


# summarise the compounds and concentrations into one string
compound_string = function(soln) {
  if( nrow(soln$compounds) ) {
    out = paste0( soln$compounds$name, " [", soln$compounds$conc, "]", collapse=", " )
  } else {
    out = ""
  }
  out
}
solvent_string = function(soln) {
  paste0( soln$solvent$perc, "% ", soln$solvent$name, collapse=", " )
}

print.Solution = function( soln ) {
  cat("Volume: ", soln$volume, "\n")  
  if( length(compound_names(soln)) ) cat( compound_string(soln), "\n" )
  cat( solvent_string(soln) )
}

print.actionList = function( actionlist ) {
  
  out = list()
  out$ID = ID(actionlist)
  out$i = index(actionlist)
  out$rmVol = vapply(actionlist,"[[",1,"rmVol")
  if( all(out$rmVol==0) ) out$rmVol=NULL
  
  # Subtract solutions to figure out what was added
  # at each action
  solns = solution(actionlist)
  if(length(solns)>1) 
    solns[-1] = Map("-",solns[-1], solns[-length(solns)])
  
  # Add the volume of the solutions
  out$adVol = vapply(solns,"[[",1,"volume")
  out$compounds = vapply( solns, compound_string, "" )
  out$solvent = vapply( solns, solvent_string, "" )
  if( length(unique(out$solvent))==1 ) out$solvent=NULL
  
  out = as.data.frame(out)
  print(out,row.names=FALSE)
}



