print.wellList = function( wells, printall=FALSE, IDw="last" ) {
  
  files = filename(wells)
  if( unique(files)==1 || length(wells)<20 || printall ) {
    
    out = list()
    out$file = files
    out$loc = code(wells)
    
    solns = lapply(wells, function(x) solution(x,ID=IDw))
    out$compounds = sapply(solns, compound_string, type="start")
    
    out$compounds[ sapply(solns,is.null) ] = paste0("No", IDw, " ID")
    
    out$total = sapply(solns, compound_string, type="total", wConc=FALSE)
    as.data.frame(out)
    
  } else {
    r = data.frame( roster(wells), w=1:length(wells) )
    rs = split( r$w, r$file )
  
    ranges = sapply(rs,text_ranges)
    comps = sapply( rs, function(x) paste(compound_names(wells[x]),collapse=", ") )
    cat("A 'wellList' object \n")
    print(data.frame(i=ranges,compounds=comps))
  }
}

print.well = function( well ) {
  
  cat("File:", well$file, "\n")
  cat("Location:", well$code, "\n")
  cat(length(well$actions), "actions")
  #print(well$actions)
  
  cat("\n\n")
  cat("Final solution:\n")
  print( solution(well,ID="last") )
  
  cat("\n\n")
  cat( nrow(well$data), "data points\n" )
  print_well_data(well$data)
}

print_well_data = function(data) {
  if( nrow(data) > 10 ) {
    top = data[1:5,]
    bottom = tail(data,5)
    print( rbind(top,"---",bottom), row.names=FALSE)
  } else {
    print(data, row.names=FALSE )
  }
}

# x = c(100.1, 1.23, 1232312.3333, 0.000003, 0.0101001, 1 )
# floor(log10(x))


# summarise the compounds and concentrations into one string
compound_string = function(soln, type="all", wConc=TRUE ) {
  
  yn = switch(type,
    all = rep(TRUE,nrow(soln$compounds)),
    total = soln$compounds$type == "total",
    start = soln$compounds$type == "start" )
  
  nms =  soln$compounds$name[yn]
  if( wConc && length(nms) ) {
    concs = format1( soln$compounds$conc[yn] )
    out = paste0( nms, " [", concs, "]", collapse=", " )
  } else if( length(nms) ) {
    out = paste0( nms, collapse=", " )
  } else {
    out = ""
  }
  out
}
solvent_string = function(soln) {
  if( nrow(soln$solvent)==0 ) return ("")
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
  out$rmVol = sapply(actionlist,"[[","rmVol")
  if( all(out$rmVol==0) ) out$rmVol=NULL
  
  # Subtract solutions to figure out what was added
  # at each action
  solns = solution(actionlist)
  if(length(solns)>1) 
    solns[-1] = Map("-",solns[-1], solns[-length(solns)])
  
  out$adVol = sapply(solns,"[[","volume")
  out$compounds = vapply( solns, compound_string, "" )
  out$solvent = vapply( solns, solvent_string, "" )
  if( length(unique(out$solvent))==1 ) out$solvent=NULL
  
  out = as.data.frame(out)
  print(out,row.names=FALSE)
}



