
################################################################################
#                        Well search functions                                 #
################################################################################

# get the names of all the compounds from a compound matrix
compoundNames = function( x, type="all", withConcs=FALSE ) {
  
  nms = sort( if(type=="all") x$name else x$name[x$type==type] )
  if(withConcs) nms = paste( nms, 
                             round(x[ match(nms,x$name), "conc" ],3), 
                             sep="-")
  return(nms)
}

# wrapper function to search wellList AND return a smaller wellList
retrieveWells = function( wells, ... ) {
  locs = searchWells(wells,...)
  get.wells( wells, locs$location, locs$file )
}

# figure out which element of the list the desired well is
well.index = function( wells, location, file ) {
  
  files = getfiles(wells)
  locs = getlocations(wells) 
  
  idx = numeric(length(location))
  for( i in 1:length(idx) ) {
    idx.i = which(files == file[i] & locs == location[i])
    idx[i] = ifelse( length(idx.i), idx.i, NA )
  }
  return(idx)
}

# return the well given the location on the plate and file name
get.wells = function( wells, location, file ) {
  idx = well.index(wells, location, file)
  out = wells[idx]
  class(out) = class(wells)
  return( out )
}

# Matrix that contains all compound info for each solution. One row per solution.
compoundMatrix = function( solns ) {
  
  # get all the compound matrices in a list
  compounds = getcompounds(solns)
  
  # recast each compound matrix (one row per compound) to one row by making
  # "compound name"_"column names" the column names
  foo = function(x) {
    if(nrow(x)==0) {
      x = data.frame(name=NA,conc=NA,type=NA)
    }
    y = melt(x,id.var=c("name"))
    cast(y,~name+variable,id.var="name")[-1]
  }
  
  # and combine all of those new rows into one big data frame
  out = ldply( lapply(compounds, foo) )
  
  # get rid of columns with all NA values
  out = out[ , !apply( is.na(out), 2, all ) ]
  
  return(out)
}

# Matrix that contains all solvent info for each solution. One row per solution.
solventMatrix = function( solns ) {
  
  # get all of the solvent matrices in a list
  solvents = getsolvents(solns)
  
  # transpose solvent matrix so components are columns in one row
  foo = function(x) { rownames(x) = paste0("solvent_",rownames(x)); t(x) }
  
  # combine all the rows into a matrix
  out = ldply( lapply(solvents,foo) )
  return(out)
}

# get all info for all wells in a matrix. By default, "final" status used
wellList.matrix = function( wells, ID="final" ) {
  
  # well level information
  locs = getlocations(wells)
  files = getfiles(wells)
  
  # action level information
  ids = getIDs(wells, ID)
  solns = getsolutions( wells, ID )
  vols = getvolumes(solns)
  
  # Solution level information
  cm = compoundMatrix(solns)
  sm = solventMatrix(solns)
  
  # final matrix for easier searching
  data.frame( file=files, location=locs, 
              ID=ids, volume=vols, cm, sm, stringsAsFactors=FALSE)
}

# find all the wells in a well list that match the given criteria
searchWells = function( wells, file=NULL, location=NULL, ID="final",
                        max.volume=NULL, min.volume=NULL, 
                        compounds=NULL, 
                        min.concentrations=NULL,
                        max.concentrations=NULL,
                        solvents=NULL, 
                        min.solvent.pctgs=NULL,
                        max.solvent.pctgs=NULL,
                        controls=FALSE) {
  
  # only get wells that have the appropriate ID
  ids = getIDs(wells, ID)
  wells = wells[!is.na(ids)]
  
  # set up matrix for easier searching
  x = wellList.matrix(wells, ID)
  y = rep(TRUE,length(wells))
  
  # matching of atomic vectors
  if( !is.null(file) )  y = y & (x$file %in% file)
  if( !is.null(location) )  y = y & (x$location %in% location)
  if( ID != "final" ) y = y & (x$ID %in% ID)
  if( !is.bs(min.volume) ) y = y & (x$volume>min.volume)
  if( !is.bs(max.volume) ) y = y & (x$volume<max.volume)
  
  # and the compounds...
  if( all(!is.bs(compounds)) ) {
    cc = x[, paste0( compounds,"_conc"), drop=FALSE] # Compound Columns
    cc = as.data.frame( apply(cc,1:2,as.numeric) ) # convert char to numeric
    y = y & apply( !is.na(cc), 1, any )
  }
  
  # and the concentration limits...
  # (the order of concentration limits correspond to the order of the compounds)
  if( all(!is.bs(min.concentrations)) ) {
    cc[is.na(cc)] = 0
    y = y & apply( mapply(`>`,cc,min.concentrations), 1, any )
  }
  if( all(!is.bs(max.concentrations)) ) {
    cc[ is.na(cc) | cc==0 ] = Inf
    y = y & apply( mapply(`<`,cc,max.concentrations), 1, any )
  }
  
  # and the solvent components...
  if( all(!is.bs(solvents)) ) {
    sc = x[,paste0("solvent_",solvents ),drop=FALSE] # Solvent Columns
    sc = as.data.frame( apply(sc,1:2,as.numeric) ) # convert char to numeric
    y = y & apply( !is.na(sc), 1, any )
  }
  
  # and the solvent composition...
  if( all(!is.bs(min.solvent.pctgs)) ) {
    sc[is.na(sc)] = 0
    y = y & apply( mapply(`>`,sc,min.solvent.pctgs), 1, any )
  }
  if( all(!is.bs(max.solvent.pctgs)) ) {
    sc[is.na(sc)] = 0
    y = y & apply( mapply(`<`,sc,max.solvent.pctgs), 1, any )
  }
  
  # very specific case to return wells without any compounds of 
  # concentration type "start" or "final" but do have type "total"
  # the same as some of the wells found. Code is awful - can be cleaned later
  if( controls == TRUE)  y[ findControls(wells,y,x) ] = TRUE
  
  return( x[y,c("file","location")] )
}

# given wells and a logical vector for the wells of interest, find the 
# other wells that serve as controls
findControls = function( wells, y, x=NULL, samefile=FALSE ) {
  
  if(is.null(x)) x=wellList.matrix(wells)
  
  # if controls must come from the same file
  if(samefile) {
    file.rows = x$file %in% unique(x[y,"file"])
  } else {
    file.rows = rep(TRUE,length(x$file))
  }
  
  # find what compounds are in the searched wells
  cc = grep("_type",colnames(x),value=TRUE) # Compound Columns
  in.wells = apply( !is.na( x[y,cc] ), 2, any ) # Compounds in wells?
  ccw = cc[in.wells] # Compound Columns for compounds that are in Wells.
  
  # find which of those compounds are of concentration type "total"
  x.ccw = x[,ccw,drop=FALSE]
  ccw.tot.type = ccw[ apply( !is.na(x.ccw) & x.ccw=="total", 2, any ) ] # total columns in wells
  
  # find the number totals for the searched wells
  ccw.tot = str_replace_all( ccw.tot.type, "_type", "_conc" )
  conc.mat = data.frame(apply( x[y,ccw.tot,drop=FALSE], 1:2, as.numeric ))
  tots = lapply( as.list(conc.mat), function(x) unique(x[!is.na(x)] ) )
  
  # find all the wells that have the same number totals
  tots.same = as.data.frame( Map( `%in%`, x[,ccw.tot,drop=FALSE], tots ) )
  same.rows = apply(tots.same,1,any)
  
  # all other totals and all compounds must be blank
  cc.tot = cc[ apply( !is.na(x[,cc]) & x[,cc]=="total", 2, any ) ] # all total wells
  others = c( setdiff(cc,cc.tot), setdiff(cc.tot,ccw.tot.type) )
  blank.rows = apply( is.na(x[,others,drop=FALSE]), 1, all )
  
  return( file.rows & same.rows & blank.rows )
}

# Factor classifying wells according to the inputs of the function
factorWells = function( wells, by, 
                        ID="final", compoundType="start", withConcs=FALSE, ... ) {
  
  if( by=="file" ) return( getfiles(wells) )
  if( by=="location" ) return( getlocations(wells) )
  if( by=="volume" ) return( as.character(getvolumes(wells,ID)) )
  if( by=="mass" ) return( compound_masses_string(wells, ID=ID, ...) )
  
  # the compounds
  solns = getsolutions(wells,ID)
  sts = getsolvents(solns)
  
  if( by=="compounds" ) {
    cmps = getcompounds(solns)
    cmp.nms = lapply(cmps,compoundNames,type=compoundType,withConcs=withConcs)
    f = vapply(cmp.nms,paste0,"",collapse=".")
  }
  
  # the solvents
  sts = getsolvents(solns)
  if( by == "solvents" ) {
    sts.nms = lapply(sts,function(x) sort(rownames(x)))
    f = vapply(sts.nms,paste0,"",collapse=".")
  }
  if( by == "solvent.pctgs" ) {
    sts.nms = lapply(sts, function(x) paste(rownames(x),x,sep="-")  )
    f = vapply(sts.nms,paste0,"",collapse=",")
  }
  
  return(f)
}

# Factor classifying wells according to a shorthand
groupWells = function( wells, group="none", ID="final", ... ) {
  
  # by "simpler" categorizations
  if(group=="by.file") f = factorWells(wells,"file",ID)
  if(group=="by.well") f = factorWells(wells,"location",ID)
  if(group=="by.volume") f = factorWells( wells, by="volume", ID=ID )
  if(group=="by.mass") f = factorWells( wells, by="mass", ID=ID, ... )
  
  # by the compounds in the well
  compOptions = c("by.all.compounds","by.compounds","by.total.compounds" )
  if( group %in% compOptions )  {
    
    # which type of compounds?
    solns = getsolutions(wells,ID)
    cmps = getcompounds(solns)
    compTypes = c("all","start","total")
    type = compTypes[ compOptions==group ]
    f = factorWells( wells, by="compounds", ID=ID, compoundType=type )
  }
  
  # by concentrations (not total)
  if(group=="by.concentrations") {
    f = factorWells( wells, by="compounds", ID=ID,
                     compoundType="start", withConcs=TRUE)
  }
  
  # by concentrations (by total)
  if(group=="by.total.concentrations") {
    f = factorWells( wells, by="compounds", ID=ID,
                     compoundType="total", withConcs=TRUE)
  }
  
  # by solvents
  if(group=="by.solvents") f = factorWells( wells, by="solvents", ID=ID )
  if(group=="by.solvent.pctgs") f = factorWells( wells, by="solvent.pctgs", ID=ID )
  
  
  return(f)
}
