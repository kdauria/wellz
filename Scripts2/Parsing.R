################################################################################
#                            Parsing functions                                 #
################################################################################

# Parse the metadata file and data files into a single wellList object
# Wrapper for several smaller parsing functions
parse.RTCAanalyze = function( metadata, data.dir ) {
  
  # parse the metadata text files
  f = parse.metadata.file( metadata )
  
  # convert the metadata data.frame to a list of RTCAaction objects
  a = df.to.actions(f$actions)
  
  # convert actions to timelines for each well
  wells = actions.to.wells( a )
  class(wells) = c("wellList","list")
  
  # add the data to the wells
  wells = add.data.to.wells(wells, data.dir )
  
  # add splines that can interpolate the data in the wells
  wells = add_interpolating_spline(wells)
  
  return(wells)
}


# wrapper calling all parsing functions for a metadata file
parse.metadata.file = function( fpath ) {
  
  sections = csv.to.metadata( fpath )
  file.metadata = sections[[1]]
  actions.metadata = sections[[2]]
  actions.metadata = fill.in.actions(actions.metadata)
  actions.metadata = expand.shorthand.RTCAinput(actions.metadata)
  return(list(actions=actions.metadata,files=file.metadata))
}

# separate csv file into data.frames describing actions and files
csv.to.metadata = function( filepath ) {
  
  metadata = read.csv(filepath ,header=FALSE, as.is=TRUE)
  if( ncol(metadata)==1 ) {
    metadata = read.csv(filepath ,header=FALSE, as.is=TRUE, sep="\t")
  }
  
  # separate the FILES section from the ACTIONS section
  borders = which( metadata[,1] %in% c("FILES","ACTIONS") )
  sections = list( metadata[ (borders[1]+1):(borders[2]-1),  ],
                   metadata[ (borders[2]+1):nrow(metadata),]     )
  sections = lapply(sections, rm.empty.rows.columns )
  sections = lapply(sections, row.as.header)
  
  return( list( file.metadata = sections[[1]], 
                actions.metadata = sections[[2]] ) )
}

# Fill in the empty columns of an "action" row, given a actions metadata data.frame
fill.in.actions = function( x ) {
  
  # If a row doesn't have "Well" defined, then it's describing the solution
  # of the action row above it. Action row = arow. Solution row = srow.
  arows = which( !is.bs(x$wells) )
  srows = which( is.bs(x$wells) )
  
  # fill in the action rows
  for( i in setdiff(seq_along(arows),1) ) { 
    rownum = arows[i]
    past.rownum = arows[i-1]
    empty.columns = which( x[ rownum, ] == "" )
    x[rownum, empty.columns] = x[past.rownum, empty.columns]
  }
  
  # fill in the rows describing solutions
  for( rownum in srows ) { 
    fillvars = c("file","name","conc","type","solvent")
    empty.columns = subset(fillvars, is.bs(x[rownum,fillvars]) )  
    x[rownum, empty.columns] = x[rownum-1, empty.columns]
  }
  
  return(x)
}

# return a list describing which rows correspond to each action and vice versa
actions.to.rows = function( x ) {
  
  wellrows = !is.bs( x$wells )
  rows.of.action = split( 1:nrow(x), cumsum(wellrows)  )
  nactions=length(rows.of.action)
  return( list( wellrows=wellrows,
                rows.of.action=rows.of.action,
                nactions=nactions) )
}

# Expand comma separated values to multiple rows
expand.shorthand.RTCAinput = function( df ) {
  
  a = actions.to.rows(df)
  rows = split(df,cumsum(a$wellrows))
  out = ldply(rows,expand.action)
  return(out[,setdiff(names(out),".id")])
}

# expand one "action" according to the number of wells and comma separated values
expand.action = function(x) {
  
  # setup the expanded data.frame
  wells = expand.wells(x$wells[1])
  nwells = length(wells)
  newdf = x[ rep(1:nrow(x),nwells), ]
  inputrows = function(rnum) (0:(nwells-1))*nrow(x)  + rnum
  newdf$wells[ inputrows(1) ] = wells
  if( nrow(newdf)==1 ) return(newdf)
  
  # for each original row, expand comma separated columns 
  for( i in 1:nrow(x)) {
    row = as.character(newdf[i,])
    comma.cols = grepl(",",row)
    
    if(any(comma.cols)) {
      mysplit = function(x) str_trim(unlist(str_split(x,",")))
      split.df = matrix( mysplit(row[comma.cols]) , nrow=nwells )
      newdf[ inputrows(i),comma.cols ] = split.df
    }
  }
  return(newdf)
}

# Expands the shorthand to describe wells.
# e.g., "[A-C][1-2]" becomes a vector: "A01","B01","C01","A02","B02","C02"
expand.wells = function( wellString) {
  
  if( is.bs(wellString) ) stop("Blank string cannot be converted to a well")
  
  # step 1: splitting comma separated notations
  cs.nots = str_trim(str_split(wellString,",")[[1]]) # get comma separated well notations
  
  letter.part = as.list(str_extract(cs.nots,"[A-H]-*[A-H]*"))
  number.part = as.list(str_extract(cs.nots,"[0-9]+-*[0-9]*"))
  
  # expand the letter part if there are dashes
  for( i in 1:length(letter.part) ) {
    if( grepl("-",letter.part[[i]])  ) {
      lets = str_extract_all(letter.part[[i]], "[A-H]")[[1]]
      bounds = which(LETTERS %in% lets)
      letter.part[[i]] = LETTERS[bounds[1]:bounds[2]]
    }
  }
  
  # expand the number part if there are dashes
  for( i in 1:length(number.part) ) {
    if( grepl("-",number.part[[i]])  ) {
      bounds = as.numeric(str_extract_all(number.part[[i]], "[0-9]+")[[1]])
      nums = bounds[1]:bounds[2]
      number.part[[i]] = str_pad( as.character(nums), width=2, pad="0" )
    } else {
      number.part[[i]] = str_pad( number.part[[i]], width=2, pad="0" )
    }
  }
  
  # cross-combine the letter and number part
  ws = vector("list",length=length(letter.part))
  for( i in 1:length(letter.part) ) {
    outer( letter.part[[i]], number.part[[i]], FUN="paste0" )
    ws[[i]] = do.call(paste0,expand.grid(letter.part[[i]],number.part[[i]]))
  }
  return( unlist(ws) )
}

# parse an action in a data.frame to an RTCAaction object
dataframe.to.RTCAaction = function( rows ) {
  
  # hardcoding of columns
  recording.cols = c("file","wells")
  compound.cols = c("name","conc","type","solvent")
  volume.col = "adVol"
  action.cols = setdiff( names(rows),c(recording.cols,compound.cols,volume.col) )
  numeric.cols = c("t1","t2","rmVol","adVol","conc")
  logical.cols = NULL
  
  # converting text to other formats
  rows[,numeric.cols] = suppressWarnings( apply(rows[,numeric.cols],1:2,as.numeric) )
  rows[,logical.cols] = apply(rows[,logical.cols],1:2,as.logical)
  
  # take care of the solvent "slot" in the solution
  soln = list()
  comps = rows[,compound.cols]
  soln$solvent = matrix(100)
  rownames(soln$solvent) = comps$solvent[1]
  colnames(soln$solvent) = "pctg"
  comps$solvent = NULL
  
  # take care of any rows with "name" of "-". These rows just used to add solvent.
  soln$compounds = comps[ comps$name != "-", ]
  soln$volume = rows[1,volume.col]
  class(soln) = "Solution"
  
  action = as.list(rows[1,action.cols])
  action$solution = soln
  class(action) = "RTCAaction"
  
  return( action )
  
}

# parse all actions from a data.frame to a list of RTCAaction objects
df.to.actions = function( x ) {
  
  # convert all the rows to actions
  a = actions.to.rows(x)
  rows = split(x,cumsum(a$wellrows))
  actions = lapply(rows,dataframe.to.RTCAaction)
  
  # save to which well and experiment the action belongs
  locations = x[a$wellrows,c("file","wells")]
  rownames(locations) = NULL
  
  return(list(locs=locations,actions=actions))  
}

# convert a "list" of "RTCAaction" objects to a "RTCAactionList" object
new.actionList = function(x) {
  
  out = do.call( rbind, llply( x, as.data.frame ) )
  class(out) = c("RTCAactionList","data.frame")
  return(out)
}

# convert an "RTCAactionList" to an "RTCAactionTimeline"
list.to.timeline = function( actionList ) {
  
  # reorder information chronologically
  neworder = order(actionList$t1)
  actionList = actionList[neworder,]
  
  # add a column that describes the status of the well and additions to the well
  adds = status = solns = actionList[,"solution"]
  
  # volume and concentrations of entire well at each action
  status[[1]] = solns[[1]]
  if(nrow(actionList)>1) {
    for( i in 2:nrow(actionList) ) {
      
      # remove volume if necessary
      prev.Solution = status[[i-1]]
      if( actionList[i,"rmVol"]>0 )
        prev.Solution$volume = prev.Solution$volume - actionList[i,"rmVol"]
      
      # add solutions
      status[[i]] = prev.Solution + solns[[i]]
      adds[[i]] = status[[i]] - prev.Solution
    }
  }
  
  # volume and concentrations of solution that was added at each action
  actionList$status = I(status)
  actionList$solution = I(adds)
  rownames(actionList) = NULL
  class(actionList) = c("data.frame","RTCAactionTimeline")
  actionList
}

# convert the actions and locations into well objects with timelines
# The input is the output from a "df.to.actions" function call
actions.to.wells = function( a ) {
  
  # convert the list of RTCAaction objects to a RTCAactionList object
  b = new.actionList(a$actions)
  
  # split actions according to location and then make RTCAlists
  wells = list()
  allwells = a$locs[!duplicated(a$locs),]
  
  for( i in 1:nrow(allwells) ) {
    
    # Make an RTCAactionList for each well
    location = allwells[i,"wells"]
    file = allwells[i,"file"]
    rows = apply( mapply(`==`,a$locs,allwells[i,]), 1, all )
    actions = a$actions[rows]
    actionList = b[rows,]
    
    # Convert to an RTCActionTimeline
    timeline = list.to.timeline(actionList)
    
    # Make a "Well" object with the timeline
    well = list()
    well$location = location
    well$file = file
    well$timeline = timeline  
    
    class(well) = c("Well","list")
    wells[[i]] = well
  }
  return( sortWells( wells ) )
  
}

# reorder wells first by file. Then by location, e.g., A1...H1, B2...H2, ...
sortWells = function( wells ) {
  
  files = vapply(wells,"[[","","file")
  locs = vapply(wells,"[[","","location")
  letter.part = str_extract(locs,"[A-H]+")
  number.part = str_extract(locs,"[0-9]+")
  wells = wells[ order(files,number.part,letter.part) ]
}

# process each data file and then assign data to the appropriate wells
add.data.to.wells = function( wells, data.dir=getwd() ) {
  
  files = unique(getfiles(wells))
  fpaths = file.path(data.dir,files)
  
  # loop through each data file
  for( i in seq_along(fpaths) ) {
    dat = exprs(parseRTCA(fpaths[i]))
    file = files[i]
    
    # loop through the column of each data matrix, asssigning data to wells
    for( j in 1:ncol(dat) ) {
      location = colnames(dat)[j]
      idx = well.index(wells,location,file)
      data = dat[,j,drop=FALSE]
      df = data.frame( sweep = 1:nrow(data), 
                       time=as.numeric(rownames(data)), 
                       values=as.numeric(data))
      if( is.na(idx) ) {
        message(str_c("Well ",location," in file ",file, " is not in the protocol. Well skipped."))
      } else {
        wells[[idx]]$data = df
      }
    }
  }
  wells
}




