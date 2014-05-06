
# Wrapper function
parse_metadata = function( metadata, data.dir, parse_fun ) {

  # Parse the metadata
  message("Parsing metadata")
  meta.df = read_metadata(metadata,data.dir)
  meta.df = fillblanks_metadata(meta.df)
  wells = metadata_to_wells(meta.df)
  for( i in seq_along(wells) )
    solution(wells[[i]]) = Reduce(`+`,solution(wells[[i]]),accumulate=TRUE)
  
  wells = add_data(wells, data.dir, parse_fun )
}

# Read the data into R
read_metadata = function( metadata, data.dir, sep="\t" ) {
  read.csv(file=metadata,header=TRUE,sep=sep,
               stringsAsFactors=FALSE,strip.white=TRUE)
}

# Fill in empty cells with the value from above column
fillblanks_metadata = function(x) {
  x[x==""] = NA
  x = na.locf(x)
  x[x=="-"] = NA
  x
}

# Expand comma-separated shorthand
expand_action = function( df ) {
  codes = expand_code( df$wells )
  nc = nrow(df) # number of compounds
  nw = length(codes) # number of wells
  newdf = df[rep(1:nc,nw),]
  newdf$wells = rep(codes,each=nc)
  
  for( i in 1:nc) {
    row = as.character(newdf[i,])
    comma.cols = grepl(",",row)
    
    if(any(comma.cols)) {
      split.df = matrix( strsplit(row[comma.cols],"[ ]*,[ ]*")[[1]] , nrow=nw )
      newdf[ 0:(nw-1)*nc+i, comma.cols ] = split.df
    }
  }
  newdf
}

# Set some columns to be numeric
colclasses_metadata = function(meta.df, numeric.cols=c("i","rmVol","adVol","conc") ) {
  meta.df[,numeric.cols] = apply(meta.df[,numeric.cols],2,as.numeric)
  meta.df
}

# Convert the metadata to wells
metadata_to_wells = function( meta.df ) {
  
  # Figure out how many actions and in what rows they start
  cond = paste( meta.df$file, meta.df$wells, meta.df$ID )
  action.idxs = as.numeric(factor(cond,levels=unique(cond)))
  action.rows = match(unique(action.idxs),action.idxs)
  action.nrows = diff(c(action.rows,length(action.idxs)+1))
  nactions = length(action.rows)
  
  # Figure out how many wells there are
  codes = lapply( meta.df[action.rows,"wells"], expand_code )
  files = meta.df[action.rows,"file"]
  roster = data.frame(file=rep(files,times=vapply(codes,length,1)),code=unlist(codes), stringsAsFactors=FALSE )
  roster = roster[!duplicated(roster),]
  roster = roster[order(roster$file,roster$code),]
  
  # Allocate a wellList for all of the wells
  template.well = structure(list(file="",code="",
                                 actions=structure(list(),class=c("actionList","list"))),
                            class=c("well","list"))
  wells = structure( rep(list(template.well),nrow(roster)), class=c("wellList","list"))
  
  # Add the basic information for each well
  filename(wells) = roster$file
  code(wells) = roster$code
  
  for( i in 1:nactions ) {
    
    nrows = action.nrows[i]
    rows = meta.df[ action.rows[i] + 1:nrows - 1,  ]
    newdf = colclasses_metadata(expand_action(rows))
    
    for( j in 1:nrow(newdf) ) {
      x = newdf[j,]
      solution = list()
      solution$solvent = data.frame(name=x$solvent[1],perc=100,stringsAsFactors=FALSE)
      solution$compounds = na.omit( x[,c("name","conc","type")] )
      rownames(solution$compounds) = NULL
      solution$volume = x$adVol[1]
      class(solution) = c("Solution","list")
      
      y = list()
      y$ID = x$ID
      y$i = x$i
      y$rmVol = x$rmVol
      y$solution = solution
      class(y) = c("action","list")
      
      # Append new action
      well.i = which(x$wells == roster$code & x$file == roster$file)
      new.actions = structure( c( wells[[well.i]]$actions, structure(list(y),names=x$ID) ),
                               class=c("actionList","list"))
      wells[[well.i]]$actions = structure( new.actions[ order(index(new.actions)) ],
                                           class=c("actionList","list"))
    }
  }
  wells
}



