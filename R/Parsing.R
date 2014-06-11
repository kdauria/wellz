#' @title Parse metadata file of well actions
#' 
#' @description
#' The metadata file must be of a csv file of a specific format. This wrapper
#' function calls several other functions that creates a \code{wellList} object.
#' 
#' @details
#' In order, the column headers of the file must be
#' \code{"file"}, \code{"wells"}, \code{"ID"}, \code{"i"},
#' \code{"rmVol"}, \code{"adVol"}, \code{"name"}, \code{"conc"},
#' \code{"type"}, and \code{"solvent"}.
#' 
#' Empty cells of the csv file are filled with the next
#' non-empty cell above. The \code{wells} column is for the
#' location of the well. The location is a combination of a letter
#' (indicating the row on the plate) and a number (the column
#' on the plate). For example, "D3" would be the well in
#' fourth row and third column of a plate. 
#' Shorthand is allowed for the \code{well} column.
#' For example, \code{"A-B1-2, F3-4"} would
#' expand to \code{c("A1","A2","B1","B2","F3","F4")} (see \code{expand_code} for
#' details).
#' 
#' The ID labels the row as an \code{action} that is being performed
#' right before or at the same time that data point \code{i} is collected.
#' \code{rmVol} and \code{adVol} are the volume removed and added, respectively,
#' for the action.
#' 
#' \code{name}, \code{conc}, and \code{type} columns refer to the compounds
#' in the solution added in this action. If nothing but solvent was added,
#' then all columns can be filled with a hyphen ("-"). Each compound must
#' be entered on a separate row. If there are more than two compounds in
#' an action (in one solution), then simply make sure that all other
#' columns are the same (or blank). The code will identify this as a solution
#' with multiple compounds. \code{type} identifies different ways that
#' solutions can be combined or will be tracked. \code{type="start"} means
#' that the given value is the concentration before the solution is added
#' to the well (at the start). \code{type="final"} means that this will
#' be the concentration of the well after \code{rmVol} is removed and
#' \code{adVol} is added. \code{type="total"} is the total count of some
#' substance in the solution (e.g., the total number of cells or beads);
#' a compount of type "total" is treated specially in that it's "concentration"
#' never changes with changing volume.
#' 
#' For the \code{solvent} column, only one solvent can be given
#' for each action. Complicated mixtures must be referred to
#' by a simple name (e.g., "media" or "broth").
#' 
#' The \code{parse_fun} argument must parse a data file and return
#' a data.frame where the column names are the locations of the wells, and the
#' first column contains the times for each data point. The name of this first
#' column must be "t".
#' 
#' @param data.dir the root directory for the data files
#' @param parse_fun function that parses data files and returns data matrix
#' @param spline logical indicating to add a spline or not
#' 
parse_metadata = function( metadata, data.dir=NULL, parse_fun=NULL, spline=!is.null(parse_fun) ) {

  # Parse the metadata
  message("Parsing metadata")
  meta.df = read_metadata(metadata,data.dir)
  meta.df = fillblanks_metadata(meta.df)
  wells = metadata_to_wells(meta.df)
  
  # Add solutions of different "actions" to get status of well
  for( i in seq_along(wells) ) {
    for( j in seq(2, length.out=length(wells[[i]]$actions)-1)) {
      wells[[i]]$actions[[j]]$solution = 
        `+.Solution`(s1 = wells[[i]]$actions[[j-1]]$solution,
                     s2 = wells[[i]]$actions[[j]]$solution,
                     rmVol = wells[[i]]$actions[[j]]$rmVol)
    }
  }
  
  # Add the data to the wells
  if(!is.null(parse_fun)) wells = add_data(wells, data.dir, parse_fun )
  
  if(spline) {
    message("Adding interpolating splines")
    wells = add_spline(wells)
  }
  
  # Add an interpolating spline to all the wells
  #for( i in seq_along(wells))
  #  wells[[i]]$spline = splinefun( x=tdata(wells[[i]]), y=vdata(wells[[i]]), method="monoH.FC" )
  
  return(wells)
}

#' Raw parse well metadata
#' 
#' A wrapper for \code{read.csv} for metadata files
#' 
#' @param metadata file path for the metadata file
#' @param data.dir directory of the data files
read_metadata = function( metadata, data.dir, sep="\t" ) {
  read.csv(file=metadata,header=TRUE,sep=sep,
               stringsAsFactors=FALSE,strip.white=TRUE)
}


#' fill empty cells of metadata file
#' 
#' If a cell from metadata file is empty, fill it
#' with the value from the next non-empty cell above it.
#' If a cell is "-", then change it to \code{NA}.
#' 
#' @param x metadata file
fillblanks_metadata = function(x) {
  x[x==""] = NA
  x = na.locf(x)
  x[x=="-"] = NA
  x
}

#' Expand a metadata row
#' 
#' If a row in a metadata file has shorthand for
#' applying the same action to several wells, this
#' function expands those rows are repeated for each well. The
#' location of the wells are expanded with \code{expand_code}.
#' All other columns are checked if they have comma separated values.
#' If there is such a column, it's values are separated among
#' each of the new rows.
#' 
#' @param df a \code{data.frame} holding all the rows for for one action
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

#' Change metadata column classes
#' 
#' By default, the metadata is parsed in as all strings. This changes
#' columns with names in \code{numeric.cols} to numeric type variables.
#' 
#' @param meta.df a \code{data.frame} of all the metadata (see \code{parse_metadata})
#' @param numeric.cols the names of columns to make numeric
colclasses_metadata = function(meta.df, numeric.cols=c("i","rmVol","adVol","conc") ) {
  meta.df[,numeric.cols] = apply(meta.df[,numeric.cols],2,as.numeric)
  meta.df
}


#' Convert metadata data.frame to wellList
#' 
#' This is the large wrapper function that does most of the
#' grunt work for converting metadata to \code{Solution}, 
#' \code{action}, \code{actionList}, \code{well}, and \code{wellList}
#' wellList. This should eventually be refactored so that there
#' are constructors for each of these objects.
#' 
#' @param meta.df the metadata in a \code{data.frame} (see \code{parse_metadata})
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
  rost = data.frame(file=rep(files,times=vapply(codes,length,1)),code=unlist(codes), stringsAsFactors=FALSE )
  rost = rost[!duplicated(rost),]
  rost = rost[order(rost$file,rost$code),]
  
  # Allocate a wellList for all of the wells
  template.well = structure(list(file="",code="",
                                 actions=structure(list(),class=c("actionList","list"))),
                            class=c("well","list"))
  wells = structure( rep(list(template.well),nrow(rost)), class=c("wellList","list"))
  
  # Add the basic information for each well
  filename(wells) = rost$file
  code(wells) = rost$code
  
  for( i in 1:nactions ) {
    
    nrows = action.nrows[i]
    rows = meta.df[ action.rows[i] + 1:nrows - 1,  ]
    newdf = colclasses_metadata(expand_action(rows))
    codes = unique(newdf$wells)
    
    for( j in 1:length(codes) ) {
      x = newdf[newdf$wells==codes[j],]
      soln = list()
      soln$solvent = data.frame(name=x$solvent[1],perc=100,stringsAsFactors=FALSE)
      soln$compounds = na.omit( x[,c("name","conc","type")] )
      rownames(soln$compounds) = NULL
      soln$volume = x$adVol[1]
      class(soln) = c("Solution","list")
      
      y = list()
      y$ID = x$ID[1]
      y$i = x$i[1]
      y$rmVol = x$rmVol[1]
      y$solution = soln
      class(y) = c("action","list")
      
      # Append new action
      well.i = which(x$wells[1] == rost$code & x$file[1] == rost$file)
      new.actions = structure( c( wells[[well.i]]$actions, structure(list(y),names=y$ID )),
                               class=c("actionList","list"))
      wells[[well.i]]$actions = structure( new.actions[ order(index(new.actions)) ],
                                           class=c("actionList","list"), names=ID(new.actions))
    }
  }
  wells
}



