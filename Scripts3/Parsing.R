library(zoo)
library(stringr)
source("./Scripts3/Parsing_expand.R")
source("./Scripts3/Getset.R")
source("./Scripts3/General.R")

metadata = "./Tests/LoadingData/OneExperiment-Correct.csv"
metadata = "./Tests/LoadingData/MultipleCompoundSolution.csv"
data.dir = "./Data/"

# 1. Read the data into R
read_metadata = function( metadata, data.dir, sep="\t" ) {
  read.csv(file=metadata,header=TRUE,sep=sep,
               stringsAsFactors=FALSE)
}

# 2. Fill in empty cells with the value from above column
fillblanks_metadata = function(x) {
  x[x==""] = NA
  x = na.locf(x)
  x[x=="-"] = NA
  x
}

# 3. Expand comma-separated shorthand
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
      split.df = matrix( comma_split(row[comma.cols]) , nrow=nw )
      newdf[ 0:(nw-1)*nc+i, comma.cols ] = split.df
    }
  }
  newdf
}
expand_actions = function(meta.df) {
  action.rows = split(meta.df,paste(meta.df$wells,meta.df$ID))
  do.call( rbind, lapply(action.rows,expand_action) )
}

# 4. Set some columns to be numeric
colclasses_metadata = function(meta.df, numeric.cols=c("i","rmVol","adVol","conc") ) {
  meta.df[,numeric.cols] = apply(meta.df[,numeric.cols],2,as.numeric)
  meta.df
}

# 5. Create a list of actions for each well
row_to_action = function( x ) {
  # Make a solution object
  solution = list()
  solution$solvent = data.frame(name=x$solvent[1],perc=100)
  solution$compounds = na.omit( x[,c("name","conc","type")] )
  solution$volume = x$adVol[1]
  class(solution) = c("Solution","list")
  
  # Make the action object
  data.frame(file=x$file[1], code=x$wells[1],
              ID=x$ID[1], i=x$i[1], 
              rmVol=x$rmVol[1], solution=I(list(solution)),
              stringsAsFactors=FALSE)

}
rows_to_actions = function( meta.df ) {
  action.rows = split(meta.df,paste(meta.df$file,meta.df$wells,meta.df$ID))
  all.actions = do.call(rbind, lapply(action.rows,row_to_action) )
  all.actions = all.actions[order(all.actions$i,all.actions$file,all.actions$code),]
  split(all.actions[,c("ID","i","rmVol","solution")], 
        paste(all.actions$file,all.actions$code))
}

# 6. Create wells object from the actions
make_wells = function(meta.df, well.actions) {
  # Create empty well objects and the wellList
  template.well = list(file=NA,code=NA,actions=NA)
  class(template.well) = c("well","list")
  wells = rep(list(template.well),length(well.actions))
  class(wells) = c("wellList","list")
  
  # Fill in the file names and codes
  wells.df = do.call(rbind, str_split( names(well.actions), " ") )
  files = 
  code(wells) = wells.df[,2]
  filename(wells) = wells.df[,1]
  wells
}

# 7. Convert the well.actions to lists
action.as.list = function(x) {
  x = as.list(x)
  class(x) = c("action","list")
  x
}
action.df.as.list = function(x) {
  action.list = apply( x, 1, action.as.list )
  class(action.list) = c("actionList","list")
  names(action.list) = ID(action.list)
  action.list
}


meta.df = read_metadata(metadata,data.dir)
meta.df = fillblanks_metadata(meta.df)
meta.df = expand_actions(meta.df)
meta.df = colclasses_metadata(meta.df)

well.actions = rows_to_actions(meta.df)
wells = make_wells(meta.df, well.actions)
action.lists = lapply(well.actions,action.df.as.list)

















