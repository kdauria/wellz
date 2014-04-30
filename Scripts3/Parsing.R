# Steps to parsing an annotation file
# 1. Read the data into R
# 2. Fill in empty cells with value from above column

# 3. Identify action rows
# 4. Convert rows to action objects

# 5. Identify all wells
# 6. Make all well objects

# 7. Assign the actions to actionLists in appropriate well objects
# 8. Sort the actionLists by index for each well

# DONE


library(zoo)
library(stringr)
source("./Scripts3/Parsing_expand.R")
source("./Scripts3/Getset.R")

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

# 3. Set some columns to be numeric
colclasses_metadata = function(meta.df, numeric.cols=c("i","rmVol","adVol","conc") ) {
  meta.df[,numeric.cols] = apply(meta.df[,numeric.cols],2,as.numeric)
  meta.df
}

meta.df = read_metadata(metadata,data.dir)
meta.df = fillblanks_metadata(meta.df)
meta.df = colclasses_metadata(meta.df)


action.rows = split(meta.df,paste(meta.df$wells,meta.df$ID))

### Create well objects
# Expand a well shorthand to a vector of wells
well.codes = vapply(action.rows,function(x) x$wells[1], "")
well.codes = lapply(well.codes,expand_code)

# Make a "roster" of wells, with its code and file as it's "name"
files = vapply(action.rows,function(x) x$file[1], "")
wells.in.file = lapply( split(well.codes,files), function(x) unique(unlist(x)))
files = rep(names(wells.in.file),time=sapply(wells.in.file,length))
codes = unlist(wells.in.file)
wells = data.frame(file=files, code=codes, actions=NA)
class(wells) = c("well", "data.frame")
rownames(wells) = NULL

### Create action objects from the rows
row_to_action = function( x ) {
  # Make a solution object
  solution = list()
  solution$solvent = data.frame(name=x$solvent[1],perc=100)
  solution$compounds = na.omit( x[,c("name","conc","type")] )
  solution$volume = x$adVol[1]
  class(solution) = "Solution"
  
  # Make the action object
  action = list()
  action$solution = solution
  action$ID = x$ID[1]
  action$rmVol = x$rmVol[1]
  class(action) = "Action"
  action
}
actions = lapply(action.rows,row_to_action)

### Add the action objects to the well objects
files = vapply(action.rows,function(x) x$file[1], "")
for( i in 1:length(files) ) {
  codes = well.codes[[i]]
  select(wells,files[i],codes)$actions = actions[i]
}

select(wells,files[i],codes)
search(wells,files[i],codes)





wells$actions[[1]] = actions[[1]]

select(wells,file=files[1])$actions









action = as.list(rows[1,action.cols])
action$solution = soln
class(action) = "RTCAaction"





out = as.list(x[1,c("")])

