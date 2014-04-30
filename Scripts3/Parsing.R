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


library(data.table)

metadata = "./Tests/LoadingData/OneExperiment-Correct.csv"
metadata = "./Tests/LoadingData/MultipleCompoundSolution.csv"

data.dir = "./Data/"

# 1. Read the data into R
read_metadata = function( metadata, data.dir, sep="\t" ) {
  read.csv(file=metadata,header=TRUE,sep=sep,
               stringsAsFactors=FALSE,comment.char="",
               colClasses=c("character","character","character",
                            "numeric","numeric","numeric",
                            "character","character","character",
                            "character"),check.names=FALSE)
}

# 2. Fill in empty cells with the value from above column
fillblanks_metadata = function(meta.df) {
  meta.df[is.na(meta.df)] = ""
  
  # Some rows only exist to add multiple compounds to one action
  action.rows = which(meta.df$ID!="" | meta.df$wells!="")
  compound.rows = setdiff(1:nrow(meta.df),action.rows)
  compound.cols = c("name","conc","type")
  
  # For single-compound actions
  for( i in 1:length(action.rows) ) {
    empty.cols = meta.df[action.rows[i],]==""
    if(sum(empty.cols)) 
      meta.df[action.rows[i],empty.cols] = meta.df[action.rows[i-1],empty.cols]
  }
  
  # For multicompound actions
  for( i in 1:length(compound.rows) ) {
    empty.cols = compound.cols[meta.df[compound.rows[i],compound.cols]==""]
    if(length(empty.cols)) 
      meta.df[compound.rows[i],empty.cols] = meta.df[compound.rows[i]-1,empty.cols]
  }
  
  meta.df
}

# 3. Identify action rows

action.rows = which(meta.df$ID!="" | meta.df$wells!="")
file.names = meta.df$file[action.rows]
well.names = meta.df$wells[action.rows]


rep(action.rows, times=diff( c(action.rows, nrow(meta.df)+1) ))



# Go action by action, assigning actions to lists
wells = list()
action.rows = which(meta.df$file!="")



meta.df = read_metadata(metadata,data.dir)
meta.df = fillblanks_metadata(meta.df)


library(zoo)
na.locf(meta.df)

aa[aa]




