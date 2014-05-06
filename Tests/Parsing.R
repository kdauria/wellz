library(zoo)
library(stringr)
library(plyr)
library(data.table)
source("./Scripts3/Parsing.R")
source("./Scripts3/Parsing_expand.R")
source("./Scripts3/Parsing_data.R")
source("./Scripts3/Getset.R")
source("./Scripts3/General.R")
source("./Scripts3/Solutions.R")

metadata = "./Tests/LoadingData/OneExperiment-Correct.csv"
metadata = "./Tests/LoadingData/MultipleCompoundSolution.csv"
metadata = "./MasterSheet.csv"
data.dir = "./Data/"

wells = parse_metadata(metadata,data.dir,parse_rtca)

library(profr)
aa = profr(parse_metadata(metadata,data.dir,parse_rtca))
plot(aa)

Rprof()
x = parse_metadata(metadata,data.dir,parse_rtca)
Rprof(NULL)
summaryRprof()




#################### Profiling to speed up the parsing and
#################### ability to access parts of a wellList


# Figure out how many actions and in what rows they start
cond = paste( meta.df$file, meta.df$wells, meta.df$ID )
action.idxs = as.numeric(factor(cond,levels=unique(cond)))
action.rows = match(unique(action.idxs),action.idxs)
action.nrows = diff(c(action.rows,length(action.idxs)+1))
nactions = length(action.rows)

# Figure out how many wells total there will be and allocate
# a wellList object for all of them
codes = lapply( meta.df[action.rows,"wells"], expand_code )
files = meta.df[action.rows,"file"]
roster = data.frame(file=rep(files,times=vapply(codes,length,1)),code=unlist(codes), stringsAsFactors=FALSE )
roster = roster[!duplicated(roster),]
roster = roster[order(roster$file,roster$code),]

# Allocate a wellList for all of the wells
template.well = structure(list(file="",code="",actions=list()),class=c("well","list"))
wells = structure( rep(list(template.well),nrow(roster)), class=c("wellList","list"))

# Go through each action one by one and add to wells
filename(wells) = roster$file
code(wells) = roster$code












Rprof()
for( i in 1:10 ) {
  codes = lapply( meta.df[action.rows,"wells"], expand_code )
  files = meta.df[action.rows,"file"]
  roster = data.frame(file=rep(files,times=vapply(codes,length,1)),code=unlist(codes), stringsAsFactors=FALSE )
  roster = roster[!duplicated(roster),]
  roster = roster[order(roster$file,roster$code),]
  
  # Allocate a wellList for all of the wells
  template.well = structure(list(file="",code="",actions=NA),class=c("well","list"))
  wells = structure( rep(list(template.well),nrow(roster)), class=c("wellList","list"))
  
  # Go through each action one by one and add to wells
  filename(wells) = roster$file
  code(wells) = roster$code
  
  # 
  
}
Rprof(NULL)
summaryRprof()


for( i in 1:nactions ) {
  
  rows = meta.df[ action.rows[i] + 1:action.nrows[i] - 1,  ]
  
  
  
}