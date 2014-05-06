library(zoo)
library(stringr)
library(plyr)
library(data.table)
library(Rcpp)
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

library(profr)
a = profr({wells = parse_metadata(metadata,data.dir,parse_rtca)})
plot(a,minlabel=0.01)


Rprof()
x = parse_metadata(metadata,data.dir,parse_rtca)
Rprof(NULL)
summaryRprof()


#################### Profiling to speed up the parsing and
#################### ability to access parts of a wellList

# Parse the metadata
Rprof()
meta.df = read_metadata(metadata,data.dir)
meta.df = fillblanks_metadata(meta.df)
wells = metadata_to_wells(meta.df)
Rprof(NULL)
summaryRprof()

################### Parsing the data faster
################### Copying data is what takes the longest
parse_fun = parse_rtca
allfiles = filename(wells)
allcodes = code(wells)
codes = split( allcodes, allfiles )
files = names(codes)
file.paths = file.path(data.dir,names(codes))
r = roster(wells)

for( i in seq_along(file.paths)) {
  
  codes.i = codes[[i]]
  message(paste("Parsing",files[i]))
  dat = parse_fun(file.paths[i])
  
  for( j in seq_along(codes.i) ) {
    if( !codes.i[j] %in% colnames(dat) ) {
      warning(paste("Well",codes.i[j],"not in file",files[i]))
      next
    }
    well.ij = which( files[i] == r$file & codes.i[j] == r$code )
    wells[[well.ij]]$data = dat[,c("i","t",codes.i[j])]
  }
}








