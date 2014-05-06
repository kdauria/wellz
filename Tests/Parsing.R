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




