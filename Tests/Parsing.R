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
source("./Scripts3/Print.R")
source("./Scripts3/Select.R")

metadata = "./Tests/LoadingData/OneExperiment-Correct.csv"
metadata = "./Tests/LoadingData/MultipleCompoundSolution.csv"
metadata = "./MasterSheet.csv"
data.dir = "./Data/"

library(profr)
a = profr({wells = parse_metadata(metadata,data.dir,parse_rtca)})
plot(a,minlabel=0.01)


Rprof()
wells = parse_metadata(metadata,data.dir,parse_rtca)
Rprof(NULL)
summaryRprof()


select(wells)



search(wells,compstr="HCT8")
subset = select(wells,"(TcdB & gdTcdB) | ( TcdA & gdTcdB )")
print(subset,printall=TRUE)
select.wellList


print( paste(comp.name, conc.bounds) )
