library(zoo)
library(stringr)
library(plyr)
library(data.table)
library(Rcpp)
library(ggplot2)
source("./Scripts3/Parsing.R")
source("./Scripts3/Parsing_expand.R")
source("./Scripts3/Parsing_data.R")
source("./Scripts3/Getset.R")
source("./Scripts3/General.R")
source("./Scripts3/Solutions.R")
source("./Scripts3/Print.R")
source("./Scripts3/Select.R")
source("./Scripts3/Melt.R")
source("./Scripts3/Group.R")

metadata = "./Tests/LoadingData/OneExperiment-Correct.csv"
metadata = "./Tests/LoadingData/MultipleCompoundSolution.csv"
metadata = "./MasterSheet.csv"
data.dir = "./Data/"

wells = parse_metadata(metadata,data.dir,parse_rtca)


search(wells,compstr="HCT8")
subset = select(wells,"(TcdB[10] & gdTcdB) | ( TcdA[10] & gdTcdB )")
subset = select(wells,"(TcdB[10] | TcdA[10]) & gdTcdB")
subset = select(wells,"(TcdB[10] | TcdA[10]) & !gdTcdB & !PMN")


print(subset,printall=TRUE)
select.wellList


print( paste(comp.name, conc.bounds) )


# OK the next step is plotting the wells






