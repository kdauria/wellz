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
source("./Scripts3/Spline.R")
source("./Scripts3/Plotting.R")
source("./Scripts3/Transform.R")

metadata = "./Tests/LoadingData/OneExperiment-Correct.csv"
metadata = "./Tests/LoadingData/MultipleCompoundSolution.csv"
metadata = "./MasterSheet.csv"
data.dir = "./Data/"

wells = parse_metadata(metadata,data.dir,parse_rtca)


search(wells,compstr="HCT8")
subset = select(wells,"(TcdB[10] & gdTcdB) | ( TcdA[10] & gdTcdB )", controls=TRUE)
subset = select(wells,"(TcdB[10] | TcdA[10]) & gdTcdB")
subset = select(wells,"(TcdB[10] | TcdA[10]) & !gdTcdB & !PMN")

# practice with replicates
x = select(wells,"TcdA",filename="CecalCells.txt", controls=TRUE)
x = c(x,x)
code(x) = as.character(1:34)

plot(x, xlim=c(33,40), color="concentration")









