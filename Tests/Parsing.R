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


