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


aa = profr(parse_metadata(metadata,data.dir,parse_rtca))
plot(aa)

Rprof()
x = parse_metadata(metadata,data.dir,parse_rtca)
Rprof(NULL)
summaryRprof()


library(profr)


glm_ex <- profr({Sys.sleep(1); example(glm)}, 0.01)
head(glm_ex)
summary(glm_ex)
plot(glm_ex)
