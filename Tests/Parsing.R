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





x = wells[[1]]

search(wells,compstr="HCT8")
subset = select(wells,"(TcdB[10] & gdTcdB) | ( TcdA[10] & gdTcdB )", controls=TRUE)
subset = select(wells,"(TcdB[10] | TcdA[10]) & gdTcdB")
subset = select(wells,"(TcdB[10] | TcdA[10]) & !gdTcdB & !PMN")




x = select(wells, "HCT8[5000] & TcdA[100]", ID="toxinAdd", controls=TRUE)

y = transform(x, c(tcenter, normalize, slice), ID="toxinAdd", xlim=c(-1,10))
plot(y, color="concentration", ID="toxinAdd", xlim = c(-2,2), points=TRUE)




bb = wells[[1]]
cc = level(bb,ID="toxinAdd")
plot(cc)

tt = rep(FALSE,20)
tt[1] = TRUE
aa = slice( wells[tt], xlim=c(0,10))
plot(aa, color="concentration", linetype="compound")

y = normalize(tcenter(x))
plot(x)
plot(y, xlim=c(-1,1), points=TRUE, color="concentration")


ID_i(x,"toxinAdd")

tcenter(x, ID="toxinAdd")

x = wells[[1]]
x2 = tcenter(x, "toxinAdd")

plot(x)
plot(tcenter(x,"toxinAdd") )


