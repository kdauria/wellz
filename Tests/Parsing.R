library(zoo)
library(stringr)
library(plyr)
library(data.table)
library(Rcpp)
library(ggplot2)
library(lokern)
library(DierckxSpline)
source("./R/Parsing.R")
source("./R/Parsing_expand.R")
source("./R/Parsing_data.R")
source("./R/Getset.R")
source("./R/General.R")
source("./R/Solutions.R")
source("./R/Print.R")
source("./R/Select.R")
source("./R/Melt.R")
source("./R/Group.R")
source("./R/Spline.R")
source("./R/Plotting.R")
source("./R/Transform.R")

''
metadata = "./Data/MasterSheet.csv"
data.dir = "./Data/"

wells = parse_metadata(metadata,data.dir,parse_rtca)

# search(wells,compstr="HCT8")
# subset = select(wells,"(TcdB[10] & gdTcdB) | ( TcdA[10] & gdTcdB )", controls=TRUE)
# subset = select(wells,"(TcdB[10] | TcdA[10]) & gdTcdB")
# subset = select(wells,"(TcdB[10] | TcdA[10]) & !gdTcdB & !PMN")


x = select(wells, "HCT8[5000] & TcdA", ID="toxinAdd", controls=TRUE)
x = select(wells, "HUVEC & TcdA", ID="toxinAdd", controls=TRUE)

x = transform(x, c("tcenter","normalize"), ID="toxinAdd")
x = add_smoother(x, method="composite", window.width=15/60)
#plot(x)
plot(x[3:6], xlim=c(-1,4), points=TRUE, deriv=1, color="concentration", ID="toxinAdd" )


y = transform(x, c(tcenter, normalize, slice), ID="toxinAdd", xlim=c(-1,5))
plot(y, color="concentration", ID="toxinAdd", points=TRUE)
plot(y, color="concentration", ID="toxinAdd", points=TRUE, spline=TRUE)

y = add_smoother(y, method="smooth.spline", spar=0.5)
y = add_smoother(y, method="curfit")
y = add_smoother(y, method="composite")
plot(y, color="concentration", ID="toxinAdd", points=TRUE, smoother=TRUE)




