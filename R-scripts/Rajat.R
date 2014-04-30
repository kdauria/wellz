source("./R-scripts/dif.R")
source("./R-scripts/truncateSurvival.R")
source("./R-scripts/plot.columns.R")
source("./R-scripts/dif.columns.R")
source("./R-scripts/library.R")
library(reshape)
library(reshape2)
library(RSvgDevice)

################################################################################
#                         Plot entire growth curve                             #
################################################################################
aa = read.csv("/home/kevin/Replicate1.csv")
aa = aa[103:nrow(aa),]
colnames(aa) = c("Time","A1000","A300","Media","A100","A10","A3","A1","A0.1")
aa[,1] = aa[,1] - aa[1,1]

bb = read.csv("/home/kevin/Replicate2.csv")
bb = bb[495:nrow(bb),]
colnames(bb) = c("Time","Raj.100","Media","A300","Raj.1000","A100","Raj.10","A3","A1","A0.1")
bb[,1] = bb[,1] - bb[1,1]
bb = bb[1:nrow(aa),]

# combine the data
Time = aa[,"Time"]
A1000 = aa[,"A1000"]
A300 = (aa[,"A300"] + bb[,"A300"])/2
A100 = aa[,"A100"]
A10 = aa[,"A10"]
A3 = (aa[,"A3"] + bb[,"A3"])/2
A1 = (aa[,"A1"] + bb[,"A1"])/2
A0.1 = (aa[,"A0.1"] + bb[,"A0.1"])/2
Raj.100 = bb[,"Raj.100"]
Raj.10 = bb[,"Raj.10"]
Raj.1000 = bb[,"Raj.1000"]

combined = as.data.frame(cbind(Time,A1000,A300,A100,A10,A3,A1,A0.1,Raj.1000,Raj.100,Raj.10))
colnames(combined) = c("Time","A1000","A300","A100","A10","A3","A1","A0.1","Raj.1000","Raj.100","Raj.10")
plot.columns(combined,"Time")

combined = as.data.frame(cbind(Time,A1000,A100,A10,Raj.1000,Raj.100,Raj.10))
colnames(combined) = c("Time","A1000","A100","A10","Raj.1000","Raj.100","Raj.10")
plot.columns(combined,"Time")

combined = as.data.frame(cbind(Time,A1000,A100,A10))
colnames(combined) = c("Time","A1000","A100","A10")
plot.columns(combined,"Time")


################################################################################
#                       Plot post-toxin data                                   #
################################################################################
# The cell index has already been normalized for the time when the toxin
# was added
data = combined

# plot all of the data on the same graph
plot.columns(data, x.var="Time")
plot.deriv( data, x.var="Time", df=50 )
max.rates2(data,"Time",50,F)

################################################################################
#                             Data Analysis                                    #h
################################################################################
data = combined
concs = c(1000,300,100,10,3,1,0.1,1000,100,10)
          
# rate of cytotoxicity
max.rate = max.rates2( data, "Time", 40, by.time=T )
plot.deriv( data, x.var="Time", df=35 )
plot(log10(concs),rates,
     pch=c(rep("H",7),rep("R",3))) #ok but not quite



##################

data.deriv = dif.columns(data,x.var,45)
data.deriv[1:15,c("A10","A1","A0.1","A3","Raj.10")] = 0
plot.columns(data.deriv,"Time")
y.data.deriv = data.deriv[,colnames(data.deriv)!=x.var]
rates = apply(y.data.deriv,2,max)
times = apply(y.data.deriv,2,which.max)

max.rates2 = function( data, x.var, df.precise=50, by.time=F ) {
  
  data.deriv = dif.columns(data,x.var,df.precise)
  plot.columns(data.deriv,x.var)
  y.data.deriv = data.deriv[,colnames(data.deriv)!=x.var]
  rates = apply(y.data.deriv,2,max)
  times = apply(y.data.deriv,2,which.max)
  if(by.time)
    times = data[times,x.var]
  
  return(cbind(times,rates))
}
