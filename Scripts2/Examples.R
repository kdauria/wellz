# Data file....
source("./Scripts2/library.R")

#### Load data ####
wells = parse.RTCAanalyze( metadata="./MasterSheet.csv", data.dir="./Data" )
wells

##### Selecting wells ####
subset = retrieveWells(wells,file="HCT8.txt")
subset

#### Basic plotting ####
plot(subset, color="by.concentrations", linetype="by.compounds")
plot(subset, replicates=TRUE, ID="cellSeed", manip=TRUE)

#### Internals for "power users" ####
well = subset[[1]]
well
s1 = getsolutions(well, ID="cellSeed")
s2 = getsolutions(well, ID="toxinAdd")
s1+s2
s1+s2-s2

#### Data transformations ####
twells = transform(subset,c("slice","tcenter","normalize"),ID="toxinAdd",xlim=c(-1,10))
plot(twells)
plot(twells,deriv=1)

#### Customized plotting ####
plot(twells,deriv=1,diagnostic=1,manip=TRUE)

#### Analyses of wells ####
subset = retrieveWells(wells,file=c("HCT8.txt"),controls=TRUE,ID="toxinAdd",
                       compounds="HCT8", max.concentrations=6000)
twells = transform(subset,c("slice","normalize","tcenter"),ID="toxinAdd",xlim=c(-1,25))
aa = max.rate(twells,ID="toxinAdd",duration=40,nbw=20)
met = groupMetric(aa,"toxinAdd","max.rate")
plotMetric(met)

p1 = plot(twells)
p2 = plot(twells, deriv=1)
a1 = addMetricLayer( p1, aa, "max.rate")
a2 = addMetricLayer( p2, aa, "max.rate")
a1
a2

#### novel spline adaptive to curvature and noise ####
rr = wsmooth(twells,xscale=1.5,yscale=c(0,1))
p = plot(rr, deriv=1,showpoints=FALSE)
aa = max.rate(rr,ID="toxinAdd",duration=40,nbw=20)
a = addMetricLayer( p, aa, "max.rate")

plot(rr,deriv=1,showpoints=FALSE)
p = plot(rr)
a = addMetricLayer(p,aa,"max.rate")
a





