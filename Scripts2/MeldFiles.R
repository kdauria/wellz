source("./Scripts2/library.R")

# Parse and store data
wells = parse.RTCAanalyze( metadata="./MasterSheet.csv", data.dir="./Data" )



# Getting the metrics
twell = max.rate(twell,ID="toxinAdd",smoother=TRUE,nbw=10,min.diff=10/60/60,ylim=0.9,xlim=2)
aa = getMetric(twell,metric="max.rate")
bb = groupMetric(twell,metric="max.rate",ID="toxinAdd")
plotMetric(bb)

# Plotting the metrics
p2 = plot(twell,showpoints=FALSE,color="by.concentrations",linetype="by.file")
addMetricLayer(p2,wells=twell,metric="max.rate")



