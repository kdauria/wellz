# A library of functions for processing multi-well data
source("./Scripts2/library.R")
source("./Scripts2/latexLibrary.R")
wells = parse.RTCAanalyze( metadata="./MasterSheet2.csv", data.dir="./Data2" )


###### Panel A
subset = retrieveWells(wells,file=c("J774-a.txt","J774-b.txt"))
t.subset = normalize_toxin( subset, xlim=c(-Inf,100))

# Toxin A curves
conditions = groupWells( t.subset, group="by.concentrations") %in% 
                 c("",paste0("TcdA-",c(0.1,3,100,1000)))
p1 = plot(t.subset[conditions], replicates=TRUE, xlim=c(-0.01,40), color="by.concentrations")

# Toxin B curves
conditions = groupWells( t.subset, group="by.concentrations") %in% 
                 c("",paste0("TcdB-",c(0.1,10,100)))
p2 = plot(t.subset[conditions], replicates=TRUE, xlim=c(-0.01,15), color="by.concentrations")

# TcdA and TcdB curves side by side
grid.arrange(p1,p2,ncol=2)


###### Panel B
subset = retrieveWells( wells, file="J774-4.txt" )
t.subset = normalize_toxin( subset, xlim=c(-Inf,100))

conditions = groupWells( t.subset, group="by.concentrations") %in% 
                 c("","TcdA-300","TcdB-100","TcdB-1")
p3 = plot(t.subset[conditions],xlim=c(-1,48), color="by.concentrations", replicates=TRUE)
p4 = plot(t.subset[conditions],xlim=c(-1,5), color="by.concentrations", replicates=TRUE)

grid.arrange(p3,p4,ncol=2)




