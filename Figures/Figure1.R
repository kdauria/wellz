source("./Scripts2/library.R")
source("./Scripts2/latexLibrary.R")
wells = parse.RTCAanalyze( metadata="./MasterSheet.csv", data.dir="./Data" )

##########################################################################################
#                               Panel A                                                  #
##########################################################################################

# Process data from file HCT8-4.txt
subset = retrieveWells(wells,file="HCT8-4.txt")
t.subset = normalize_toxin( subset, xlim=c(-Inf, 100) )
  
# Select controls and TcdA 1000 ng/ml conditions
conditions = groupWells(t.subset,group="by.concentrations") %in% c("","TcdA-1000")
f.subset = t.subset[conditions]

# Make and organize the two graphs
p1 = plot(f.subset, xlim=c(-43,10) )
p2 = plot(f.subset, xlim=c(-1,10) )
grid.arrange( p1, p2, ncol=2 )

