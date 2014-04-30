# A library of functions for processing multi-well data
source("./Scripts2/library.R")
source("./Scripts2/latexLibrary.R")
wells = parse.RTCAanalyze( metadata="./MasterSheet2.csv", data.dir="./Data2" )


##########################################################################################
#                               Panel A                                                  #
##########################################################################################


subset = retrieveWells(wells, file = c("HCT8-4.txt"))
t.subset = normalize_toxin(subset, xlim = c(-2, Inf))

###### Panel A
conditions = groupWells(t.subset, group = "by.compounds") %in% c("gdTcdB", "")
p1 = plot(t.subset[conditions]) + ylim(0, 1.2)

###### Panel B
conditions = groupWells(t.subset, group = "by.concentrations") %in% 
                 c("", "gdTcdB-100", "TcdB-10", "gdTcdB-100.TcdB-10")
p2 = plot(t.subset[conditions], xlim = c(-0.2, 4)) + ylim(0, 1.2)

###### Panel C
conditions = groupWells(t.subset, group = "by.concentrations") %in% 
                 c("", "gdTcdB-1000", "TcdA-1000", "gdTcdB-1000.TcdA-1000", "TcdA-100", "gdTcdB-1000.TcdA-100")
p3 = plot(t.subset[conditions], xlim = c(-0.2, 10)) + ylim(0, 1.2)

grid.arrange(p1,p2,p3,ncol=2)
