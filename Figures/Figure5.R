# A library of functions for processing multi-well data
source("./Scripts2/library.R")
source("./Scripts2/latexLibrary.R")
wells = parse.RTCAanalyze( metadata="./MasterSheet2.csv", data.dir="./Data2" )


##### Panel A
subset = retrieveWells(wells, file = "J774-4.txt")
t.subset = normalize_toxin(subset, xlim = c(-2, Inf))
conditions = groupWells(t.subset, group = "by.compounds") %in% 
                 c("", "TcdB", "gdTcdB")
p1 = plot(t.subset[conditions], xlim = c(-1, 48))

##### Panel B
subset = retrieveWells(wells, file = c("J774-3a.txt", "J774-3b.txt"))
t.subset = normalize_toxin(subset, xlim = c(-2, Inf))
6
conditions = groupWells(t.subset, group = "by.concentrations") %in% 
                 c("", "TcdB-0.01", "gdTcdB-1.TcdB-0.01")
p2 = plot(t.subset[conditions], xlim = c(-0.2, 5))

##### Panel C
subset = retrieveWells(wells, file = c("J774-5.txt"))
t.subset = normalize_toxin(subset, xlim = c(-2, Inf))
conditions = groupWells(t.subset, group = "by.concentrations") %in% 
                  c("", "gdTcdB-10", "TcdA-1", "gdTcdB-10.TcdA-1")
p3 = plot(t.subset[conditions], xlim = c(-0.2, 24))

grid.arrange(p1, p2, p3, ncol = 2)






