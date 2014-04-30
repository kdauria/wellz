# A library of functions for processing multi-well data
source("./Scripts2/library.R")
source("./Scripts2/latexLibrary.R")
wells = parse.RTCAanalyze( metadata="./MasterSheet2.csv", data.dir="./Data2" )


#### Since this figure includes data from multiple experiments and files,
#### they must all be processed first

# Only select wells seeded with at most 6,000 HCT8 cells
hct8 = retrieveWells(wells,file="HCT8.txt", compounds="HCT8", max.concentrations=6000)

# The rest of the wells come from these files
files = list( "CHO.txt", "IMCE.txt", c("HUVEC-a.txt","HUVEC-b.txt"), c("T84-a.txt","T84-b.txt"))
subsets = lapply( files, function(f) retrieveWells(wells,file=f) )
subsets = c( list(hct8), subsets )

# Normalize each subset
xlims = list( c(-0.1, 43), c(-1, Inf), c(-1, Inf), c(-1,60), c(-1,27) )
n.subsets = mapply( normalize_toxin, subsets, xlim=xlims )

# Add a smoother to each subset
x.scales = c( 2/3, 1, 1, 2/3, 2/3 )
s.subsets = mapply( smoother_toxin, n.subsets, x.scale=x.scales )

# Calculate ABC for each subset (with different integration limits)
left = rep(0,5)
right = c(43,40,80,80,27)
i.subsets = mapply( integrate, s.subsets, left, right)
allwells = do.call( c, i.subsets )
  
# Calculate MaxS for each well
allwells = max.rate( allwells, ID="toxinAdd", min.diff=10/60/60, ylim=0.8, xlim=2 )



######### Panel A. Different cell types. Same concentrations. #########
subset = retrieveWells( allwells, compounds="TcdA", ID="toxinAdd", 
                        max.concentrations=101, min.concentrations=99 )
panelA = plot(subset, xlim=c(-1,10), se=FALSE, color="by.total.compounds", linetype="by.compounds")

######### Panel B. Same cell type. Different toxins ############
subset = retrieveWells( allwells, compounds="IMCE" )
subset2 = retrieveWells( subset, compounds=c("TcdA","TcdB"), ID="toxinAdd", 
                        max.concentrations=c(101,101), min.concentrations=c(99,99) )
panelB = plot(subset2,xlim=c(-0.1,2), se=FALSE )

######## Panel C. MaxS and ABC for IMCE cells ###########
subset = retrieveWells( allwells, compounds="IMCE" )
MaxS = groupMetric( subset, ID="toxinAdd", metric="max.rate")
p1 = plotMetric(MaxS)
ABC = groupMetric( subset, ID="toxinAdd", metric="integral")
p2 = plotMetric(ABC)
panelC = arrangeGrob(p1 + theme(legend.position="none"),
                     p2 + theme(legend.position="none"),ncol=2)

####### Panel D. The MCC for each cell type ##############
# The MCC was found by plotting the ABC of each cell type over
# a range of concentrations. The concentration diverging from
# controls was considered the MCC
ABC = groupMetric( allwells, ID="toxinAdd", metric="integral")
ABC$cells = groupWells(allwells,group="by.total.compounds")
plotMetric(ABC,file=FALSE) + facet_wrap(~cells)

# Manually enter the MCC for each cell type
d = data.frame( type = c( "CHO", "HCT8", "HUVEC", "IMCE", "T84" ),
                a    = c( 1    ,  1    ,  1     ,  0.1  , 0.1   ),
                b    = c( .001 ,  .01  ,  .01  ,  .0001 , 0.1   ) )
panelD = ggplot( d, aes( x=log10(a), y=log10(b), label=type) ) + geom_text() +
  scale_x_reverse(limits=c(0,-4)) + scale_y_reverse(limits=c(0,-4)) + 
  geom_abline(slope=1,intercept=0) + coord_equal() + geom_point(color="red")

# Show the final plot
grid.arrange( panelA, panelB, panelC, panelD, ncol=2 )

