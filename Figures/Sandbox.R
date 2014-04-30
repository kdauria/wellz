source("./Scripts2/library.R")
wells = parse.RTCAanalyze(metadata = "./MasterSheet.csv", data.dir = "./Data")



############## J774 cells ##################
subset = retrieveWells(wells,file=c("J774-t7.txt"))

t.subset = transform(subset,c("tcenter","slice","normalize"),ID="toxinAdd",xlim=c(-Inf,100))
plot(t.subset,xlim=c(-0.2,80), color="by.concentrations",replicates=TRUE)


##############   PMNs   ##################
subset = retrieveWells(wells,file=c("PMN-t4.txt"))
t.subset = transform(subset,c("tcenter","slice","level"),ID="toxinAdd",xlim=c(-Inf,100))

#p = plot(t.subset,xlim=c(-1,1),color="by.concentrations", replicates=TRUE)



il8 = groupWells(t.subset,group="by.compounds",ID="cellSeed")
p1 = plot(t.subset[il8=="IL8"][-5],xlim=c(-1,22),color="by.concentrations", replicates=TRUE, se=TRUE)
p2 = plot(t.subset[il8==""],xlim=c(-1,60),color="by.concentrations", replicates=TRUE, se=TRUE)

grid.arrange(p1,p2,ncol=2)

grid.arrange(p1+ylim(-0.65,0.2),p2+ylim(-0.65,0.2),ncol=2)

##############   PMNs   ##################

subset = retrieveWells(wells,file=c("PMN-t5.txt"))
t.subset = transform(subset,c("tcenter","slice","level"),ID="toxinAdd",xlim=c(-Inf,100))

il8 = groupWells(t.subset,group="by.compounds",ID="cellSeed")
p1 = plot(t.subset[il8=="IL8"],xlim=c(-1,50),color="by.concentrations", replicates=FALSE, se=TRUE)
p2 = plot(t.subset[il8==""],xlim=c(-1,50),color="by.concentrations", replicates=FALSE, se=TRUE)

grid.arrange(p1+ylim(-0.1,0.2),p2+ylim(-0.1,0.2),ncol=2)


########## J774 cells #####################

subset = retrieveWells(wells,file=c("J774-t8.txt"))

t.subset = transform(subset,c("tcenter","slice","normalize"),ID="toxinAdd",xlim=c(-Inf,100))
plot(t.subset,xlim=c(-0.2,18), color="by.concentrations", replicates=TRUE,se=TRUE)


####### HCT8 cells #######################

subset = retrieveWells(wells,file=c("HCT8-t7.txt"))
t.subset = transform(subset,c("tcenter","slice","normalize"),ID="toxinAdd",xlim=c(-Inf,100))
t.subset = t.subset[-(12:13)] # It looks like something went wrong in two wells
plot(t.subset, xlim=c(-0.2,8), color="by.concentrations", replicates=TRUE, se=TRUE)

# Toxin B only
sub.b = retrieveWells(t.subset,compounds=c("gdTcdB","TcdB"), controls=TRUE)
plot(sub.b, xlim=c(-0.2,10), color="by.concentrations", replicates=TRUE, se=TRUE)

# Toxin A only
sub.a = retrieveWells(t.subset,compounds="TcdA",controls=TRUE)
plot(sub.a, xlim=c(-0.2,11), color="by.concentrations", replicates=TRUE, se=TRUE)

# All TcdAs
sub.a = retrieveWells(t.subset,compounds=c("TcdA","TcdAup","TcdAother"),controls=TRUE)
plot(sub.a, xlim=c(-0.2,11), color="by.concentrations", replicates=TRUE, se=TRUE)



plot(subset,replicates=FALSE,xlim=c(36.15,36.3),diagnostic=25,showpoints=TRUE)
plot(t.subset,replicates=TRUE) + theme(legend.position="none")

####### HCT8 cells #######################

subset = retrieveWells(wells,file=c("HCT8-t8.txt"))
t.subset = transform(subset,c("tcenter","slice","normalize"),ID="toxinAdd",xlim=c(-Inf,100))
plot(t.subset,xlim=c(-1,24), color="by.concentrations", replicates=FALSE)

subset.a = retrieveWells(t.subset, compounds="TcdA", controls=TRUE)
plot(subset.a,xlim=c(-0.2,10), color="by.concentrations", replicates=FALSE)

subset.b = retrieveWells(t.subset, compounds="TcdB", controls=TRUE)
plot(subset.b,xlim=c(-0.2,10), color="by.concentrations", replicates=FALSE)

ii = groupWells(t.subset,group="by.compounds") %in% c("gdTcdB","")
plot(t.subset[ii],xlim=c(-0.2,48), color="by.concentrations", replicates=TRUE, se=TRUE) + ylim(0.2,1.1)




############ Old HCT8 cells ########

subset = retrieveWells(wells,file=c("HCT8-t3.txt","HCT8-t4.txt"))


t.subset = transform(subset,c("tcenter","slice"),ID="toxinAdd",xlim=c(-Inf,100))
p1 = plot(t.subset,color="by.concentrations",xlim=c(-25,15))

t.subset = transform(subset,c("tcenter","slice","normalize"),ID="toxinAdd",xlim=c(-Inf,100))
p2 = plot(t.subset,color="by.concentrations",xlim=c(-25,15))


p2 = plot(t.subset,replicates=TRUE)
subsetA = retrieveWells(t.subset,compounds="TcdA")
subsetB = retrieveWells(t.subset,compounds="TcdB")
p3 = plot(subsetA,replicates=TRUE,xlim=c(-1,10))
p4 = plot(subsetB,replicates=TRUE,xlim=c(-0.01,5))




