################## CHO.txt
subset = retrieveWells(wells,file=c("CHO.txt"))
twell = transform(subset,c("tcenter","slice"),"toxinAdd",xlim=c(-5,49))
plot(twell,manip=TRUE,diagnostic=16,color="by.concentrations",
     linetype="by.compounds",showpoints=FALSE)

################## CHO-t2.txt
subset = retrieveWells(wells,file=c("CHO-t2.txt"))
twell = transform(subset,c("slice","normalize"),ID="toxinAdd",xlim=c(-.5,2))
plot(twell,manip=TRUE,color="by.concentrations",diagnostic=16)

################## HCT8.txt
subset = retrieveWells(wells,file=c("HCT8.txt"),controls=TRUE,ID="toxinAdd",
                       compounds=c("HCT8"),
                       max.concentrations=c(6000))
twell = transform(subset,c("slice","tcenter","normalize"),ID="toxinAdd",xlim=c(-1,10))
plot(twell,manip=TRUE,color="by.concentrations",
     diagnostic=4,showpoints=TRUE,ID="toxinAdd",replicates=TRUE)

################## HCT8-t2.txt
subset = retrieveWells(wells,file=c("HCT8-t2.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),ID="toxinAdd",xlim=c(-0.1,48))
#twell[[4]] = NULL
plot(twell,manip=TRUE,showpoints=FALSE,diagnostic=12)

################## HCT8-t3.txt
subset = retrieveWells(wells,file=c("HCT8-t3.txt"))
twell = transform(subset,c("slice","tcenter","normalize"),xlim=c(-5,48),ID="toxinAdd")
plot(twell[c(1:3,9:11)])

################## HCT8-t4.txt
subset = retrieveWells(wells,file=c("HCT8-t4.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),ID="toxinAdd",xlim=c(-5,48))
plot(twell[c(1,9)],xlim=c(-2,20),diagnostic=1)

################# HCT8-t5.txt
subset = retrieveWells(wells,file=c("HCT8-t5.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-5,13),ID="toxinAdd")
plot(twell[c(10,11,2)],smoother=FALSE)

################# HCT8-t6.txt
subset = retrieveWells(wells,file=c("HCT8-t6.txt"))
twell = transform(subset,c("tcenter","normalize","slice"),xlim=c(-1,10),ID="toxinAdd")
plot(twell[9],smoother=FALSE)

################# HUVEC-r1.txt
subset = retrieveWells(wells,file=c("HUVEC-r1.txt"))
twell = transform(subset,c("tcenter","normalize","slice"),xlim=c(-1,48),ID="toxinAdd")
plot(twell, diagnostic=1)

################# HUVEC-r2.txt
subset = retrieveWells(wells,file=c("HUVEC-r2.txt"))
twell = transform(subset,c("tcenter","normalize","slice"),xlim=c(-1,48),ID="toxinAdd")
plot(twell)

################# J774-R1.txt
subset = retrieveWells(wells,file=c("J774-R1.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-1,50),ID="toxinAdd")
plot(twell)

################# J774-R2.txt
subset = retrieveWells(wells,file=c("J774-R2.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-1,500),ID="toxinAdd")
plot(twell, diagnostic=1)

################ J774-t2.txt
subset = retrieveWells(wells,file=c("J774-t2.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-1,500),ID="toxinAdd")
plot(twell)

################ J774-t3.txt
subset = retrieveWells(wells,file=c("J774-t3.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-1,30),ID="toxinAdd")
plot(twell,replicates=TRUE,color="by.compounds",showpoints=FALSE,linetype="by.concentrations")

################ J774-t4.txt
subset = retrieveWells(wells,file=c("J774-t4.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-1,5),ID="toxinAdd")
plot(twell,replicates=TRUE,color="by.compounds",showpoints=FALSE,linetype="by.concentrations")


################ PMN-R1.txt
subset = retrieveWells(wells,file=c("PMN-r1.txt"),compounds="TcdA",controls=TRUE)
twell = transform(subset,c("tcenter","slice","level"),xlim=c(-1,16),ID="toxinAdd")
twell = add_smoother( twell, method="composite", x.scale=40/60, y.scale=1, 
                      noise.scale=10/60, deriv.cutoffs=c(0.05,0.5), 
                      nbw=10, min.diff=10/60/60)
p1 = plot(twell,showpoints=FALSE,smoother=TRUE,color="by.concentrations") # TcdB
p2 = plot(twell,showpoints=FALSE,smoother=TRUE,color="by.concentrations") # TcdA

################ PMN-R2.txt
subset = retrieveWells(wells,file=c("PMN-r2.txt"))
twell = transform(subset,c("tcenter","slice","level"),xlim=c(-1,16),ID="toxinAdd")
twell = add_smoother( twell, method="composite", x.scale=40/60, y.scale=1,
                      noise.scale=10/60, deriv.cutoffs=c(0.05,0.5),
                      nbw=10, min.diff=10/60/60)
plot(twell,showpoints=FALSE,smoother=TRUE) + facet_wrap(~well,ncol=2)

################ PMN-t2.txt
subset = retrieveWells(wells,file=c("PMN-t2.txt"))
twell = transform(subset,c("tcenter","slice","level"),xlim=c(-5,16),ID="toxinAdd")
plot(twell, diagnostic=1)

################ PMN-t3.txt
subset = retrieveWells(wells,file=c("PMN-t3.txt"))
twell = transform(subset,c("tcenter","slice","level"),xlim=c(-5,40),ID="toxinAdd")
plot(twell, diagnostic=1)

################ T84-TcdA.txt
subset = retrieveWells(wells,file=c("T84-TcdA.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-1,300),ID="toxinAdd")
plot(twell,diagnostic=1)

################ T84-TcdB.txt
subset = retrieveWells(wells,file=c("T84-TcdB.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-1,26),ID="toxinAdd")
plot(twell,diagnostic=6)

################ CecalCells.txt
subset = retrieveWells(wells,file=c("CecalCells.txt"))
twell = transform(subset,c("tcenter","slice","normalize"),xlim=c(-1,10),ID="toxinAdd")
plot(twell,color="by.concentrations",replicates=TRUE,showpoints=FALSE,linetype="by.compounds")


