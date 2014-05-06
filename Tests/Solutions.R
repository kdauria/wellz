# Test 1
s1 = list(solvent=data.frame(name=c("PBS","media"),perc=c(25,75),stringsAsFactors=FALSE),
          compounds=data.frame(name=c("A","B"),conc=c(10,12),type=c("start","start"),stringsAsFactors=FALSE),
          volume=10)
s2 = list(solvent=data.frame(name="media",perc=100,stringsAsFactors=FALSE),
          compounds=data.frame(name="A",conc=10,type="start",stringsAsFactors=FALSE),
          volume=2.5)
class(s1) = class(s2) = c("Solution","list")

# Test 2
s1 = list(solvent=data.frame(name=c("PBS","media"),perc=c(25,75),stringsAsFactors=FALSE),
          compounds=na.omit(data.frame(name=NA,conc=NA,type=NA,stringsAsFactors=FALSE)),
          volume=10)
s2 = list(solvent=data.frame(name="media",perc=100,stringsAsFactors=FALSE),
          compounds=data.frame(name="A",conc=100,type="total",stringsAsFactors=FALSE),
          volume=2.5)
class(s1) = class(s2) = c("Solution","list")


# Test 3
s1 = list(solvent=data.frame(name="media",perc=100,stringsAsFactors=FALSE),
          compounds=na.omit(data.frame(name="HCT8",conc=60000,type="total",stringsAsFactors=FALSE)),
          volume=200)
s2 = list(solvent=data.frame(name="media",perc=100,stringsAsFactors=FALSE),
          compounds=data.frame(name=c("TcdB","gdTcdB"),conc=c(1,100),type=c("final","final"),stringsAsFactors=FALSE),
          volume=22.2)
class(s1) = class(s2) = c("Solution","list")

Rprof(interval=0.001)
for(i in 1:500) s1+s2
Rprof(NULL)
summaryRprof()







