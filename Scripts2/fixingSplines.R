library("DierckxSpline")


# practice with this data set to extend thi

source("./Scripts2/library.R")
wells = parse.RTCAanalyze( metadata="./MasterSheet.csv", data.dir="./Data" )
subset = retrieveWells(wells,file=c("HCT8.txt"),controls=TRUE,ID="toxinAdd",
                       compounds="HCT8", max.concentrations=6000)
subset = retrieveWells(wells,file=c("CHO-t2.txt"),controls=TRUE)
twell = transform(subset,c("slice","normalize","tcenter"),ID="toxinAdd",xlim=c(-5,10))
twell = transform(subset,c("normalize","tcenter"),ID="toxinAdd")

plot(twell,diagnostic=1)

# Input data
well = twell[[1]]
f = well$spline
x = timesdata(well)
y = welldata(well)

# get the weightings on the knots based off the derivative of the
# interpolating spline. High derivative = High weight.
xfine = insert.nbw(x,10)
foo = function(x) mean(abs(x))
yfine = f(xfine,deriv=0)
w = rolltime( xfine, f(xfine,deriv=1), 0.5, foo )
yscale = diff(range(yfine))
w2 = w/yscale
w3 = w2 + 0.1
n = length(xfine)

# Using Dierckx
sf = 0.5
cubic.fit = curfit( xfine, f(xfine), w=w3, s=sf/n )
kk = knots(cubic.fit)
par(mfrow=c(2,1))
plot(x,y,col="gray",xlim=c(-0.5,3))
lines(xfine,predict(cubic.fit,xfine),col="blue")
points(kk,f(kk),col="blue")
plot(x,y,col="gray")
lines(xfine,predict(cubic.fit,xfine),col="blue")
points(kk,f(kk),col="blue")


