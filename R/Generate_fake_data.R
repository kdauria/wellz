
.local = function() {
library(ggplot2)
library(reshape)

n = 16 # number of wells

# Parameters for logistic growth
doubling.time = rnorm(n, 10, 1)
params = data.frame(
              P = rnorm(n, 10000, 200), # initial population
              r = log(2)/doubling.time, # growth rate
              K = rnorm(n, 100000, 1000) ) # carrying capacity
t = seq(0,200,by=15/60) # 50 hours, every 15 minutes       

# Function to simulate logistic growth
log_growth = function( t, P, r, K ) {
  K*P*exp(r*t) / (K + P*(exp(r*t)-1))
}
exp_decay = function( t, P, r ) {
  newt = t - t[1]
  P*exp(r*newt)
}

# Make a data frame for the growth part of the experiment
data = data.frame( apply(params, 1, function(x) log_growth(t, x["P"], x["r"], x["K"])) )
colnames(data) = 1:ncol(data)

# Now add a perturbation that decreases the number of cells
# two conditions at three concentrations (in duplicate) = 2 * 3 * 2 = 12
# the rest of the wells continue logistic growth (don't modify these wells)
half.life = c( 1000, 1000, 100, 100, 10, 10,
               100,   100,  10,  10,  1,  1 )
r = -log(2)/half.life # rate of decay
num.cells = unlist(data[ t>50, ][1, 1:12]) # number of cells at 50 hours

# Lets say that the perurbation was added at t.perturb hours
t.perturb = c(rep(50,6),rep(55,6))
params = data.frame( P = num.cells, r = r )
for( i in 1:nrow(params) ) {
  new.t = t[t>t.perturb[i]]
  decay.data = exp_decay( new.t, num.cells[i], r[i])
  data[ t>t.perturb[i], i] = decay.data
}

############ Quick replot of the data
data.wtime = cbind(t,data)
mdata = melt( data.wtime, id.vars="t" )
colnames(mdata) = c("t","loc","value")
ggplot(mdata, aes(x=t, y=value, group=loc)) + geom_line()

############ Make wellList object with the data
comps = c(rep("A",6),rep("B",6),rep("C",4))
concs = c(half.life, rep(1,4))
wells = WellList(compound.names=comps, concentrations=concs, location=1:16)
wells = add_data_data.frame(wells, data.wtime)
plot(a2, color="compound")




}






















