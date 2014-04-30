library(deSolve)
library(FME)

################################################################################
#                              deSolve                                         #
################################################################################

Rc = 0.3 # max growth rate
Cmax = 1.2 # Max number of cells

# Practice solving a differential equation with deSolve -- logistic growth
f1 = function( t, y, parms ) {
  ydot = vector(len=1)
  
  ydot[1] = Rc*y[1]*(1-y[1]/Cmax)
  
  return(list(ydot))
}

yini = 0.1
times=1:100
out = lsode( yini, times, f1, parms=0, jactype="fullint" )
plot(out)



################################################################################
#                                 FME                                          #
################################################################################

# A function for normal cell growth
solveCells = function(pars, times=seq(0,100,by=0.5)) {
  
  derivs = function(t,state,pars) {
    with(as.list(c(state,pars)), {
      if(Sub<0) Sub=0
      growthRate = (Umax*Sub/(Sub+Ks))*(1-B/Barea)
      deathRate = (1 - Sub/(Sub+Ks))*0.03
      dB = eff*growthRate*B - deathRate*B
      dSub = -growthRate*B - Renergy*B
      if(Sub<=0) dSub=0
      return(list(c(dB,dSub)))
    } )
  }
  
  state = c( B=0.1, Sub=110 )
  return( ode( y=state, times=times, func=derivs, parms=pars ) )
  
}
                      
# Umax -- maximum uptake rate of media
# Gmax -- maximum growth rate
# Sub -- Substrate or media left
# Ks -- Half saturation constant (the concentration supporting half the maximum
#             uptake rate. Limited concentration => Limited uptake )
# death -- the normal death rate of cells
pars = list( Umax=0.5, eff=0.8, Ks=20, Renergy=0.02, Barea=58 )
out = solveCells(pars)

Barea = 58
cells = out[,2]
att = cells*(1 - cells/(1.1*Barea))
out = cbind(out,att)

matplot( out[,1], out[,-1], type="l", lty=1:3, lwd=c(2,2,1),
         col="black", xlab="time, hour", ylab="Biomass")
legend("topright",c("Cells","Nutrients","TOC"), lty=1:3, lwd=c(2,2,1))


