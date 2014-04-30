# The normal data processing for cells treated with toxin
normalize_toxin = function(x,xlim=c(-1,5)) {
  transform(x, c("tcenter","slice","normalize"), ID="toxinAdd", xlim)
}

# Set default parameters for a composite smoothing algorithm
smoother_toxin = function(x, x.scale=40/60, y.scale=1, noise.scale=2/60,
                          deriv.cutoffs=c(0.05,0.35), nbw=10,min.diff=10/60/60) {
  add_smoother(x, method="composite", x.scale=x.scale, y.scale=y.scale,
               noise.scale=noise.scale,nbw=nbw,min.diff=min.diff,deriv.cutoffs=deriv.cutoffs )
}

# Changing the defaults of some other functions
oldplot = plot.wellList
plot.wellList = function(x,color="by.concentrations",ID="toxinAdd",replicates=TRUE,...) {
  oldplot(x,color=color,ID=ID,replicates=replicates,...)
}

oldretrieveWells = retrieveWells
retrieveWells = function(x,controls=TRUE,...) {
  if( "file" %in% names(list(...)) ) controls=TRUE
  oldretrieveWells(x,controls=controls,...)
}
no_x_labels = scale_x_discrete(labels=function(x) rep("",length(x)))

oldintegrate = integrate.wellList
integrate.wellList = function( x, ... ) {
  oldintegrate(x,...,baseline="controls")
}