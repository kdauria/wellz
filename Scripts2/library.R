library("reshape")
library("ggplot2")
library("manipulate")
library("plyr")
library("tools")
library("latticeExtra")
library("stringr")
library("RTCA")
library("scales")
library("gtable")
library("grid")
library("gridExtra")
library("DierckxSpline")
library("data.table")
library("zoo")
library("lokern")
library("Rcpp")
library("caTools")
library("xtable")
require("taRifx")
source("./Scripts2/Classes.R")
source("./Scripts2/Methods.R")
source("./Scripts2/Parsing.R")
source("./Scripts2/getset.R")
source("./Scripts2/wellsearch.R")
source("./Scripts2/plotting.R")
source("./Scripts2/general.R")
source("./Scripts2/transformations.R")
source("./Scripts2/splines.R")
source("./Scripts2/binCondense.R")
source("./Scripts2/smoothers.R")
source("./Scripts2/interpolate.R")
source("./Scripts2/fsmoothers.R")
source("./Scripts2/wellTables.R")
source("./Scripts2/analyses.R")


#############################################################################
#                   Functions to produce metrics from curves                #
#############################################################################


# Group the metrics by compound and plot versus concentration
groupMetric = function(x,...) UseMethod("groupMetric",x)
groupMetric.wellList = function( wells, ID, metric ) {
  ldply(lapply(wells,groupMetric,ID=ID,metric=metric))
}
groupMetric.Well = function(well,ID,metric) {
  ss = getsolutions(well,ID)
  cc = getcompounds(ss)
  cc2 = cc[ cc$type == "start", ]
  
  if(nrow(cc2)>1) {
    stop("Code can't yet handle 2+ compounds per well")
  } else if(nrow(cc2)==0) {
    group = "none"
    xval = 0
  } else {
    group = cc2$name
    xval = cc2$conc
  }
  yval = well$metrics[[metric]]
  data.frame( file=getfiles(well), group=group, xval=xval, yval )
}

# plot the output from groupMetric
plotMetric = function( met, file=TRUE ) {
  tp = met[ met$group != "none", ]
  fl = met[ met$group == "none", ]
  if(file) {
    mapping = aes(x=xval,y=value,color=interaction(group,file))
  } else {
    mapping = aes(x=xval,y=value,color=group)
  }
  p = ggplot(data=tp,mapping) + geom_point(size=2) +
    scale_x_log10() + 
    geom_hline(data=fl,aes(yintercept=value,color=group),linetype=2) +
    scale_color_discrete(name="Legend")
  return(p)
}

# return the metrics for all of the wells as a dataframe
getMetric = function(x,metric) UseMethod("getMetric",x)
getMetric.wellList = function(wells, metric) {
  ldply(wells,getMetric,metric)
}
getMetric.Well = function(well, metric) {
  mets = well$metrics[[metric]]
  loc = getlocations(well)
  file = getfiles(well)
  data.frame(file=file,well=loc,mets)
}



# Overlay a metric onto the data
addMetricLayer = function(p, wells, metric) {
  
  # metrics for all of the wells
  smoother = p$wellOptions$smoother
  mets = getMetric( wells, metric )
  
  # make the data frame to be plotted
  plotvals = p$data[ !duplicated(p$data[,c("file","well")]), ]
  xtras = setdiff(colnames(plotvals),c("file","well","time","sweep","value","sds"))
  rownames(plotvals) = paste(plotvals$file,plotvals$well,sep="::")
  xtra.cols = plotvals[ paste( mets$file, mets$well, sep="::" ), xtras,drop=FALSE ]
  df = data.frame( mets, xtra.cols )
  rownames(df) = NULL
  
  # given the times, get the values of the metrics
  deriv = p$wellOptions$deriv
  wids = well.index( wells, df$well, df$file )
  for( i in seq_along(wids)) {
    if(smoother)
      df$value[i] = wells[[i]]$smoother( df$time[i], deriv=deriv )
    else
      df$value[i] = wells[[i]]$spline( df$time[i], deriv=deriv )
  }
  
  # add the point layer
  p = p + geom_point(data=df, aes(x=time,y=value), size=3) +
    geom_point(data=df, aes(x=time,y=value), size=1.5, color="white")
  
  # add the aesthetics to the plot
  for( nm in xtras ) 
    p$layers[[ length(p$layers)-1 ]]$mapping[[nm]] = as.symbol(nm)
  
  return(p)
}




