########################### Other functions ####################################

# area under the curve
all.aoc = function( data, x.var, upper=20 ) {
  x = data[,x.var]
  foo.aoc = function( y ) aoc(x,y,20)
  aocs = apply(data,2,foo.aoc)
  aocs = aocs[names(aocs)!=x.var]
  return(aocs)
}


aoc = function( x, y, upper=20 ) {
  df=length(x)
  fs = smooth.spline(x,y,df=df)
  f = splinefun(fs$x,fs$y)
  return( integrate(f,0,upper)$value )
}



plot.curve.with.rates = function( data, x.var, df.cutoff, df.precise ) {
  
  # get points on the curve where the rates happen
  rates = as.data.frame(max.rates( data, x.var, df.cutoff, df.precise ))
  rates.x = data[rates$times,x.var]
  rate.matrix = data[rates$times,colnames(data)!=x.var]
  rates.y = diag(as.matrix(rate.matrix))
  curv.rates = data.frame(times=rates.x,rates=rates.y)
  
  df.data = melt(data, id=x.var, variable_name="series")
  plot1 = ggplot(df.data, aes(Time,value)) + geom_line(aes(colour=series))
  plot1 + geom_point(data=curv.rates,aes(x=times,y=rates))
  
}


plot.deriv = function( data, x.var, df ) {
  data.deriv = dif.columns(data,x.var,df)
  plot.columns(data.deriv,x.var)
}

max.rates = function( data, x.var, df.cutoff=15, df.precise=50, by.time=F ) {
  
  cutoffs = find.curve.bottom(data,x.var,df.cutoff)
  data.deriv = dif.columns(data,x.var,df.precise)
  plot.columns(data.deriv,x.var)
  y.data.deriv = data.deriv[,colnames(data.deriv)!=x.var]
  rates = apply(y.data.deriv,2,min)
  times = apply(y.data.deriv,2,which.min)
  if(by.time)
    times = data[times,x.var]
  
  return(cbind(times,rates))
}

plot.cut.curve = function( data, x.var, df=15 ) {
  
  cutoffs = find.curve.bottom( data, x.var, df )
  df.data = listMeld(data,cutoffs)
  ggplot(df.data, aes(Time,value)) + geom_line(aes(colour=series))
  
}

# cut off the cytotoxicity curves when they level off
find.curve.bottom = function( data, x.var, df ) {
  
  # figure out where to stop the curve
  data.deriv = dif.columns( data, x.var, df )
  data.columns = colnames(data.deriv) != x.var
  y.data.deriv = as.matrix( data.deriv[ , data.columns] )
  
  data.cut = apply(y.data.deriv,2,truncateSurvival)
  cutoffs = colSums( !is.na( data.cut ) )
  return(cutoffs)
  
}

# make a list of vectors from the groupSizes variable
makeList = function( data, groupSizes ) {
  df = listMeld(data,groupSizes)
  return(split(df[,3],df[,2]))
}

# Now meld the data given the group sizes
listMeld = function( data, groupSizes ) {
  out = list()
  values = data[,2:ncol(data)]
  xx = data[,1]
  for(i in 1:length(groupSizes)) {
    yy = values[,i]
    groupSize = groupSizes[i]
    out[[i]] = list( x=xx[1:groupSize], y=yy[1:groupSize]   )
  }
  data.cut.x = sapply(out,function(xx) xx$x)
  data.cut.y = sapply(out,function(xx) xx$y)
  groupNames = rep(names(groupSizes),times=groupSizes)
  df = as.data.frame(matrix(NA,length(groupNames),3))
  df[,2] = groupNames
  df[,3] = unlist(data.cut.y)
  df[,1] = unlist(data.cut.x)
  colnames(df) = c("Time","series","value")
  return(df)
}
