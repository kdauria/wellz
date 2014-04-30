# Take the derivatives of all columns with respect to the x.var column

dif.columns = function( data, x.var, df ) {
  x = data[,x.var]
  datamat = as.matrix(data[,2:ncol(data)])
  foo = function(y) dif(x,y,df=df)
  datap = as.data.frame(cbind(x,apply(datamat,2,foo)))
  colnames(datap) = colnames(data)
  return(datap)
}