# plot the columns of a data frame where all rows but one
# are y-values for a different data set. The "one" is the x values

require(ggplot2)
plot.columns = function( data, x.var ) {
  df.data = melt(data, id=x.var, variable_name="series")
  ggplot(df.data, aes(Time,value)) + geom_line(aes(colour=series))
}